########## 
########## 
########## CODE TO GENERATE FEATURES FOR SHARMODEEP'S 
########## OUTLIER DETECTION METHOD 
##########
########## by James Long 
########## date: 10/29/2011 
########## 


import synthetic_data
import visualize
import numpy as np
import create_database
import sqlite3
import derive_features
import noisification
import db_output
import math
import scipy.stats

reload(db_output)
reload(noisification)
reload(derive_features)
reload(create_database)
reload(synthetic_data)
reload(visualize)
reload(np)



# for getting db_info in nice format
def tolist(db_info):
 list1 = []
 for i in db_info:
  list1.append(i[0])
 return(list1)


####
#### this code loaded the ASAS data so we could access the time series
#### takes a while (~ 10 hours) so only run once
####
# # make and test connection to the database
# features_file = "../db/derived_features_list.txt"
# connection = sqlite3.connect('../db/asas_full_cadences.db')
# cursor = connection.cursor()
# create_database.create_db(cursor,features_file=features_file,REMOVE_RECORDS=True)


# folder = "../data/asas_full"
# connection.commit()
# create_database.ingest_many_xml(folder,cursor,connection,
#                                 survey="asas",
#                                 number_processors=2)
# connection.commit()
# connection.close()





##################
##################
################## make synthetic_train, test, and hipparcos_train light curves
##################


# make and test connection to the database
features_file = "../db/derived_features_list_oct312011.txt"
connection = sqlite3.connect('../db/sharmo_outliers.db')
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,REMOVE_RECORDS=True)

# make a nice view of the features table
sql_cmd = """CREATE VIEW IF NOT EXISTS features_short AS SELECT source_id,freq1_harmonics_freq_0,std,max,weighted_average FROM features"""
cursor.execute(sql_cmd)

# make a nice view of the features table
sql_cmd = """CREATE VIEW IF NOT EXISTS sources_short AS SELECT source_id,original_source_id,classification,noisification,noise_args,true_period FROM sources"""
cursor.execute(sql_cmd)



# set up the survey and
# create two curves and visualize them
aSurvey = synthetic_data.surveySetup()
aCadence = synthetic_data.CadenceFromVOSource()
aSurvey.aCadence = aCadence
aSurvey.generateCurve()
tfe = np.column_stack((aSurvey.times[:,np.newaxis],
		       aSurvey.fluxes[:,np.newaxis],
		       aSurvey.errors[:,np.newaxis]))
visualize.plot_curve(tfe,period= (2*aSurvey.period_this))




# generate training data, add to db
survey='normal'
for i in range(50):
	aSurvey.generateCurve()
	tfe = np.column_stack((aSurvey.times[:,np.newaxis],aSurvey.fluxes[:,np.newaxis],aSurvey.errors[:,np.newaxis]))
	points_per_curve = len(aSurvey.times)
	source_class = aSurvey.class_name
	period = aSurvey.period_this
	curve_info = [points_per_curve,source_class,0,0,0,0,None,survey,0,period]
	curve_info_names = ["number_points","classification","c1","e1","c2","e2","raw_xml","survey","xml_filename","true_period"]
	print source_class
	print curve_info
	create_database.enter_record(curve_info,curve_info_names,tfe,cursor)


## derive features for sources
## retreive everything
sql_cmd = """SELECT source_id FROM sources"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
noise_dict = noisification.get_noisification_dict()
derive_features.derive_features_par(source_ids,noise_dict,cursor,connection,number_processors=2,delete_existing=True)


#######
####### OUTPUT FOR ANALYSIS IN R
#######

## output all sources to R file for analysis
sql_cmd = """SELECT source_id FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.outputRfile(source_ids,cursor,'../data_processed/sharmo_outliers.dat')

## output tfes
sql_cmd = """SELECT source_id FROM sources WHERE original_source_id = source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.tfeOutput(source_ids,cursor,'../data_processed/sharmo_outliers_tfe.dat')


connection.commit()

