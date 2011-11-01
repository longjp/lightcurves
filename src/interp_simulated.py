#####
##### SIMULATION FOR SYNTHETIC DATA WITH NOISIFICATION BUT NO
##### SMOOTHING USING A FAKE CADENCE
#####
#####

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

# make and test connection to the database
features_file = "../db/derived_features_list.txt"
connection = sqlite3.connect('../db/simulated_astronomy.db')
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
aSurvey.generateCurve()
tfe = np.column_stack((aSurvey.times[:,np.newaxis],
		       aSurvey.fluxes[:,np.newaxis],
		       aSurvey.errors[:,np.newaxis]))
visualize.plot_curve(tfe,period=aSurvey.period_this)





# generate training data, add to db
survey='train'
for i in range(500):
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

survey='test'
for i in range(500):
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




########
######## MAKE NOISIFIED ENTRIES IN SOURCES
########

source_pragma = create_database.get_pragma(cursor,table='sources')
n_points = [10,20,30,40,50,60,70,80,90,100]

### make training entries
n_versions_first = 5
n_versions_random = 1
sql_cmd = """SELECT source_id FROM sources WHERE source_id = original_source_id AND survey = 'train'"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
len(db_info)
db_info = tolist(db_info)
for i in db_info:
   create_database.noisify_unsmoothed_sources(cursor,i,source_pragma,n_versions_first=n_versions_first,n_versions_random=n_versions_random)



### make test entries
n_versions_first = 1
n_versions_random = 0
sql_cmd = """SELECT source_id FROM sources WHERE source_id = original_source_id AND survey = 'test'"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
len(db_info)
db_info = tolist(db_info)
for i in db_info:
   create_database.noisify_unsmoothed_sources(cursor,i,source_pragma,n_versions_first=n_versions_first,n_versions_random=n_versions_random)




#### check to see if this went okay
## check 1
sql_cmd = """SELECT * FROM sources WHERE original_source_id != source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
len(db_info)

## check 2
sql_cmd = """SELECT * FROM sources WHERE original_source_id != source_id LIMIT 100"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
len(db_info)
for i in db_info:
	print i



#############
############# DERIVE FEATURES
#############
# retreive everything
sql_cmd = """SELECT source_id FROM sources LIMIT 100"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
noise_dict = noisification.get_noisification_dict()
derive_features.derive_features_par(source_ids,noise_dict,cursor,connection,number_processors=2,delete_existing=True)

# take a look at the features
sql_cmd = """SELECT * FROM features_short"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
	print i







#############
############# OUTPUT R FILES FOR ANALYSIS
#############

# output all sources to R file for analysis
sql_cmd = """SELECT source_id FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.outputRfile(source_ids,cursor,'../data_processed/simulated_data.dat')

# output tfes
sql_cmd = """SELECT source_id FROM sources WHERE original_source_id = source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.tfeOutput(source_ids,cursor,'../data_processed/simulated_data_tfe.dat')


