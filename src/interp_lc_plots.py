########## 
########## 
########## DERIVE FEATURES SO CAN MAKE PLOTS OF 
########## HIPPARCOS AND OGLE LIGHTCURVES 
##########
########## by James Long 
########## date: 2/5/13 
########## 


import noisification
import db_output
import derive_features
import synthetic_data
import visualize
import create_database
import visualize
import numpy as np
import sqlite3
import scipy.stats
import math
import smoothers

#reload(db_output)
#reload(noisification)
#reload(derive_features)
#reload(create_database)
#reload(synthetic_data)
#reload(visualize)
#reload(np)



## put results in a list
def tolist(db_info):
 list1 = []
 for i in db_info:
  list1.append(i[0])
 return(list1)




## make and test connection to the database
features_file = "../db/derived_features_list.txt"
connection = sqlite3.connect('../db/hip_three_class.db')
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,REMOVE_RECORDS=True)

## ingest all dubath hipparcos sources
folder = "../data/hipparcos_dubath"
connection.commit()
create_database.ingest_many_xml(folder,cursor,connection,
                                survey="hipparcos",
                                number_processors=2)


## getting ogle sources
folder = "../data/OGLEIII/mira"
create_database.ingest_many_tfes(folder,
                                 ".dat",
                                 cursor,
                                 connection,
                                 survey="ogle",
                                 classification="Mira",
                                 max_lightcurves=500)
connection.commit()
print "obtained miras . . ."


folder = "../data/OGLEIII/classical-cepheid-fundamental"
create_database.ingest_many_tfes(folder,
                                 ".dat",
                                 cursor,
                                 connection,
                                 survey="ogle",
                                 classification="Classical Cepheid",
                                 max_lightcurves=500)
connection.commit()
print "obtained classical cepheids . . ."

folder = "../data/OGLEIII/rr-ab"
create_database.ingest_many_tfes(folder,
                                 ".dat",
                                 cursor,
                                 connection,
                                 survey="ogle",
                                 classification="RR Lyrae AB",
                                 max_lightcurves=500)
connection.commit()
print "obtained rr lyraes ab . . ."





## make a nice view of the features table
sql_cmd = """CREATE VIEW IF NOT EXISTS features_short AS SELECT source_id,freq1_harmonics_freq_0,std,max,weighted_average FROM features"""
cursor.execute(sql_cmd)

## make a nice view of the features table
sql_cmd = """CREATE VIEW IF NOT EXISTS sources_short AS SELECT source_id,original_source_id,classification,noisification,noise_args,true_period FROM sources"""
cursor.execute(sql_cmd)






sql_cmd = """UPDATE features SET n_points=(SELECT number_points FROM sources WHERE features.source_id = sources.source_id)"""
cursor.execute(sql_cmd)
connection.commit()







sql_cmd = """SELECT source_id FROM sources"""        
cursor.execute(sql_cmd)
connection.commit()
db_info = cursor.fetchall()
source_ids = tolist(db_info)
noise_dict = noisification.get_noisification_dict()
derive_features.derive_features_par(source_ids,noise_dict,cursor,connection,number_processors=2,delete_existing=True)
connection.commit()




### check that we have filenames


##########
########## OUTPUT RESULTS
##########
## output all sources to R file for analysis
sql_cmd = """SELECT source_id FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.outputRfile(source_ids,cursor,'../data_processed/hip_ogle_plot.dat')

## output tfes
sql_cmd = """SELECT source_id FROM sources WHERE original_source_id = source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.tfeOutput(source_ids,cursor,'../data_processed/hip_ogle_plot_tfe.dat')

connection.commit()
