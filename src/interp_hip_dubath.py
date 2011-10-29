########## 
########## 
########## GET DUBATH'S HIPPARCOS DATA INTO DB
##########
##########
########## by James Long 
########## date: 8/4/2011 
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

reload(db_output)
reload(noisification)
reload(derive_features)
reload(create_database)
reload(synthetic_data)
reload(visualize)
reload(np)



## put results in a list
def tolist(db_info):
 list1 = []
 for i in db_info:
  list1.append(i[0])
 return(list1)


## make and test connection to the database
db_filename = '../db/hipparcos_dubath.db'
features_file = "../db/derived_features_list.txt"
connection = sqlite3.connect(db_filename)
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,
                          REMOVE_RECORDS=True)
connection.commit()


folder = "../data/hipparcos_dubath"
create_database.ingest_many_xml(folder,cursor,connection,
                                survey="hipparcos",
                                number_processors=2)
connection.commit()






## examine what we have collected
sql_cmd = """SELECT source_id,survey,number_points,classification FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    print i

print len(db_info)



sql_cmd = """SELECT source_id, min(time) FROM measurements GROUP BY source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info
db_info = tolist(db_info)


reload(create_database)
max_time = 8221.67
source_pragma = create_database.get_pragma(cursor,table='sources')
del source_pragma[source_pragma.index('raw_xml')]
source_pragma
for i in db_info:
	if i[1] < max_time:
		create_database.noisify_truncation(cursor,i[0],source_pragma,max_time)


connection.commit()


## examine what we have collected
sql_cmd = """SELECT source_id,survey,number_points,noise_args,noisification,classification FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    print i

print len(db_info)





## derive features for sources
## retreive everything
reload(noisification)
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
db_output.outputRfile(source_ids,cursor,'../data_processed/hipparcos_dubath_sources.dat')

## output tfes
sql_cmd = """SELECT source_id FROM sources WHERE original_source_id = source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.tfeOutput(source_ids,cursor,'../data_processed/hipparcos_dubath_tfe.dat')


connection.commit()

