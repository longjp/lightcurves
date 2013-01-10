###
### used for deriving features used in job talk
###
### January 9, 2013
### James Long
###


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
folder = "../data/debosscher"
connection.commit()
create_database.ingest_many_xml(folder,cursor,connection,
                                survey="hipparcos",
                                number_processors=2)

## make a nice view of the features table
sql_cmd = """CREATE VIEW IF NOT EXISTS features_short AS SELECT source_id,freq1_harmonics_freq_0,std,max,weighted_average FROM features"""
cursor.execute(sql_cmd)

## make a nice view of the features table
sql_cmd = """CREATE VIEW IF NOT EXISTS sources_short AS SELECT source_id,original_source_id,classification,noisification,noise_args,true_period FROM sources"""
cursor.execute(sql_cmd)





## make sure you are selecting the correct sources before deleting
sql_cmd = """SELECT source_id,survey,number_points,classification FROM sources WHERE Classification != 'RR Lyrae, Fundamental Mode'"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    print i

print(len(db_info))
db_info = tolist(db_info)


## only using TO_KEEP sources
sql_cmd = """DELETE FROM sources WHERE source_id IN""" + repr(tuple(db_info))
cursor.execute(sql_cmd)

## only using TO_KEEP sources
sql_cmd = """DELETE FROM measurements WHERE source_id IN """ + repr(tuple(db_info))
cursor.execute(sql_cmd)

connection.commit()



## CHECK
sql_cmd = """SELECT source_id,survey,number_points,classification,true_period FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    print i

len(db_info)

sql_cmd = """SELECT count(*) FROM measurements"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info




## only use 50 sources
sql_cmd = """SELECT source_id FROM sources LIMIT 50"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info
db_info = tolist(db_info)


sql_cmd = """DELETE FROM sources WHERE source_id NOT IN""" + repr(tuple(db_info))
cursor.execute(sql_cmd)

sql_cmd = """DELETE FROM measurements WHERE source_id NOT IN """ + repr(tuple(db_info))
cursor.execute(sql_cmd)

connection.commit()


## check
sql_cmd = """SELECT source_id,survey,number_points,classification,true_period FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    print i

len(db_info)



## create noisified versions
reload(create_database)
sql_cmd = """DELETE from sources WHERE source_id != original_source_id"""
cursor.execute(sql_cmd)

source_pragma = create_database.get_pragma(cursor,table='sources')
del source_pragma[source_pragma.index('raw_xml')]
source_pragma
n_points = [10,20,30,40,50]
n_versions_first = 1
n_versions_random = 0
sql_cmd = """SELECT source_id FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
len(db_info)
db_info = tolist(db_info)

for i in db_info:
   create_database.noisify_unsmoothed_sources(cursor,
                                              i,
                                              source_pragma,
                                              n_points,
                                              n_versions_first=n_versions_first,
                                              n_versions_random=n_versions_random)

connection.commit()






## HACK
## n_points feature no longer being derived
## so copy number_points column from sources table to
## n_points column in features table

sql_cmd = """UPDATE features SET n_points=(SELECT number_points FROM sources WHERE features.source_id = sources.source_id)"""
cursor.execute(sql_cmd)
connection.commit()




##########
########## DERIVE FEATURES FOR THE DATA
##########
sql_cmd = """SELECT source_id FROM sources"""        
cursor.execute(sql_cmd)
connection.commit()
db_info = cursor.fetchall()
source_ids = tolist(db_info)
noise_dict = noisification.get_noisification_dict()
derive_features.derive_features_par(source_ids,noise_dict,cursor,connection,number_processors=2,delete_existing=True)
connection.commit()


##########
########## OUTPUT RESULTS
##########
## output all sources to R file for analysis
sql_cmd = """SELECT source_id FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.outputRfile(source_ids,cursor,'../data_processed/job_talk.dat')

## output tfes
sql_cmd = """SELECT source_id FROM sources WHERE original_source_id = source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.tfeOutput(source_ids,cursor,'../data_processed/job_talk_tfe.dat')

connection.commit()

# sql_cmd = """SELECT number_points FROM sources"""
# cursor.execute(sql_cmd)
# db_info = cursor.fetchall()
# print db_info


# columns_to_get = create_database.get_pragma(cursor,table='features')
# sql_cmd = """SELECT n_points FROM features"""
# cursor.execute(sql_cmd)
# db_info = cursor.fetchall()
# print db_info
# a = tolist(db_info)
