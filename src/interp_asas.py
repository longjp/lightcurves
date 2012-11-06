###########
########### interp file for exploring ASAS data
###########
########### by James Long
########### date November 5, 2012

import os
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



## put results in a list
def tolist(db_info):
 list1 = []
 for i in db_info:
  list1.append(i[0])
 return(list1)



## make and test connection to the database
## need to tell program ahead of time what features you are using
## so it can create correct columns in database
features_file = "../db/derived_features_list_new.txt"
connection = sqlite3.connect('../db/asas.db')
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,REMOVE_RECORDS=True)
connection.commit()

## now load in all of the light curves, this is a small test set
folder = "../data/asas_ACVS_50k_new_aper_20120221"
create_database.ingest_many_xml(folder,cursor,connection,
                                survey="asas",
                                number_processors=2,
                                max_files=1000)
connection.commit()




### just for viewing purposes, sometimes I run this line by line for debugging
## make a nice view of the features table
sql_cmd = """CREATE VIEW IF NOT EXISTS features_short AS SELECT source_id,freq1_harmonics_freq_0,std,max,weighted_average FROM features"""
cursor.execute(sql_cmd)

## make a nice view of the sources table
sql_cmd = """CREATE VIEW IF NOT EXISTS sources_short AS SELECT source_id,original_source_id,classification,noisification,noise_args,true_period FROM sources"""
cursor.execute(sql_cmd)


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




####### DERIVE FEATURES FOR SOURCES
## derive features for sources
## retreive everything
sql_cmd = """SELECT source_id FROM sources"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
noise_dict = noisification.get_noisification_dict()
derive_features.derive_features_par(source_ids,noise_dict,cursor,connection,number_processors=2,delete_existing=True)
connection.commit()









##########
########## OUTPUT RESULTS
##########
## output features file
sql_cmd = """SELECT source_id FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.outputRfile(source_ids,cursor,'../data_processed/asas.dat')
connection.commit()

## output tfes (i.e. time, fluxes (magnitudes actually), and errors) in 
## a file, nice for doing visualization
sql_cmd = """SELECT source_id FROM sources WHERE original_source_id = source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.tfeOutput(source_ids,cursor,'../data_processed/asas_tfe.dat')
connection.commit()
