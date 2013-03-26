########## 
########## 
########## compare cadences and magnitude error in  
########## hipparcos and ogle curves 
##########
########## by James Long 
########## date: 3/25/2013 
########## 


import random
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
import kde


## put results in a list
def tolist(db_info):
 list1 = []
 for i in db_info:
  list1.append(i[0])
 return(list1)


## make and test connection to the database
features_file = "../db/derived_features_list.txt"
connection = sqlite3.connect('../db/cadence_comp.db')
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,REMOVE_RECORDS=True)
connection.commit()
folder = "../data/ogle-i-random"
create_database.ingest_many_tfes(folder,
                                ".dat",
                                cursor,
                                connection,
                                survey="ogle",
                                classification="ogle")
connection.commit()

folder = "../data/hipparcos_dubath"
create_database.ingest_many_xml(folder,cursor,connection,
                                survey="hipparcos",
                                number_processors=2)
connection.commit()



## output tfe
sql_cmd = """SELECT sources.source_id, survey, measurements_id, time, flux, error FROM sources JOIN measurements ON sources.source_id=measurements.source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
fp = open('../data_processed/cadence_comparison_tfe.dat','w')
fp.write("source_id survey measurements_id, time, flux, error\n")
fp.write('\n'.join('%s %s %s %s %s %s' % x for x in db_info))
fp.close()


