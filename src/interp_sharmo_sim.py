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

# make and test connection to the database
features_file = "../db/derived_features_list.txt"
connection = sqlite3.connect('../db/asas_full_cadences.db')
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,REMOVE_RECORDS=True)


folder = "../data/asas_full"
connection.commit()
create_database.ingest_many_xml(folder,cursor,connection,
                                survey="asas",
                                number_processors=2)
connection.commit()
connection.close()
