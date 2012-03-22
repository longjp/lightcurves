### load 100 asas sources
### create 5 versions of each, cutting at different mag limits
### derive features



import noisification
import db_output
import derive_features
import create_database

import create_database
import visualize
import numpy as np
import sqlite3
import scipy.stats
import math

## put results in a list
def tolist(db_info):
 list1 = []
 for i in db_info:
  list1.append(i[0])
 return(list1)


## make and test connection to the database
features_file = "../db/derived_features_list.txt"
connection = sqlite3.connect('../db/asas_censoring.db')
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,
                          REMOVE_RECORDS=True)
connection.commit()
folder = "../data/asas_full"
create_database.ingest_many_xml(folder,cursor,connection,
                                survey="hipparcos",
                                number_processors=2,
                                max_files=10)





source_pragma = create_database.get_pragma(cursor,table='sources')
censoring_levels = [10,11,12,13,14]

sql_cmd = """SELECT source_id FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)



# for i in db_info:
#    create_database.noisify_unsmoothed_sources(cursor,i,source_pragma,
#                                               n_versions_first=n_versions_first,
#                                               n_versions_random=n_versions_random)



