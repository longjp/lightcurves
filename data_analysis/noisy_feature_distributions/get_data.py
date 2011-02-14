####
#### get debosscher data
####

import sys
sys.path.append('/home/james/Desktop/lightcurves/src/')

import sqlite3
import create_database
import derive_features
import noisification
import db_output
from time import time

## what columns for features in features table
features_file = "../../db/derived_features_list.txt" # where we define features

## setup the db
connection = sqlite3.connect('/home/james/Desktop/lightcurves/db/astronomy.db')
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,REMOVE_RECORDS=False)
connection.commit()



####
#### get the flux / time measurements for some objects
####

sql_cmd = '''SELECT source_id FROM sources WHERE survey='debosscher' AND number_points > 200 AND original_source_id=source_id'''
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info
source_id = []
for i in db_info:
    source_id.append(i[0])


db_output.tfeOutput(source_id,cursor,'tfe.txt')





####
#### get class of object we are looking at
####


sql_cmd = '''SELECT source_id,classification FROM sources WHERE survey='debosscher' AND number_points > 200 AND original_source_id=source_id'''
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info

to_output = []
for i in range(len(db_info)):
   to_output.append(str(db_info[i][0]) + '; ' + str(db_info[i][1]) + '\n') 




g = open('class_output.txt','w')
g.writelines(to_output)
g.close()
