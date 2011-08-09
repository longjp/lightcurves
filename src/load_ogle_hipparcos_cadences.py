########## 
########## 
########## COMPARE PERFORMANCE OF NOISIFIED CLASSIFIERS ON 
########## SYNTHETIC DATA WHEN TEST / TRAIN HAVE CADENCE
########## OF OGLE / HIPPARCOS (AND VISA VERSA)
##########
########## by James Long 
########## date: 4/17/2011 
########## 

import noisification
import db_output
import derive_features
import synthetic_data
import visualize
import create_database
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


## MAKE HIPPARCOS CADENCES DATABASE
## make and test connection to the database
db_filename = '../db/hipparcos_cadences.db'
features_file = "../db/derived_features_list.txt"
connection = sqlite3.connect(db_filename)
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,
                          REMOVE_RECORDS=True)
connection.commit()
folder = "../data/debosscher"
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

##
## remove everything not in an hipparcos, first select OGLE sources
##
OGLE = ("Multiple Mode Cepheid","RR Lyrae, Double Mode","Algol (Beta Persei)","Beta Lyrae","W Ursae Majoris")

## make sure you are selecting the correct sources before deleting
sql_cmd = """SELECT source_id FROM sources WHERE Classification IN""" + repr(OGLE)
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    print i

## should be 517 if joey's email is correct AND my code is correct
print(len(db_info))

ogle_sources = tolist(db_info)



## delete OGLE sources from sources and measurements tables
sql_cmd = """DELETE FROM sources WHERE classification IN""" + repr(OGLE)
cursor.execute(sql_cmd)
sql_cmd = """DELETE FROM measurements WHERE source_id IN """ + repr(tuple(ogle_sources))
cursor.execute(sql_cmd)
connection.commit()


sql_cmd = """SELECT count(*) FROM measurements"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info
sql_cmd = """SELECT count(DISTINCT source_id) FROM measurements"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info
sql_cmd = """SELECT count(*) FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info


## remove everything with fewer than 100 flux measurements
sql_cmd = """SELECT number_points FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    print i

len(db_info)
db_info = tolist(db_info)

sumList = 0
for i in db_info:
	sumList = i + sumList

float(sumList) / len(db_info)


sql_cmd = """SELECT source_id FROM sources WHERE number_points < 100"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info

db_info = tolist(db_info)
sql_cmd = """DELETE FROM measurements WHERE source_id IN""" + repr(tuple(db_info))
cursor.execute(sql_cmd)

sql_cmd = """DELETE FROM sources WHERE number_points < 100"""
cursor.execute(sql_cmd)

## ::check what we did::
## see how many sources in measurements and how many sources in sources
sql_cmd = """SELECT count(DISTINCT source_id) FROM measurements"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info
sql_cmd = """SELECT count(*) FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info



##############
connection.commit()
connection.close()









## OGLE CADENCES DATABASE
## make and test connection to the database
db_filename = '../db/ogle_cadences.db'
features_file = "../db/derived_features_list.txt"
connection = sqlite3.connect(db_filename)
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,
                          REMOVE_RECORDS=True)
connection.commit()
folder = "../data/debosscher"
create_database.ingest_many_xml(folder,cursor,connection,
                                survey="ogle",
                                number_processors=2)
connection.commit()






## examine what we have collected
sql_cmd = """SELECT source_id,survey,number_points,classification FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    print i

print len(db_info)

##
## remove everything not in an hipparcos, first select OGLE sources
##
OGLE = ("Multiple Mode Cepheid","RR Lyrae, Double Mode","Algol (Beta Persei)","Beta Lyrae","W Ursae Majoris")

## make sure you are selecting the correct sources before deleting
sql_cmd = """SELECT source_id FROM sources WHERE Classification IN""" + repr(OGLE)
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    print i

## should be 517 if joey's email is correct AND my code is correct
print(len(db_info))

ogle_sources = tolist(db_info)



## delete NON OGLE sources from sources and measurements tables
sql_cmd = """DELETE FROM sources WHERE classification NOT IN""" + repr(OGLE)
cursor.execute(sql_cmd)
sql_cmd = """DELETE FROM measurements WHERE source_id NOT IN """ + repr(tuple(ogle_sources))
cursor.execute(sql_cmd)
connection.commit()


sql_cmd = """SELECT count(*) FROM measurements"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info
sql_cmd = """SELECT count(DISTINCT source_id) FROM measurements"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info
sql_cmd = """SELECT count(*) FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info


## remove everything with fewer than 100 flux measurements
sql_cmd = """SELECT number_points FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    print i

len(db_info)
db_info = tolist(db_info)

sumList = 0
for i in db_info:
	sumList = i + sumList

float(sumList) / len(db_info)


sql_cmd = """SELECT source_id FROM sources WHERE number_points < 100"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info

db_info = tolist(db_info)
sql_cmd = """DELETE FROM measurements WHERE source_id IN""" + repr(tuple(db_info))
cursor.execute(sql_cmd)

sql_cmd = """DELETE FROM sources WHERE number_points < 100"""
cursor.execute(sql_cmd)

## ::check what we did::
## see how many sources in measurements and how many sources in sources
sql_cmd = """SELECT count(DISTINCT source_id) FROM measurements"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info
sql_cmd = """SELECT count(*) FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info



##############
connection.commit()
connection.close()


