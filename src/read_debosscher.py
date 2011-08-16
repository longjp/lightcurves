########## 
########## 
########## READ, DERIVE FEATURE, AND NOISIFY DEBOSSCHER
##########
##########
########## by James Long 
########## date: 4/17/2011 
########## 

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
connection = sqlite3.connect('../db/hipparcos.db')
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,
                          REMOVE_RECORDS=True)
connection.commit()
folder = "../data/debosscher"
create_database.ingest_many_xml(folder,cursor,connection,
                                survey="hipparcos",
                                number_processors=2)









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
sql_cmd = """SELECT source_id,survey,number_points,classification FROM sources WHERE Classification IN""" + repr(OGLE)
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    print i

## should be 517 if joey's email is correct AND my code is correct
print(len(db_info))


## only using hipparcos sources
sql_cmd = """DELETE FROM sources WHERE classification IN""" + repr(OGLE)
cursor.execute(sql_cmd)



sql_cmd = """SELECT source_id,survey,number_points,classification FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    print i


len(db_info)



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


## remove everything with fewer than 50 flux measurements
sql_cmd = """DELETE FROM sources WHERE number_points < 50"""
cursor.execute(sql_cmd)

sql_cmd = """SELECT source_id,survey,number_points,classification FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    print i

## number of sources we will actually be using
print(len(db_info))


## used for splitting train / test in random way
train = 1*(scipy.stats.uniform().rvs(len(db_info)) > .3)
testtrain = map(lambda x,y:(x,y),list(train),map(lambda x: x[0],db_info))
sql_cmd = """UPDATE sources SET survey = (?) WHERE source_id = (?)"""
for i in testtrain:
	if i[0] == 1:
		group = "train"
	if i[0] == 0:
		group = "test"	
	cursor.execute(sql_cmd,(group,i[1]))


connection.commit()



sql_cmd = """SELECT source_id,survey,number_points,true_period,classification FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    print i




###
### retreive and then noisify entries in sources
###

source_pragma = create_database.get_pragma(cursor,table='sources')
n_points = [10,20,30,40,50,60,70,80,90,100]


## retrieve training data
sql_cmd = """SELECT source_id FROM sources WHERE survey = 'train'"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
n_versions_first = 5
n_versions_random = 1
for i in db_info:
   create_database.noisify_unsmoothed_sources(cursor,i,source_pragma,
                                              n_versions_first=n_versions_first,
                                              n_versions_random=n_versions_random)



## retreive test data
sql_cmd = """SELECT source_id FROM sources WHERE survey = 'test'"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
n_versions_first = 1
n_versions_random = 0
for i in db_info:
   create_database.noisify_unsmoothed_sources(cursor,i,source_pragma,
                                              n_versions_first=n_versions_first,
                                              n_versions_random=n_versions_random)




## make a nice view of the features table
sql_cmd = """CREATE VIEW IF NOT EXISTS sources_short AS SELECT source_id,original_source_id,classification,noisification,noise_args,true_period FROM sources"""
cursor.execute(sql_cmd)




## display sources
sql_cmd = """SELECT * FROM sources_short"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
	print i



###
### DERIVE FEATURES AND TAKE A LOOK AT CURVES
###

sql_cmd = """SELECT source_id FROM sources"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
noise_dict = noisification.get_noisification_dict()
derive_features.derive_features_par(source_ids,noise_dict,cursor,connection,number_processors=2,delete_existing=True)



# make a nice view of the features table
sql_cmd = """CREATE VIEW IF NOT EXISTS features_short AS SELECT source_id,freq1_harmonics_freq_0,std,max,weighted_average FROM features"""
cursor.execute(sql_cmd)

# take a look at the features
sql_cmd = """SELECT * FROM features_short"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
	print i



#######
####### OUTPUT FOR ANALYSIS IN R
#######

# output all sources to R file for analysis
sql_cmd = """SELECT source_id FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.outputRfile(source_ids,cursor,'../data_processed/hipparcos/sources00001.dat')

# output tfes
sql_cmd = """SELECT source_id FROM sources WHERE original_source_id = source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.tfeOutput(source_ids,cursor,'../data_processed/hipparcos/tfe00001.dat')
