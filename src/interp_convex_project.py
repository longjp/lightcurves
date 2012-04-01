########## 
########## 
########## NOISIFY LIGHT CURVES FOR CONVEX FINAL PROJECT 
##########
########## by James Long 
########## date: 3/22/2012 
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




# for getting db_info in nice format
def tolist(db_info):
 list1 = []
 for i in db_info:
  list1.append(i[0])
 return(list1)


## make and test connection to the database
features_file = "../db/derived_features_list.txt"
connection = sqlite3.connect('../db/ogleiiiconvex_project.db')
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,
                          REMOVE_RECORDS=True)
connection.commit()
folder = "../data/OGLEIII/mira"
create_database.ingest_many_tfes(folder,
                                ".dat",
                                cursor,
                                connection,
                                survey="ogle",
                                classification="Mira",
                                max_lightcurves=5)
connection.commit()
print "obtained miras . . ."
folder = "../data/OGLEIII/classical-cepheid-fundamental"
create_database.ingest_many_tfes(folder,
                                ".dat",
                                cursor,
                                connection,
                                survey="ogle",
                                classification="Classical Cepheid",
                                max_lightcurves=5)
connection.commit()
print "obtained classical cepheids . . ."
folder = "../data/OGLEIII/rr-ab_subset"
create_database.ingest_many_tfes(folder,
                                ".dat",
                                cursor,
                                connection,
                                survey="ogle",
                                classification="RR Lyrae AB",
                                max_lightcurves=5)
connection.commit()
print "obtained rr lyraes ab . . ."




## make a nice view of the features table
sql_cmd = """CREATE VIEW IF NOT EXISTS features_short AS SELECT source_id,freq1_harmonics_freq_0,std,max,weighted_average FROM features"""
cursor.execute(sql_cmd)

## make a nice view of the features table
sql_cmd = """CREATE VIEW IF NOT EXISTS sources_short AS SELECT source_id,original_source_id,classification,noisification,noise_args,true_period FROM sources"""
cursor.execute(sql_cmd)





## examine what we have collected
sql_cmd = """SELECT source_id,survey,number_points,classification,xml_filename FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
total_points = 0
for i in db_info:
    print i
    total_points += i[2]


## SANITY CHECK
print "the total number of points is:"
print total_points
sql_cmd = """SELECT count(*) FROM measurements"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
print "the number of entries in measurements table is:"
print db_info

connection.commit()


##
## make density plot of number of points by class
##
sql_cmd = """SELECT classification,number_points FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()

points_dict = {}
for i in db_info:
  try:
    points_dict[i[0]].append(i[1])
  except KeyError:
    points_dict[i[0]] = [i[1]]


for i in points_dict.keys():
  points_dict[i] = np.array(points_dict[i],dtype='float32')


reload(kde)
kde.produceKDE(points_dict)



## MAKE ENTRIES IN SOURCES TABLE THAT REPRESENT UNCERTAINTY
## TAKE 5 SUBSAMPLES OF HALF LENGTH OF CURVE

source_pragma = create_database.get_pragma(cursor,table='sources')

### make training entries
n_versions_first = 5
n_versions_random = 0
sql_cmd = """SELECT source_id, number_points FROM sources WHERE source_id = original_source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    n_points = [int(i[1] / 2.)]
    create_database.noisify_unsmoothed_sources(cursor,i[0],source_pragma,n_points=n_points,n_versions_first=n_versions_first,n_versions_random=0)



## check lengths
sql_cmd = """SELECT source_id, number_points FROM sources WHERE source_id = original_source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
len(db_info)

sql_cmd = """SELECT source_id, number_points FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
len(db_info)


## look at all entries
sql_cmd = """SELECT * FROM sources_short"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
len(db_info)
for i in db_info:
    print i



## DERIVE FEATURES
sql_cmd = """SELECT source_id FROM sources"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
noise_dict = noisification.get_noisification_dict()
derive_features.derive_features_par(source_ids,noise_dict,cursor,connection,number_processors=2,delete_existing=True)



##########
########## OUTPUT RESULTS
##########
## output all sources to R file for analysis
sql_cmd = """SELECT source_id FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.outputRfile(source_ids,cursor,'../data_processed/convexMeta.dat')


## output original sources, column for class, column for source id, 
## columns for features, so (#columns) = 2 + (#features)
sql_cmd = """SELECT source_id FROM sources WHERE source_id = original_source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.outputOriginalOnly(source_ids,cursor,'../data_processed/convexPoint.dat')

## output original sources, column for class, column for source id
sql_cmd = """SELECT source_id FROM sources WHERE source_id = original_source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.outputIntervals(source_ids,cursor,'../data_processed/convexInterval.dat')

## output tfes
sql_cmd = """SELECT source_id FROM sources WHERE original_source_id = source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.tfeOutput(source_ids,cursor,'../data_processed/convexTfe.dat')


connection.commit()
