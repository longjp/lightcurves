########## 
########## 
########## READ IN AND DERIVE FEATURES FOR OGLE III 
########## RR LIGHT CURVES
##########
########## by James Long 
########## date: 8/22/2011 
########## 


import random
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
import smoothers
import kde
from matplotlib import pyplot as plt


## put results in a list
def tolist(db_info):
 list1 = []
 for i in db_info:
  list1.append(i[0])
 return(list1)


## make and test connection to the database
features_file = "../db/derived_features_list.txt"
connection = sqlite3.connect('../db/eclipse_RR.db')
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,REMOVE_RECORDS=True)
connection.commit()
folder = "../data/eclipse_RR"
create_database.ingest_many_tfes(folder,
                                ".dat",
                                cursor,
                                connection,
                                survey="ogle",
                                classification="rr")
connection.commit()


folder = "../data/ogle-rr-i"
create_database.ingest_many_tfes(folder,
                                 ".dat",
                                 cursor,
                                 connection,
                                 survey="ogle",
                                 classification="rr",
                                 max_lightcurves=10)
connection.commit()




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
    total_points = total_points + i[2]

## SANITY CHECK
print "the total number of points is:"
print total_points
sql_cmd = """SELECT count(*) FROM measurements"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
print "the number of entries in measurements table is:"
print db_info

connection.commit()





## DERIVE FEATURES
sql_cmd = """SELECT source_id FROM sources"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
noise_dict = noisification.get_noisification_dict()
derive_features.derive_features_par(source_ids,noise_dict,cursor,connection,number_processors=2,delete_existing=True)
connection.commit()




reload(smoothers)
sql_cmd = """SELECT S.source_id, true_period, classification, survey, freq1_harmonics_freq_0 FROM sources AS S JOIN features AS F ON S.source_id=F.source_id LIMIT 2"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()

curve_info_names = ["classification","original_source_id","number_points"]  



i = db_info[0]

tfe = create_database.get_measurements(i[0],cursor)
tfe = tfe[np.argsort(tfe[:,0],axis=0),]
times = tfe[:,0].copy()

## smoothers.supersmoothers messes with tfe, so hard to use
## rewrite this code
smo = smoothers.supersmooth(tfe,2/i[4])

plt.plot(tfe[:,0],tfe[:,1])
plt.plot(times,tfe[:,1])

## plot times vs tfe[:,1] and smo
## can difference them
## store times, smo, tfe[:,2] in database using enter record
## store times, mean(tfe[:,1]) + tfe[:,1] - smo, tfe[:,2] in db using enter rec.

tfe[:,1] = smo
curve_info = ['rr',i[0],tfe.shape[0]]
create_database.enter_record(curve_info,
                             curve_info_names,
                             tfe,
                             cursor=cursor)	

connection.commit()



## examine what we have collected
sql_cmd = """SELECT source_id,survey,number_points,classification,xml_filename FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
total_points = 0
for i in db_info:
    print i
    total_points = total_points + i[2]





##########
########## OUTPUT RESULTS
##########
## output all sources to R file for analysis
sql_cmd = """SELECT source_id FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.outputRfile(source_ids,cursor,'../data_processed/eclipse-rr.dat')

## output tfes
sql_cmd = """SELECT source_id FROM sources WHERE original_source_id = source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.tfeOutput(source_ids,cursor,'../data_processed/eclipse-rr-tfe.dat')


connection.commit()

