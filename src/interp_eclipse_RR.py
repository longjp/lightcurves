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
                                 max_lightcurves=500)
connection.commit()


folder = "../data/debosscher_binary"
create_database.ingest_many_xml(folder,cursor,connection,
                                survey="debosscher_binary",
                                number_processors=2)


connection.commit()


## make a nice view of the features table
sql_cmd = """CREATE VIEW IF NOT EXISTS features_short AS SELECT source_id,freq1_harmonics_freq_0,std,max,weighted_average FROM features"""
cursor.execute(sql_cmd)

## make a nice view of the features table
sql_cmd = """CREATE VIEW IF NOT EXISTS sources_short AS SELECT source_id,original_source_id,classification,noisification,noise_args,true_period FROM sources"""
cursor.execute(sql_cmd)




## SANITY CHECK -- examine what's in db
sql_cmd = """SELECT source_id,survey,number_points,classification,xml_filename FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
total_points = 0
for i in db_info:
    print i
    total_points = total_points + i[2]


print "the total number of points is:"
print total_points
sql_cmd = """SELECT count(*) FROM measurements"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
print "the number of entries in measurements table is:"
print db_info







## DERIVE FEATURES
sql_cmd = """SELECT source_id FROM sources"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
noise_dict = noisification.get_noisification_dict()
derive_features.derive_features_par(source_ids,noise_dict,cursor,connection,number_processors=2,delete_existing=True)
connection.commit()







sql_cmd = """SELECT S.source_id, survey, freq1_harmonics_freq_0, classification, xml_filename FROM sources AS S JOIN features AS F ON S.source_id=F.source_id WHERE classification = 'rr'"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    print i


curve_info_names = ["classification","survey","original_source_id","number_points","xml_filename"]

for i in db_info:
    tfe = create_database.get_measurements(i[0],cursor)
    period = 1 / i[2]
    smo = smoothers.supersmooth(tfe,period,normalize_times=False)
    tfe_smoothed = np.concatenate((tfe[:,0].reshape((tfe[:,0].size,1)),
                                   smo.reshape(smo.size,1),
                                   tfe[:,2].reshape((tfe[:,2].size,1))),
                                  axis=1)
    residuals = np.mean(tfe[:,1]) + tfe[:,1] - smo
    tfe_residual = np.concatenate((tfe[:,0].reshape((tfe[:,0].size,1)),
                                   residuals.reshape(residuals.size,1),
                                   tfe[:,2].reshape((tfe[:,2].size,1))),
                                  axis=1)
    curve_info = ['smoothed',i[1],i[0],tfe.shape[0],i[4]]
    create_database.enter_record(curve_info,
                                 curve_info_names,
                                 tfe_smoothed,
                                 cursor=cursor)	
    curve_info[0] = 'residual'
    create_database.enter_record(curve_info,
                                 curve_info_names,
                                 tfe_residual,
                                 cursor=cursor)	

connection.commit()




## SANITY CHECK
## examine what we have collected
sql_cmd = """SELECT source_id,original_source_id,survey,number_points,classification FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    print i

total_points = 0
for i in db_info:
    print i
    total_points = total_points + i[3]

total_points





## DERIVE FEATURES
sql_cmd = """SELECT source_id FROM sources WHERE classification IN ('smoothed','residual')"""
cursor.execute(sql_cmd)
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
db_output.outputRfile(source_ids,cursor,'../data_processed/eclipse-rr.dat')

## output tfes
sql_cmd = """SELECT source_id FROM sources WHERE original_source_id = source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.tfeOutput(source_ids,cursor,'../data_processed/eclipse-rr-tfe.dat')


connection.commit()

