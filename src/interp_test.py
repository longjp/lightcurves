###########
########### interp file that tests everything is working
###########
########### by James Long
########### date August 30, 2012

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
os.system("rm ../db/debosscher_test.db")
connection = sqlite3.connect('../db/debosscher_test.db')
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,REMOVE_RECORDS=True)
connection.commit()

## now load in all of the light curves, this is a small test set
folder = "../data/debosscher_test"
create_database.ingest_many_xml(folder,cursor,connection,
                                survey="hipparcos",
                                number_processors=2)
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





######### STORE SMOOTHED CURVES
## smooth curves and store smoothed values in measurements_smoothed
cursor.execute(sql_cmd)
connection.commit()
sql_cmd = """SELECT S.source_id, true_period, classification, survey, freq1_harmonics_freq_0 FROM sources AS S JOIN features AS F ON S.source_id=F.source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    print i

for i in db_info:
    tfe = create_database.get_measurements(i[0],cursor)
    smo = smoothers.supersmooth(tfe,1/i[4])
    tfe[:,1] = smo
    tfe[:,2] = 1
    create_database.insert_measurements(cursor,i[0],tfe,table='measurements_smoothed')	

connection.commit()






########### VISUALIZE SMOOTHED CURVES ###########
## does this work out okay?
## run several times and use evince to view both at once
which = np.random.randint(low=0,high=len(db_info))
reload(visualize)

tfe = create_database.get_measurements(db_info[which][0],cursor)
visualize.plot_curve(tfe,1/db_info[which][4],classification=db_info[which][2],survey=db_info[which][3],show_plot=False,save_figure=True,save_figure_name='original_plot.pdf')

tfe = create_database.get_measurements(db_info[which][0],cursor,table='measurements_smoothed')
visualize.plot_curve(tfe,1/db_info[which][4],classification=db_info[which][2],survey=db_info[which][3],show_plot=False,save_figure=True,save_figure_name='smoothed_plot.pdf')





######
###### ADD ENTRIES IN SOURCES FOR NOISIFIED VERSIONS
######

reload(create_database)
sql_cmd = """DELETE from sources WHERE source_id != original_source_id"""
cursor.execute(sql_cmd)

source_pragma = create_database.get_pragma(cursor,table='sources')
del source_pragma[source_pragma.index('raw_xml')]
source_pragma
n_points = [10,20,30]
n_versions_first = 2
n_versions_random = 1
sql_cmd = """SELECT source_id FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
len(db_info)
db_info = tolist(db_info)

## we are noisifying to an ogle cadence 
survey_dict={'hipparcos':'ogle'}
for i in db_info:
   create_database.noisify_smoothed_sources(cursor,i,source_pragma,survey_dict=survey_dict,n_points=n_points,complete_curve=True,n_versions_first=n_versions_first,n_versions_random=n_versions_random)

connection.commit()


#### check to see if this went okay
sql_cmd = """SELECT * FROM sources WHERE original_source_id != source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
len(db_info)



#### check to see if this went okay
sql_cmd = """SELECT * FROM sources WHERE original_source_id == source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
len(db_info)




##########
########## DERIVE FEATURES FOR THE DATA
##########
## select sources to derive features for (everything except original features)
sql_cmd = """SELECT source_id FROM sources WHERE source_id != original_source_id"""        
cursor.execute(sql_cmd)
connection.commit()
db_info = cursor.fetchall()
source_ids = tolist(db_info)

## get the cadences at which you want to noisify survey, these cadences are retreived
## from the measurements table of another database (in this case ogleiii cadences)
noise_dict = noisification.get_noisification_dict()
ogle = synthetic_data.CadenceFromSurvey(database_location='../db/ogleiiiall.db')
cadence_dict = {'ogle':ogle}

## derive the features and save
derive_features.derive_features_par(source_ids,noise_dict,cursor,connection,cadence_dict,number_processors=2,delete_existing=True)
connection.commit()


##########
########## OUTPUT RESULTS
##########
## output features file
sql_cmd = """SELECT source_id FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.outputRfile(source_ids,cursor,'../data_processed/arien_testing.dat')
connection.commit()

## output tfes (i.e. time, fluxes (magnitudes actually), and errors) in 
## a file, nice for doing visualization
sql_cmd = """SELECT source_id FROM sources WHERE original_source_id = source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.tfeOutput(source_ids,cursor,'../data_processed/arien_testing_tfe.dat')
connection.commit()

## output smoothed tfes, same thing as previous lines but uses measurements_smoothed table
sql_cmd = """SELECT source_id FROM sources WHERE original_source_id = source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.tfeOutput(source_ids,cursor,'../data_processed/arien_testing_tfe_smoothed.dat',table_name="measurements_smoothed")
connection.commit()
