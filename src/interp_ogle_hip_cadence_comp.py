###########
########### this is slowly taking over ogle_hip_cadence_comparison.py
###########

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

#reload(db_output)
#reload(noisification)
#reload(derive_features)
#reload(create_database)
#reload(synthetic_data)
#reload(visualize)
#reload(np)



## put results in a list
def tolist(db_info):
 list1 = []
 for i in db_info:
  list1.append(i[0])
 return(list1)





## make and test connection to the database
features_file = "../db/derived_features_list.txt"
connection = sqlite3.connect('../db/ogle_hipparcos_cadence_comparison.db')
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,REMOVE_RECORDS=True)

## make a nice view of the features table
sql_cmd = """CREATE VIEW IF NOT EXISTS features_short AS SELECT source_id,freq1_harmonics_freq_0,std,max,weighted_average FROM features"""
cursor.execute(sql_cmd)

## make a nice view of the features table
sql_cmd = """CREATE VIEW IF NOT EXISTS sources_short AS SELECT source_id,original_source_id,classification,noisification,noise_args,true_period FROM sources"""
cursor.execute(sql_cmd)

## set up the survey and
## create two curves and visualize them
aSurvey = synthetic_data.surveySetup()
aSurvey.generateCurve()
tfe = np.column_stack((aSurvey.times[:,np.newaxis],aSurvey.fluxes[:,np.newaxis],aSurvey.errors[:,np.newaxis]))		       
visualize.plot_curve(tfe,period=aSurvey.period_this)









########### OGLE ###############
## change survey to OGLE cadence
db_filename = '../db/ogle_cadences.db'
aCadence = synthetic_data.CadenceFromSurvey(db_filename)
aSurvey.aCadence = aCadence
aSurvey.generateCurve()
tfe = np.column_stack((aSurvey.times[:,np.newaxis],aSurvey.fluxes[:,np.newaxis],aSurvey.errors[:,np.newaxis]))		       
visualize.plot_curve(tfe,period=aSurvey.period_this)



# generate ogle data, add to db
survey='ogle_train'
for i in range(500):
   aSurvey.generateCurve()
   tfe = np.column_stack((aSurvey.times[:,np.newaxis],aSurvey.fluxes[:,np.newaxis],aSurvey.errors[:,np.newaxis]))
   points_per_curve = len(aSurvey.times)
   source_class = aSurvey.class_name
   period = aSurvey.period_this
   curve_info = [points_per_curve,source_class,0,0,0,0,None,survey,0,period]
   curve_info_names = ["number_points","classification","c1","e1","c2","e2","raw_xml","survey","xml_filename","true_period"]
   print source_class
   print curve_info
   create_database.enter_record(curve_info,curve_info_names,tfe,cursor)


survey='ogle_test'
for i in range(500):
   aSurvey.generateCurve()
   tfe = np.column_stack((aSurvey.times[:,np.newaxis],aSurvey.fluxes[:,np.newaxis],aSurvey.errors[:,np.newaxis]))
   points_per_curve = len(aSurvey.times)
   source_class = aSurvey.class_name
   period = aSurvey.period_this
   curve_info = [points_per_curve,source_class,0,0,0,0,None,survey,0,period]
   curve_info_names = ["number_points","classification","c1","e1","c2","e2","raw_xml","survey","xml_filename","true_period"]
   print source_class
   print curve_info
   create_database.enter_record(curve_info,curve_info_names,tfe,cursor)



########### HIPPARCOS ###############
## change survey to hipparcos cadence
db_filename = '../db/hipparcos_cadences.db'
aCadence = synthetic_data.CadenceFromSurvey(db_filename)
aSurvey.aCadence = aCadence

## now examine a curve
aSurvey.generateCurve()
tfe = np.column_stack((aSurvey.times[:,np.newaxis],aSurvey.fluxes[:,np.newaxis],aSurvey.errors[:,np.newaxis]))		       
tfe[:,0].size
visualize.plot_curve(tfe,period=aSurvey.period_this)




# generate hipparcos data, add to db
survey='hipparcos_train'
for i in range(500):
   aSurvey.generateCurve()
   tfe = np.column_stack((aSurvey.times[:,np.newaxis],aSurvey.fluxes[:,np.newaxis],aSurvey.errors[:,np.newaxis]))
   points_per_curve = len(aSurvey.times)
   source_class = aSurvey.class_name
   period = aSurvey.period_this
   curve_info = [points_per_curve,source_class,0,0,0,0,None,survey,0,period]
   curve_info_names = ["number_points","classification","c1","e1","c2","e2","raw_xml","survey","xml_filename","true_period"]
   print source_class
   print curve_info
   create_database.enter_record(curve_info,curve_info_names,tfe,cursor)


survey='hipparcos_test'
for i in range(500):
   aSurvey.generateCurve()
   tfe = np.column_stack((aSurvey.times[:,np.newaxis],aSurvey.fluxes[:,np.newaxis],aSurvey.errors[:,np.newaxis]))
   points_per_curve = len(aSurvey.times)
   source_class = aSurvey.class_name
   period = aSurvey.period_this
   curve_info = [points_per_curve,source_class,0,0,0,0,None,survey,0,period]
   curve_info_names = ["number_points","classification","c1","e1","c2","e2","raw_xml","survey","xml_filename","true_period"]
   print source_class
   print curve_info
   create_database.enter_record(curve_info,curve_info_names,tfe,cursor)




connection.commit()

###############
############### end making sets
###############







######## VISUALIZE #########
###
### get which to call a random variable
### make 3 plots
### tune parameters (is it periodic
### 
sql_cmd = """SELECT source_id, true_period, classification, survey FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()

which = np.random.randint(low=0,high=len(db_info))
reload(visualize)
tfe = create_database.get_measurements(db_info[which][0],cursor)
visualize.plot_curve(tfe,db_info[which][1],classification=db_info[which][2],survey=db_info[which][3],show_plot=False,save_figure=True)







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
## smooth curves and store
reload(smoothers)
sql_cmd = """SELECT S.source_id, true_period, classification, survey, freq1_harmonics_freq_0 FROM sources AS S JOIN features AS F ON S.source_id=F.source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
    print i

for i in db_info:
    tfe = create_database.get_measurements(i[0],cursor)
    smo = smoothers.supersmooth(tfe,2/i[4])
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

source_pragma = create_database.get_pragma(cursor,table='sources')
n_points = [10,20,30,40,50,60,70,80,90,100]


### make training entries
n_versions_first = 5
n_versions_random = 1
train_sets = ("ogle_train","hipparcos_train")
sql_cmd = """SELECT source_id FROM sources WHERE source_id = original_source_id AND survey IN """ + repr(train_sets)
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
len(db_info)
db_info = tolist(db_info)
for i in db_info:
   create_database.noisify_unsmoothed_sources(cursor,i,source_pragma,n_versions_first=n_versions_first,n_versions_random=n_versions_random)

survey_dict={'hipparcos_train':'ogle','ogle_train':'hip'}
for i in db_info:
   create_database.noisify_smoothed_sources(cursor,i,source_pragma,survey_dict=survey_dict,complete_curve=True,n_versions_first=n_versions_first,n_versions_random=n_versions_random)

survey_dict={'hipparcos_train':'hip','ogle_train':'ogle'}
for i in db_info:
   create_database.noisify_smoothed_sources(cursor,i,source_pragma,survey_dict=survey_dict,complete_curve=True,n_versions_first=n_versions_first,n_versions_random=n_versions_random)





### make test entries
n_versions_first = 1
n_versions_random = 0
test_sets = ("ogle_test","hipparcos_test")
sql_cmd = """SELECT source_id FROM sources WHERE source_id = original_source_id AND survey IN """ + repr(test_sets)
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
len(db_info)
db_info = tolist(db_info)
for i in db_info:
   create_database.noisify_unsmoothed_sources(cursor,i,source_pragma,n_versions_first=n_versions_first,n_versions_random=n_versions_random)




#### check to see if this went okay
sql_cmd = """SELECT * FROM sources WHERE original_source_id != source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
  print i

len(db_info)







##########
########## NOISIFY THE DATA
##########
### code which noisifies this data properly
sql_cmd = """SELECT source_id FROM sources WHERE source_id != original_source_id"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
noise_dict = noisification.get_noisification_dict()
hip = synthetic_data.CadenceFromSurvey(database_location='../db/hipparcos_cadences.db')
ogle = synthetic_data.CadenceFromSurvey(database_location='../db/ogle_cadences.db')
cadence_dict = {'hip':hip,'ogle':ogle}
derive_features.derive_features_par(source_ids,noise_dict,cursor,connection,cadence_dict,number_processors=2,delete_existing=True)



##########
########## OUTPUT RESULTS
##########
## output all sources to R file for analysis
sql_cmd = """SELECT source_id FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.outputRfile(source_ids,cursor,'../data_processed/cadence_comparison/ogle_versus_hipparcos.dat')

## output tfes
sql_cmd = """SELECT source_id FROM sources WHERE original_source_id = source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.tfeOutput(source_ids,cursor,'../data_processed/cadence_comparison/tfe_ogle_versus_hipparcos.dat')


connection.commit()
