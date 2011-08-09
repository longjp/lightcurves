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





# make and test connection to the database
features_file = "../db/derived_features_list.txt"
connection = sqlite3.connect('../db/ogle_hipparcos_cadence_comparison.db')
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,REMOVE_RECORDS=True)

# make a nice view of the features table
sql_cmd = """CREATE VIEW IF NOT EXISTS features_short AS SELECT source_id,freq1_harmonics_freq_0,std,max,weighted_average FROM features"""
cursor.execute(sql_cmd)

# make a nice view of the features table
sql_cmd = """CREATE VIEW IF NOT EXISTS sources_short AS SELECT source_id,original_source_id,classification,noisification,noise_args,true_period FROM sources"""
cursor.execute(sql_cmd)

# set up the survey and
# create two curves and visualize them
aCadence = synthetic_data.Cadence()
aClassicalCepheid = synthetic_data.ClassicalCepheid()
aMira = synthetic_data.Mira()
aRRLyraeFund = synthetic_data.RRLyraeFund()
aBetaPersei = synthetic_data.Eclipsing(
	dip_ratio=scipy.stats.uniform(loc=.2,scale=.8),
	fraction_flat=scipy.stats.uniform(loc=.2,scale=.6))
aBetaLyrae = synthetic_data.Eclipsing(
	dip_ratio=scipy.stats.uniform(loc=.5,scale=.5),
	fraction_flat=scipy.stats.uniform(loc=0,scale=.5))
class_names = ['Classical Cepheid','Mira','RR Lyrae Fundamental',
	       'Beta Persei','Beta Lyrae']
classes = [aClassicalCepheid,aMira,aRRLyraeFund,aBetaPersei,aBetaLyrae]
priors = np.array([.2,.2,.2,.2,.2])
aSurvey = synthetic_data.Survey(class_names,classes,priors,aCadence)
aSurvey.generateCurve()
tfe = np.column_stack((aSurvey.times[:,np.newaxis],
		       aSurvey.fluxes[:,np.newaxis],
		       aSurvey.errors[:,np.newaxis]))
visualize.plot_curve(tfe,period=aSurvey.period_this)



## set up the survey and
## create two curves and visualize them
db_filename = '../db/ogle_cadences.db'
aCadence = synthetic_data.CadenceFromSurvey(db_filename)
aSurvey = synthetic_data.Survey(class_names,classes,priors,aCadence)
aSurvey.generateCurve()
tfe = np.column_stack((aSurvey.times[:,np.newaxis],
		       aSurvey.fluxes[:,np.newaxis],
		       aSurvey.errors[:,np.newaxis]))
visualize.plot_curve(tfe,period=aSurvey.period_this)




# generate training data, add to db
survey='ogle_train'
for i in range(25):
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






######### make hipparcos training -> identical to synthetic training except for cadence

## set up the survey and
## create two curves and visualize them
db_filename = '../db/hipparcos_cadences.db'
aCadence = synthetic_data.CadenceFromSurvey(db_filename)
aSurvey = synthetic_data.Survey(class_names,classes,priors,aCadence)

## now examine a curve
aSurvey.generateCurve()
tfe = np.column_stack((aSurvey.times[:,np.newaxis],
		       aSurvey.fluxes[:,np.newaxis],
		       aSurvey.errors[:,np.newaxis]))
tfe[:,0].size
visualize.plot_curve(tfe,period=aSurvey.period_this)






## generate training data, add to db
survey='hipparcos_train'
for i in range(25):
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












## make sure you are selecting the correct sources before deleting
sql_cmd = """SELECT source_id, true_period, classification, survey FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info


###
### get which to call a random variable
### make 3 plots
### tune parameters (is it periodic
### 
which = np.random.randint(low=0,high=len(db_info))
reload(visualize)
tfe = create_database.get_measurements(db_info[which][0],cursor)
visualize.plot_curve(tfe,db_info[which][1],classification=db_info[which][2],survey=db_info[which][3],show_plot=False,save_figure=True)



## TODO -> smoothing must be done on estimated periods, not real ones
## smooth all sources and move them into measurements_smoothed table
reload(smoothers)
for i in db_info:
	tfe = create_database.get_measurements(i[0],cursor)
	smo = smoothers.supersmooth(tfe,i[1])
	tfe[:,1] = smo
	tfe[:,2] = 1
	create_database.insert_measurements(cursor,i[0],tfe,table='measurements_smoothed')	


connection.commit()



## do this work out okay?
## run several times and use evince to view both at once
which = np.random.randint(low=0,high=len(db_info))
reload(visualize)

tfe = create_database.get_measurements(db_info[which][0],cursor)
visualize.plot_curve(tfe,db_info[which][1],classification=db_info[which][2],survey=db_info[which][3],show_plot=False,save_figure=True,save_figure_name='original_plot.pdf')

tfe = create_database.get_measurements(db_info[which][0],cursor,table='measurements_smoothed')
visualize.plot_curve(tfe,db_info[which][1],classification=db_info[which][2],survey=db_info[which][3],show_plot=False,save_figure=True,save_figure_name='smoothed_plot.pdf')




### code which noisifies this data properly

