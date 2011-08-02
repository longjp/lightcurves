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
import visualize
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







##################
##################
################## make ogle_train, ogle_test, and hipparcos_train and hipparcos_test light curves
##################


# make and test connection to the database
features_file = "../db/derived_features_list.txt"
connection = sqlite3.connect('../db/ogle_hipparocs_cadence_comparison.db')
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
visualize.plot_curve(tfe,freq= (1 / (2*aSurvey.period_this)))



## set up the survey and
## create two curves and visualize them
db_filename = '../db/ogle_cadences.db'
aCadence = synthetic_data.CadenceFromSurvey(db_filename)
aSurvey = synthetic_data.Survey(class_names,classes,priors,aCadence)
aSurvey.generateCurve()
tfe = np.column_stack((aSurvey.times[:,np.newaxis],
		       aSurvey.fluxes[:,np.newaxis],
		       aSurvey.errors[:,np.newaxis]))
visualize.plot_curve(tfe,freq= (1 / (2*aSurvey.period_this)))




# generate training data, add to db
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
visualize.plot_curve(tfe,freq= (1 / (aSurvey.period_this)))






## generate training data, add to db
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








###
### retreive and then noisify training sets
###

# retrieve training data
training_sets = ("ogle_train","hipparcos_train")
sql_cmd = """SELECT source_id FROM sources WHERE survey IN """ + repr(training_sets)
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
len(source_ids)

# create n_version noisy versions of each clean source for every value of n_points
# put result in sources (this does not generate features)
n_versions = 5
n_points = np.arange(start=10,stop=101,step=10)
column_names = ["source_id","original_source_id","classification","survey","true_period","c1","e1","c2","e2","number_points","noisification","noise_args"]
sql_cmd = """SELECT """ + ', '.join(column_names) +  """ FROM sources WHERE source_id=(?)"""
for j in n_points:
	for i in source_ids:
		cursor.execute(sql_cmd,[i])
		db_info = cursor.fetchall()
		curve_info = list(db_info[0])
		curve_info[1] = curve_info[0]
		n_points_original = curve_info[-3]
		curve_info[-3] = int(j)
		curve_info[-2] = "cadence_noisify"
		sql_cmd2 = create_database.assembleSQLCommand("sources",column_names[1:])
		## input range(n_version) of source i sampling points with small time
		for k in range(n_versions):
			offset  = int(math.floor((float(n_points_original - j) / (n_versions - 1)) * k))
			curve_info[-1] = "[" + repr(j) + ",'first'," + repr(offset) + "]"
			print curve_info
			cursor.execute(sql_cmd2, curve_info[1:])
		## now just randomly grab points
		curve_info[-1] = "[" + repr(j) + ",'random']"
		cursor.execute(sql_cmd2, curve_info[1:])




###
### retreive and then noisify test sets
###

## retreive test
testing_sets = ("ogle_test","hipparcos_test")
sql_cmd = """SELECT source_id FROM sources WHERE survey IN """ + repr(testing_sets)
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
len(source_ids)

n_points = np.arange(start=10,stop=101,step=10)
column_names = ["source_id","original_source_id","classification","survey","true_period","c1","e1","c2","e2","number_points","noisification","noise_args"]
sql_cmd = """SELECT """ + ', '.join(column_names) +  """ FROM sources WHERE source_id=(?)"""
for j in n_points:
	for i in source_ids:
		cursor.execute(sql_cmd,[i])
		db_info = cursor.fetchall()
		curve_info = list(db_info[0])
		curve_info[1] = curve_info[0]
		curve_info[-3] = int(j)
		curve_info[-2] = "cadence_noisify"
		curve_info[-1] = "[" + repr(j) + ",'first']"
		sql_cmd2 = create_database.assembleSQLCommand("sources",column_names[1:])
		cursor.execute(sql_cmd2, curve_info[1:])






## display sources
sql_cmd = """SELECT * FROM sources_short"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
	print i


len(db_info)

connection.commit()

###############
############### end making sets
###############







## derive features for sources
## retreive everything
sql_cmd = """SELECT source_id FROM sources"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
noise_dict = noisification.get_noisification_dict()
derive_features.derive_features_par(source_ids,noise_dict,cursor,connection,number_processors=2,delete_existing=True)


connection.commit()

#######
####### OUTPUT FOR ANALYSIS IN R
#######

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

