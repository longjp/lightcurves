import synthetic_data
import visualize
import numpy as np
import create_database
import sqlite3
import derive_features
import noisification
import db_output
import math
import scipy.stats

reload(db_output)
reload(noisification)
reload(derive_features)
reload(create_database)
reload(synthetic_data)
reload(visualize)
reload(np)


# to do
# 1. turn generate_and_store_curves in synthetic_data into "survey1", should  have a bunch
# of these functions for different sorts of surveys
# actually should have a "survey file" which call functions in synthetic data
# to generate data, noisifies this data some way, and derives all features
# 2. write a lot of docstrings, comment code
# 4. several more prototype classes
# 5. read chapter in sql book on retreiving information / optimizing
# 6. turn some of the for statements into list comprehensions
#        (or map statements)

# for getting db_info in nice format
def tolist(db_info):
 list1 = []
 for i in db_info:
  list1.append(i[0])
 return(list1)

# make and test connection to the database
features_file = "../db/derived_features_list.txt"
connection = sqlite3.connect('../db/simulated_astronomy.db')
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




# generate training data, add to db
survey='train'
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
	create_database.enter_record(curve_info,curve_info_names,tfe,cursor,original_number=-1)




survey='test'
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
	create_database.enter_record(curve_info,curve_info_names,tfe,cursor,original_number=-1)







###
### retreive and then noisify training
###

# retrieve training data
sql_cmd = """SELECT source_id FROM sources WHERE survey = 'train'"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)

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
		# input range(n_version) of source i sampling points with small time
		for k in range(n_versions):
			offset  = int(math.floor((float(n_points_original - j) / (n_versions - 1)) * k))
			curve_info[-1] = "[" + repr(j) + ",'first'," + repr(offset) + "]"
			print curve_info
			cursor.execute(sql_cmd2, curve_info[1:])
		# now just randomly grab points
		curve_info[-1] = "[" + repr(j) + ",'random']"
		cursor.execute(sql_cmd2, curve_info[1:])


###
### retreive and then noisify test
###

# retreive test
sql_cmd = """SELECT source_id FROM sources WHERE survey = 'test'"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)

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


# display sources
sql_cmd = """SELECT * FROM sources_short"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
	print i


# retreive everything
sql_cmd = """SELECT source_id FROM sources"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)

noise_dict = noisification.get_noisification_dict()

# derive features for sources
derive_features.derive_features_par(source_ids,noise_dict,cursor,connection,number_processors=2,delete_existing=True)





# take a look at the features
sql_cmd = """SELECT * FROM features_short"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
	print i

# output all sources to R file for analysis
sql_cmd = """SELECT source_id FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.outputRfile(source_ids,cursor,'../data_analysis/synthetic_analysis/sources00001.dat')

# output tfes
sql_cmd = """SELECT source_id FROM sources WHERE original_source_id = source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.tfeOutput(source_ids,cursor,'../data_analysis/synthetic_analysis/tfe00001.dat')



# create lots of well sampled sources
# create crummy sources
# noisify good sources to match crummy sources


# create a test + train good sources
# noisify test + train (5 - 100 flux by 5)
# noisfy each train several times (different chunks of the flux measurements)
# output results to R

# set up R code to do classification / some exploration

# create more reasonable classes / survey characteristics (discuss with Joey)
# redo the above
# study how often we get correct period
# nadaraya watson / smoother in python or R

