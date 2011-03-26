import synthetic_data
import visualize
import numpy as np
import create_database
import sqlite3
import derive_features
import noisification

reload(noisification)
reload(derive_features)
reload(create_database)
reload(synthetic_data)
reload(visualize)
reload(np)


# to do
# 1. turn generate_and_store_curves in synthetic_data into "survey1", should have a bunch
# of these functions for different sorts of surveys
# actually should have a "survey file" which call functions in synthetic data
# to generate data, noisifies this data some way, and derives all features
# 2. write a lot of docstrings, comment code
# 3. db_output, include true period, noisification, noise_args
# 4. several more prototype classes
# 5. read chapter in sql book on retreiving information / optimizing
# 6. for matching cadence, could i smooth all the curves and then
#    store these in a file (pickle), then noisified versions of these curves
#    would access the data in the file and sample


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
sql_cmd = """CREATE VIEW features_short AS SELECT source_id,freq1_harmonics_freq_0,std,max,weighted_average FROM features"""
cursor.execute(sql_cmd)


sql_cmd = """SELECT * FROM features_short"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
print db_info
for i in db_info:
	print i


# make a nice view of the features table
sql_cmd = """CREATE VIEW sources_short AS SELECT source_id,original_source_id,classification,noisification,noise_args,true_period FROM sources"""
cursor.execute(sql_cmd)



# create two curves and visualize them
cadence = synthetic_data.poisson_process_cadence(nobs=100,rate=1,timeframe=False)
sinusoidal = synthetic_data.sinusoidal(cadence,period=np.pi,phase=0.,mag=1,mag_off=0,error=.1)
detached = synthetic_data.detached(cadence,period=np.pi,phase=0.,mag_off=0,error=.1,depth1=.7,depth2=.8,flat_frac=.5)
visualize.plot_curve(sinusoidal,freq=1/np.pi,plot_folded=True,plot_unfolded=True,classification='Sinusoidal')
visualize.plot_curve(detached,freq=1/np.pi,plot_folded=True,plot_unfolded=True,classification='Detached')

# generate some synthetic data, add to db
synthetic_data.generate_and_store_curves(5,10,cursor,connection)

# retreive what we created
sql_cmd = """SELECT source_id FROM sources"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)

# create 1 noisy version of each clean source
# put result in sources (this does not generate features for these noisy sources)
n_points = 5
column_names = ["source_id","original_source_id","classification","survey","true_period","c1","e1","c2","e2","number_points","noisification","noise_args"]
sql_cmd = """SELECT """ + ', '.join(column_names) +  """ FROM sources WHERE source_id=(?)"""
for i in source_ids:
	cursor.execute(sql_cmd,[i])
	db_info = cursor.fetchall()
	curve_info = list(db_info[0])
	curve_info[1] = curve_info[0]
	curve_info[-3] = n_points
	curve_info[-2] = "cadence_noisify"
	curve_info[-1] = "[" + repr(n_points) + ",'random']"
	print curve_info
	sql_cmd2 = create_database.assembleSQLCommand("sources",column_names[1:])
	cursor.execute(sql_cmd2, curve_info[1:])


# display sources
sql_cmd = """SELECT * FROM sources_short"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
print db_info
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
print db_info
for i in db_info:
	print i




# retreive noisified curves
sql_cmd = """SELECT source_id FROM sources WHERE original_source_id != source_id"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
print source_ids


# derive features for these noisified sources
derive_features.derive_features_par(source_ids,noise_dict,cursor,connection,number_processors=2,delete_existing=True)



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




# delete noisified curves
sql_cmd = """DELETE FROM sources WHERE NOISIFICATION = 'cadence_noisify'"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
print db_info
source_ids = tolist(db_info)
print source_ids


