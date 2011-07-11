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

# number of sources we will actually be using
print(len(db_info))


## stopped using b/c problem
## assign to test and training
## sources with fewer than 100 flux measurements are test
## sources with 100 or more are test
##sql_cmd = """UPDATE sources SET survey = 'test' WHERE number_points < 100"""
##cursor.execute(sql_cmd)
##sql_cmd = """UPDATE sources SET survey = 'train' WHERE number_points > 100"""
##cursor.execute(sql_cmd)
##connection.commit()


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
n_points = np.arange(start=10,stop=51,step=5)
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
### retreive and then noisify training
###


# retreive test
sql_cmd = """SELECT source_id FROM sources WHERE survey = 'test'"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)

n_points = np.arange(start=5,stop=51,step=5)
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



# make a nice view of the features table
sql_cmd = """CREATE VIEW IF NOT EXISTS sources_short AS SELECT source_id,original_source_id,classification,noisification,noise_args,true_period FROM sources"""
cursor.execute(sql_cmd)




# display sources
sql_cmd = """SELECT * FROM sources_short"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
for i in db_info:
	print i



###
### DERIVE FEATURES AND TAKE A LOOK AT CURVES
###

# retreive everything
sql_cmd = """SELECT source_id FROM sources"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)

noise_dict = noisification.get_noisification_dict()

# derive features for sources
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




