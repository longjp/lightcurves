import synthetic_data
import visualize
import numpy as np
import create_database
import sqlite3
import derive_features

reload(derive_features)
reload(create_database)
reload(synthetic_data)
reload(visualize)
reload(np)

# to do
# 1. turn generate_and_store_curves in synthetic_data into "survey1", should have a bunch
# of these functions for different sorts of surveys

# make and test connection to the database
features_file = "../db/derived_features_list.txt"
connection = sqlite3.connect('../db/simulated_astronomy.db')
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,REMOVE_RECORDS=True)


cadence = synthetic_data.poisson_process_cadence(nobs=100,rate=1,timeframe=False)
sinusoidal = synthetic_data.sinusoidal(cadence,period=np.pi,phase=0.,mag=1,mag_off=0,error=.1)
detached = synthetic_data.detached(cadence,period=np.pi,phase=0.,mag_off=0,error=.1,depth1=.7,depth2=.8,flat_frac=.5)

visualize.plot_curve(sinusoidal,freq=1/np.pi,plot_folded=True,plot_unfolded=True,classification='Sinusoidal')

visualize.plot_curve(detached,freq=1/np.pi,plot_folded=True,plot_unfolded=True,classification='Detached')

synthetic_data.generate_and_store_curves(100,200,cursor,connection)

# retreive what we created
sql_cmd = """SELECT * FROM sources WHERE survey = 'Synthetic' LIMIT 4 """        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
print db_info
source_ids = []
for i in db_info:
	source_ids.append(i[0])
print source_ids


# retreive what we created
sql_cmd = """SELECT source_id FROM sources"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
print db_info
source_ids = []
for i in db_info:
	source_ids.append(i[0])



derive_features.derive_features_par(source_ids,cursor,connection,number_processors=2,delete_existing=True,original_source_ids=False)


def tolist(db_info):
 list1 = []
 for i in db_info:
  list1.append(i[0])
 return(list1)



# 1. think about how to noisify curves
# 2. better method of getting source_ids than:
sql_cmd = """SELECT source_id FROM sources"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
source_ids




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

