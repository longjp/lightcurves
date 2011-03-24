import synthetic_data
import visualize
import numpy as np
import create_database
import sqlite3

reload(synthetic_data)
reload(visualize)
reload(np)

# to do
# 1. turn generate_and_store_curves in synthetic_data into "survey1", should have a bunch
# of these functions for different sorts of surveys

cadence = synthetic_data.poisson_process_cadence(nobs=100,rate=1,timeframe=False)
sinusoidal = synthetic_data.sinusoidal(cadence,period=np.pi,phase=0.,mag=1,mag_off=0,error=.1)


detached = synthetic_data.detached(cadence,period=np.pi,phase=0.,mag_off=0,error=.1,depth1=.7,depth2=.8,flat_frac=.5)

visualize.plot_curve(sinusoidal,freq=1/np.pi,plot_folded=True,plot_unfolded=True,classification='Sinusoidal')

visualize.plot_curve(detached,freq=1/np.pi,plot_folded=True,plot_unfolded=True,classification='Detached')


# make and test connection to the database
features_file = "../db/derived_features_list.txt"
connection = sqlite3.connect('../db/simulated_astronomy.db')
cursor = connection.cursor()

create_database.create_db(cursor,features_file=features_file,REMOVE_RECORDS=True)


sql_cmd = """SELECT source_id FROM sources LIMIT 2"""        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
print db_info


synthetic_data.generate_and_store_curves(100,100,cursor,connection)

# retreive what we created
sql_cmd = """SELECT source_id FROM sources WHERE survey = 'Synthetic' LIMIT 4 """        
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
print db_info
source_ids = []
for i in db_info:
	source_ids.append(i[0])

print source_ids

## print a few measurements for a source
sql_cmd = """SELECT time, flux, error FROM measurements WHERE source_id IN """ + repr(tuple(source_ids))
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
db_info


