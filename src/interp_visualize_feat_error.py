########## 
########## 
########## CONSTRUCT SIMULATED LC AND DERIVE FEATURES 
########## AT SPARSE CADENCE, SHIFTING PHASE AND
########## ERROR.
##########
########## PURPOSE: SHOW HOW SAMPLING OF LC 
########## INTRODUCES NOISE INTO FEATURES 
##########
########## by James Long 
########## date: 2/10/2013 
########## 


import synthetic_data
import numpy as np
import scipy
import visualize
import create_database
import sqlite3
import noisification
import derive_features
import db_output

# for getting db_info in nice format
def tolist(db_info):
 list1 = []
 for i in db_info:
  list1.append(i[0])
 return(list1)


features_file = "../db/derived_features_list.txt"
connection = sqlite3.connect('../db/error_dist_simulation.db')
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,REMOVE_RECORDS=True)



# for viewing features table
sql_cmd = """CREATE VIEW IF NOT EXISTS features_short AS SELECT source_id,freq1_harmonics_freq_0,std,max,weighted_average FROM features"""
cursor.execute(sql_cmd)



## given curve, cadence returns tfe
def ComputeTfe(aCurve,aCadence):
    mag_min_this = 12
    phase_this = scipy.stats.uniform(loc=0.0,scale=1.0).rvs()
    period_this = aCurve.period_this
    times = (aCadence.cadence_this - aCadence.cadence_this[0]
             + (period_this * phase_this))
    errors = aCadence.error_this
    fluxes = (aCurve.curve_this(times) 
              + mag_min_this + scipy.stats.norm.rvs(location=0,scale=1,size=errors.size) * errors)
    tfe = np.column_stack((times[:,np.newaxis],fluxes[:,np.newaxis],errors[:,np.newaxis]))
    return tfe




## light curve now in aRRLyraeFund.curve_this
aRRLyraeFund = synthetic_data.RRLyraeFund()
aRRLyraeFund.generateCurve()

## put cadence in aCadence
filename = "../data/OGLEIII/classical-cepheid/OGLE-LMC-CEP-2233.dat"
aCadence = synthetic_data.CadenceFromTFE()
aCadence.generate_cadence(fname=filename)


## check that this worked
tfe = ComputeTfe(aRRLyraeFund,aCadence)
visualize.plot_curve(tfe,period=aRRLyraeFund.period_this)




## parameters for entering tfes into database
source_class = "rr lyrae"
survey = "full"
points_per_curve = len(aCadence.error_this)
period = aRRLyraeFund.period_this
curve_info = [points_per_curve,source_class,0,0,0,0,None,survey,0,period]
curve_info_names = ["number_points","classification","c1","e1","c2","e2","raw_xml","survey","xml_filename","true_period"]






## generate light curve using full cadence
errors = aCadence.error_this
aCadence.error_this = np.zeros(len(errors)) + .005
tfe = ComputeTfe(aRRLyraeFund,aCadence)
create_database.enter_record(curve_info,curve_info_names,tfe,cursor)
## return aCadence.error_this to original state
aCadence.error_this = errors



## construct 50 tfe (randomness is phase and error)
## for lightcurve trucated at 10,20, . . ., 80 measurements
trunc_points = 90 - np.arange(10,90,10)
for trunc in trunc_points:
    aCadence.cadence_this = aCadence.cadence_this[0:trunc]
    aCadence.error_this = aCadence.error_this[0:trunc]
    survey = repr(trunc)
    points_per_curve = len(aCadence.error_this)
    curve_info = [points_per_curve,source_class,0,0,0,0,None,survey,0,period]
    for i in range(50):
        tfe = ComputeTfe(aRRLyraeFund,aCadence)
	create_database.enter_record(curve_info,curve_info_names,tfe,cursor)




## extract features
sql_cmd = """SELECT source_id FROM sources"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
noise_dict = noisification.get_noisification_dict()
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
db_output.outputRfile(source_ids,cursor,'../data_processed/visualize_feat_error.dat')

# output tfes
sql_cmd = """SELECT source_id FROM sources WHERE original_source_id = source_id"""
cursor.execute(sql_cmd)
db_info = cursor.fetchall()
source_ids = tolist(db_info)
db_output.tfeOutput(source_ids,cursor,'../data_processed/visualize_feat_error_tfe.dat')



