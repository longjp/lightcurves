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

features_file = "../db/derived_features_list.txt"
connection = sqlite3.connect('../db/error_dist_simulation.db')
cursor = connection.cursor()
create_database.create_db(cursor,features_file=features_file,REMOVE_RECORDS=True)




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
aCadence = synthetic_data.CadenceFromVOSource()
aCadence.generate_cadence()


## check that this worked
tfe = ComputeTfe(aRRLyraeFund,aCadence)
visualize.plot_curve(tfe,period=aRRLyraeFund.period_this)



## parameters for entering tfes into database
source_class = "rr lyrae"
survey = "simulated"
points_per_curve = len(aCadence.error_this)
period = aRRLyraeFund.period_this
curve_info = [points_per_curve,source_class,0,0,0,0,None,survey,0,period]
curve_info_names = ["number_points","classification","c1","e1","c2","e2","raw_xml","survey","xml_filename","true_period"]



## get tfe for full cadence and enter into db





## now cut curve down to 10 flux measurements
aCadence.cadence_this = aCadence.cadence_this[0:10]
aCadence.error_this = aCadence.error_this[0:10]

## construct 100 tfe (randomness is phase and error)
points_per_curve = len(aCadence.error_this)
for i in range(100):
        tfe = ComputeTfe(aRRLyraeFund,aCadence)
	create_database.enter_record(curve_info,curve_info_names,tfe,cursor)
