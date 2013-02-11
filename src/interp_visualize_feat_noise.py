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


aRRLyraeFund = synthetic_data.RRLyraeFund()
aRRLyraeFund.generateCurve()





aCadence = synthetic_data.CadenceFromVOSource()
aCadence.generate_cadence()
aCadence.cadence_this



self.class_object.generateCurve()
self.aCadence.generate_cadence()
self.phase_this = self.phase.rvs()
self.mag_min_this = self.mag_min.rvs()
self.period_this = self.class_object.period_this
self.times = (self.aCadence.cadence_this - self.aCadence.cadence_this[0]
              + (self.period_this * self.phase_this))
self.errors = self.aCadence.error_this
self.fluxes = (self.class_object.curve_this(self.times) 
               + self.mag_min_this + scipy.stats.norm.rvs(location=0,scale=1,size=self.errors.size) * self.errors) 



## need function that returns tfe given
## cadence, curve
