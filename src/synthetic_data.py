#####
##### generate some synthetic light curves
#####
##### by James Long
##### date Jan 31, 2011
##### modified April 4, 2011
#####

## questions:
## 1. should I pack all arguments into a list
## 2. nobs doesn't always make sense
## 3. could use cadences that exactly matched asas
## 4. arguments in the function definition? class X(are there ever args here?)

### focus on getting individual prototypes right first -> then do the survey
### 1. survey will have different errors for different curves
###   (include some gross error)
###

import scipy.stats
import numpy as np
import visualize
import sqlite3
#import create_database

def poisson_process_cadence(nobs=100,rate=1,timeframe=False):
    cadence = np.random.exponential(rate,nobs)
    for i in range(len(cadence)-1):
        cadence[i+1] = cadence[i] + cadence[i+1]
    return cadence


# RR Lyrae class

# ecplising class - used for Beta Persei, Beta Lyrae, ect.
class Eclipsing():
    def __init__(self,period=scipy.stats.pareto(4,loc=.2,scale=1.7),
                 magnitude=scipy.stats.pareto(3,0,.3),
                 dip_ratio=scipy.stats.uniform(loc=.2,scale=.8),
                 fraction_flat=scipy.stats.uniform(loc=.2,scale=.6)):
        self.period = period
        self.magnitude = magnitude
        self.dip_ratio = dip_ratio
        self.fraction_flat = fraction_flat
    def curve(self,period,magnitude,dip_ratio,fraction_flat):
        def function(x):
            x = (x % period) / period
            p_dip = (1 - fraction_flat) / 2
            dip1 = ( (np.cos( ( 1 / p_dip ) * (2*np.pi*x)) + 1) / 2 )
            dip2 = (np.cos( ( 1 / p_dip ) * (2*np.pi*(x-.5)) ) - 1) * (dip_ratio / 2) + 1
            is_dip1 = (x < p_dip)
            greater = (x > .5)
            less = x < (.5 + p_dip)
            stacked = np.column_stack((greater[:np.newaxis],less[:np.newaxis]))
            is_dip2 = stacked.all(axis=1)
            is_flat = 1 - (1*(is_dip1) + 1*(is_dip2))
            return magnitude * (dip1*is_dip1 + dip2*is_dip2 + 1.0*is_flat)
        return function
    def generateCurve(self):
        self.period_this = self.period.rvs()
        self.magnitude_this = self.magnitude.rvs()
        self.dip_ratio_this = self.dip_ratio.rvs()
        self.fraction_flat_this = self.fraction_flat.rvs()
        self.curve_this = self.curve(self.period_this,
                                     self.magnitude_this,
                                     self.dip_ratio_this,
                                     self.fraction_flat_this)

# look up lamba functions / anonymous functions
# classes for 2 eclipsing binaries (inheritance!!!) + RR Lyrae

# Miras!!!
class Mira:
    def __init__(self,period=scipy.stats.norm(loc=300,scale=50),
               magnitude=scipy.stats.norm(loc=2,scale=.3)):
        self.period = period
        self.magnitude = magnitude
    def curve(self,period,magnitude):
        def function(x):
            x = (x % period) / period
            return np.sin(2 * np.pi * x) * magnitude
        return function
    def generateCurve(self):
        self.period_this = self.period.rvs()
        self.magnitude_this = self.magnitude.rvs()
        self.curve_this = self.curve(self.period_this,self.magnitude_this)

# see p 87 ''light curves of variable stars''
# for more information on cepheids
class ClassicalCepheid:
    def __init__(self,period=scipy.stats.pareto(3,loc=0,scale=20),
                 magnitude=scipy.stats.pareto(3,0,.3),
                 mix=scipy.stats.uniform(loc=0,scale=.4)):
        self.period = period
        self.magnitude = magnitude
        self.mix = mix
    def curve(self,period,magnitude,mix):
        def function(x):
            x = (x % period) / period
            sine_comp = (np.sin( (2 * np.pi * x) + (np.pi / 4)) + 1) / 2
            up_comp = (x < mix) * ((-1 / mix)*x + 1)
            down_comp = (x > mix) * ((1/(1-mix))*x - (mix)/(1-mix))
            return magnitude*(.5*sine_comp + .5*(up_comp + down_comp))
        return function
    def generateCurve(self):
        self.period_this = self.period.rvs()
        self.magnitude_this = self.magnitude.rvs()
        self.mix_this = self.mix.rvs()
        self.curve_this = self.curve(self.period_this,self.magnitude_this,
                                     self.mix_this)

class WhiteNoise:
    def curve(self,period=1):
        def function(x):
            return 0
        return function
    def generateCurve(self):
        self.period = 1
        self.curve_this = self.curve()


class Survey:
    def __init__(self,n_points=100,mag_min=7.5,
                 phase=np.random.uniform,error=0,cadence=1):
        self.n_points = n_points
        self.mag_min = mag_min
        self.phase = phase
        self.error = error
        self.cadence = cadence
        


if __name__ == "__main__":
    # testing Ecplising
    if 1:
        aEclipsing = Eclipsing()
        aEclipsing.generateCurve()
        print "Eclipsing period:"
        print aEclipsing.period_this
        cadence = poisson_process_cadence(10000,10)
        fluxes = aEclipsing.curve_this(cadence)
        tfe = np.column_stack((cadence[:,np.newaxis],fluxes[:,np.newaxis], np.empty(fluxes.size)[:np.newaxis]))
        visualize.plot_curve(tfe,freq= (1 / (2*aEclipsing.period_this)))

    # test Mira
    if 0:
        aMira = Mira()
        aMira.generateCurve()
        print "Mira period:"
        print aMira.period_this
        cadence = poisson_process_cadence(100,10)
        fluxes = aMira.curve_this(cadence)
        tfe = np.column_stack((cadence[:,np.newaxis],fluxes[:,np.newaxis], np.empty(fluxes.size)[:np.newaxis]))
        visualize.plot_curve(tfe,freq= (1 / (2*aMira.period_this)))

    # test Classical Cepheid
    if 0:
        classicalCeph = ClassicalCepheid()
        classicalCeph.generateCurve()
        print "This is the mix: ", classicalCeph.mix_this
        print classicalCeph.period_this
        cadence = poisson_process_cadence(100,10)
        fluxes = classicalCeph.curve_this(cadence)
        tfe = np.column_stack((cadence[:,np.newaxis],fluxes[:,np.newaxis], np.empty(fluxes.size)[:np.newaxis]))
        visualize.plot_curve(tfe,freq= (1 / (2*classicalCeph.period_this)))
