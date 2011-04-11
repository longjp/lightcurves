#####
##### generate some synthetic light curves
#####
##### by James Long
##### date Jan 31, 2011
##### modified April 4, 2011
#####

## questions:
## 3. could use cadences that exactly matched asas
## 4. the scipy distributions all come from some meta class
##    there are two arguments for everything in the class 
##    (loc and scale) -> could we do something like this for
##    light curves?


### focus on getting individual prototypes right first -> then do the survey
### 1. survey will have different errors for different curves
###   (include some gross error)
###

import scipy.stats
import numpy as np
import visualize
import sqlite3
#import create_database



# RR Lyrae class
class RRLyraeFund():
    def __init__(self,period=scipy.stats.triang(.5,loc=.3,scale=.5),
                 magnitude=scipy.stats.triang(4.0/7.0,loc=.2,scale=.7),
                 fall_fraction=scipy.stats.uniform(loc=.1,scale=.1)):
        self.period = period
        self.magnitude = magnitude
        self.fall_fraction = fall_fraction
    def curve(self,period,magnitude,fall_fraction):
        def function(x):
            x = (x % period) / period
            part1 = (x < (1 - fall_fraction)) * ((1 / (1 - fall_fraction)) * x)
            part2 = (x >= (1 - fall_fraction)) * ((-1 / fall_fraction)*x + (1 / fall_fraction))
            return magnitude * (part1 + part2)
        return function
    def generateCurve(self):
        self.period_this = self.period.rvs()
        self.magnitude_this = self.magnitude.rvs()
        self.fall_fraction_this = self.fall_fraction.rvs()
        self.curve_this = self.curve(self.period_this,
                                     self.magnitude_this,
                                     self.fall_fraction_this)


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
            dip1 = ( (np.cos( ( 1 / p_dip ) * 
                              (2*np.pi*x)) + 1) / 2 )
            dip2 = (np.cos( ( 1 / p_dip ) * 
                            (2*np.pi*(x-.5)) ) 
                    - 1) * (dip_ratio / 2) + 1
            is_dip1 = (x < p_dip)
            greater = (x > .5)
            less = x < (.5 + p_dip)
            stacked = np.column_stack(
                (greater[:np.newaxis],less[:np.newaxis]))
            is_dip2 = stacked.all(axis=1)
            is_flat = 1 - (1*(is_dip1) + 1*(is_dip2))
            return magnitude * (dip1*is_dip1 
                                + dip2*is_dip2 + 1.0*is_flat)
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
    def __init__(self,period=scipy.stats.norm(loc=200,scale=30),
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
        self.curve_this = self.curve(self.period_this,
                                     self.magnitude_this)

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
            sine_comp = (np.sin( (2 * np.pi * x) + 
                                 (np.pi / 4)) + 1) / 2
            up_comp = (x < mix) * ((-1 / mix)*x + 1)
            down_comp = (x > mix) * ((1/(1-mix))*x - (mix)/(1-mix))
            return magnitude*(.5*sine_comp + 
                               .5*(up_comp + down_comp))
        return function
    def generateCurve(self):
        self.period_this = self.period.rvs()
        self.magnitude_this = self.magnitude.rvs()
        self.mix_this = self.mix.rvs()
        self.curve_this = self.curve(self.period_this,
                                     self.magnitude_this,
                                     self.mix_this)

class WhiteNoise:
    def curve(self,period=1):
        def function(x):
            return 0
        return function
    def generateCurve(self):
        self.period = 1
        self.curve_this = self.curve()

####
####
#### cadence class and associated functions
####
####

## for poisson times

def fixed_points(n_points):
    def function():
        return n_points
    return function

def fixed_rate(rate):
    def function():
        return rate
    return function

def a_poisson_process_cadence(nobs=fixed_points(200),rate=fixed_rate(1.5)):
    def function():
        cadence = np.random.exponential(rate(),nobs())
        for i in range(len(cadence)-1):
            cadence[i+1] = cadence[i] + cadence[i + 1]
        return cadence
    return function

# jittered


def obsByDay(probs_cdf,length):
    a = scipy.stats.uniform.rvs(size=length)
    ax, probs_cdf = np.ix_(a,probs_cdf)
    return (ax > probs_cdf).sum(axis=1)

def seqToTimes(seq):
    times_index = 0
    times = np.empty(seq.sum())
    for i in range(seq.size):
        times[times_index:(times_index + seq[i])] = i
        times_index += seq[i]
    return times


def jittered(probs=[.75,.2,.05],
             jitter=scipy.stats.norm(loc=0.,scale=.05),
             length = 1200):
    # turn the probability of 0,1,2 points
    # per night (probs) into a cdf
    probs_cdf = np.zeros(len(probs))
    probs_cdf[0] = probs[0]
    for i in range(len(probs_cdf)-1):
        probs_cdf[i+1] = probs_cdf[i] + probs[i+1]
    def function():
        seq = obsByDay(probs_cdf,length)
        times = seqToTimes(seq)
        times = times + jitter.rvs(len(times)) + 100
        times.sort()
        return times
    return function
# get days -> sum days, then jitter
# stop: fixed number (easy), time length (hard)


def chi_squared_error(sd):
    def function(size=1):
        return scipy.stats.chi2.rvs(10,0,size=size) / sd
    return function

# generate a certain number of points with certain spacings
# generate errors for those points
class Cadence:
    def __init__(self,cadence=jittered(),error=chi_squared_error(100)):
        self.cadence = cadence
        self.error = error
    def generate_cadence(self):
        self.cadence_this = self.cadence()
        self.error_this = self.error(self.cadence_this.size)



####
####
#### survey class
####
####

class Survey:
    def __init__(self,class_names,classes,priors,aCadence,
                 mag_min=scipy.stats.norm(loc=16,scale=2.0),
                 phase=scipy.stats.uniform(loc=0.0,scale=1.0)):
        self.class_names = class_names
        self.classes = classes
        self.priors = priors
        self.aCadence = aCadence
        self.mag_min = mag_min
        self.phase = phase
    def generateCurve(self):
        # choose a class
        class_index = np.random.multinomial(1,self.priors).argmax()
        self.class_name = self.class_names[class_index]
        self.class_object = self.classes[class_index]
        # generate a curve and a cadence
        self.class_object.generateCurve()
        self.aCadence.generate_cadence()
        # produce a set of points using curve, cadence, phase, mag_min
        self.phase_this = self.phase.rvs()
        self.mag_min_this = self.mag_min.rvs()
        self.period_this = self.class_object.period_this
        self.times = (self.aCadence.cadence_this - self.aCadence.cadence_this[0]
                      + (self.period_this * self.phase_this))
        self.errors = self.aCadence.error_this
        self.fluxes = (self.class_object.curve_this(self.times) 
                       + self.mag_min_this + self.errors) 


def surveySetup():
    aCadence = Cadence()
    aClassicalCepheid = ClassicalCepheid()
    aMira = Mira()
    aBetaPersei = Eclipsing(
        dip_ratio=scipy.stats.uniform(loc=.2,scale=.8),
        fraction_flat=scipy.stats.uniform(loc=.2,scale=.6))
    aBetaLyrae = Eclipsing(
        dip_ratio=scipy.stats.uniform(loc=.5,scale=.5),
        fraction_flat=scipy.stats.uniform(loc=0,scale=.5))
    aRRLyraeFund = RRLyraeFund()
    class_names = ['Classical Cepheid','Mira',
                   'Beta Persei','Beta Lyrae','RR Lyrae Fundamental Mode']
    classes = [aClassicalCepheid,aMira,
               aBetaPersei,aBetaLyrae,aRRLyraeFund]
    priors = np.array([.2,.2,.1,.1,.4])
    aSurvey = Survey(class_names,classes,priors,aCadence)
    return aSurvey


if __name__ == "__main__":
    if 1:
        aSurvey = surveySetup()
        aSurvey.generateCurve()
        print "class is: " + aSurvey.class_name
        tfe = np.column_stack((aSurvey.times[:,np.newaxis],aSurvey.fluxes[:,np.newaxis],aSurvey.errors[:,np.newaxis]))
        print "the period is:"
        print aSurvey.period_this
        visualize.plot_curve(tfe,
                             freq=(1 / 
                                   (2*aSurvey.period_this)),
                             classification=
                             aSurvey.class_name)        


    if 0:
        aRRLyraeFund = RRLyraeFund()
        aRRLyraeFund.generateCurve()
        print "RR Lyrae Fundamental Mode Period:"
        print aRRLyraeFund.period_this
        aJittered = jittered()
        cadence = aJittered()
        fluxes = aRRLyraeFund.curve_this(cadence)
        tfe = np.column_stack((cadence[:,np.newaxis],fluxes[:,np.newaxis], np.empty(fluxes.size)[:np.newaxis]))
        visualize.plot_curve(tfe,freq= (1 / (2*aRRLyraeFund.period_this)))

    if 0:
        aBetaPersei = Eclipsing(
            dip_ratio=scipy.stats.uniform(loc=.2,scale=.8),
            fraction_flat=scipy.stats.uniform(loc=.2,
                                              scale=.6))
        aBetaLyrae = Eclipsing(
            dip_ratio=scipy.stats.uniform(loc=.5,scale=.5),
            fraction_flat=scipy.stats.uniform(loc=0,
                                              scale=.5))
        aCadence = Cadence()
        class_names = ['BetaPersei','BetaLyrae']
        classes = [aBetaPersei,aBetaLyrae]
        priors = np.array([.5,.5])
        aSurvey = Survey(class_names,classes,
                         priors,aCadence)
        aSurvey.generateCurve()
        print "class is: " + aSurvey.class_name
        tfe = np.column_stack((aSurvey.times[:,np.newaxis],aSurvey.fluxes[:,np.newaxis],aSurvey.errors[:,np.newaxis]))
        visualize.plot_curve(tfe,
                             freq=(1 / 
                                   (2*aSurvey.period_this)),
                             classification=
                             aSurvey.class_name)
        
    if 0:
        #probs = [.5,.25,.25]
        #length = 20
        #dist = scipy.stats.norm(loc=0.,scale=.05)
        jitteredModel = jittered()
        print jitteredModel
        print jitteredModel()
    if 0:
        aCadence = Cadence()
        aClassicalCepheid = ClassicalCepheid()
        aMira = Mira()
        aEclipsing = Eclipsing()
        class_names = ['Classical Cepheid','Mira','Eclipsing']
        classes = [aClassicalCepheid,aMira,aEclipsing]
        priors = np.array([.3,.3,.4])
        aSurvey = Survey(class_names,classes,priors,aCadence)
        aSurvey.generateCurve()
        tfe = np.column_stack((aSurvey.times[:,np.newaxis],aSurvey.fluxes[:,np.newaxis],aSurvey.errors[:,np.newaxis]))
        visualize.plot_curve(tfe,freq= (1 / (2*aSurvey.period_this)))


    # testing Cadence
    if 0:
        aCadence = Cadence()
        print aCadence.cadence
        aCadence.generate_cadence()
        print aCadence.cadence_this
        print len(aCadence.cadence_this)
        #print aCadence.error_this
    if 0:
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
