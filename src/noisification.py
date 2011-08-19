##
## contains functions to noisify curves
##
## by James Long
## date Dec 21, 2010
## modified March 24, 2011


import synthetic_data
import numpy as np
import scipy as sp
import scipy.interpolate
import visualize
import random

## selects some of the tfes, selection is based on args
def cadence_noisify(tfe,args):
    # first sort the tfes
    positions = tfe[:,0].argsort()
    tfe = tfe[positions,:]
    ## args[1] = 'first' if we want a contiguous selection of points
    if args[1] == 'first':
        # TODO: DELETE COMMENTED REGION AFTER SOME TESTING
        # if len(args) == 2:
        #     tfe = tfe[0:args[0],]
        # if len(args) == 3:
        tfe = tfe[args[2]:(args[2] + args[0]),]
    # args[1] = 'random' if we want a random selection of measurements
    if args[1] == 'random':
        to_keep = np.random.permutation(tfe.shape[0])[:args[0]]
        tfe = tfe[to_keep,]
        positions = tfe[:,0].argsort()
        tfe = tfe[positions,:]
    # return a time ordered set of measurements
    return(tfe)


def cadence_noisify_smoothed(tfe,args):
    ## grab a random cadence
    args[4][args[0]].generate_cadence()

    ## convert get all points to the length of the cadence
    if args[2] == 'all':
        args[2] = args[4][args[0]].cadence_this.size

    ## make sure we are not selecting too many points, otherwise warn aggressively
    if args[4][args[0]].cadence_this.size < args[2]:
        args[2] = args[4][args[0]].cadence_this.size
        for i in range(100):
            print "======= WARNING ========:: REQUESTED MORE POINTS FROM CURVE THAN EXIST"

    ## grab points from the cadence
    if(args[1] == 'first'):
        if (args[4][args[0]].cadence_this.size - args[2]) > 0:
            starting_point = np.random.randint(low=0,high=(args[4][args[0]].cadence_this.size - args[2]))
        else:
            starting_point = 0
        times_orig = args[4][args[0]].cadence_this[starting_point:(starting_point + args[2])]
        errors = args[4][args[0]].error_this[starting_point:(starting_point + args[2])]
    elif(args[1] == 'random'):
        random_selection = random.sample(range(args[4][args[0]].cadence_this.size),args[2])
        times_orig = args[4][args[0]].cadence_this[random_selection]
        errors = args[4][args[0]].error_this[random_selection]
    else:
        print "======= WARNING ========="
        print "INVALID TYPE, NEED TO BE FIRST OR RANDOM"
        return(0)

    ## randomly phase the times, obtain noisy flux measurements for these times
    times = times_orig + np.random.uniform()
    times = (times % args[3]) / args[3]

    ## interpolate function - have to concatentate so that f is defined on [0,1]
    f = sp.interpolate.interp1d(np.concatenate((-1+tfe[-1:,0],tfe[:,0],1+tfe[0:1,0])),
                                 np.concatenate((tfe[-1:,1],tfe[:,1],tfe[0:1,1])))
    fluxes = f(times) + np.random.normal(scale=errors)

    # bundle together times_orig, fluxes, errors and return them
    return(np.vstack((times_orig,fluxes,errors)).transpose())

def identity(tfe,args):
    return(tfe)

def get_noisification_dict():
    noisification_dict = {'cadence_noisify_smoothed':cadence_noisify_smoothed,
                          'cadence_noisify':cadence_noisify,'identity':identity}
    return(noisification_dict)

if __name__ == '__main__':
    if 1:
        ## get the cadences
        hip = synthetic_data.CadenceFromSurvey(database_location='../db/hipparcos_cadences.db')
        ogle = synthetic_data.CadenceFromSurvey(database_location='../db/ogle_cadences.db')
        cadence_dict = {'hip':hip,'ogle':ogle}
        period = 25
        number_points = 3
        
        ## make a curve
        tfe = np.ndarray(300).reshape((100,3))
        tfe[:,0] = np.linspace(.01,.99,100)
        tfe[:,1] = np.abs(tfe[:,0]) - pow(tfe[:,0],8)
        print tfe

        ## examine noisification
        for i in range(1):
            tfe2 = cadence_noisify_smoothed(tfe,['ogle','first',number_points,period,cadence_dict])
        print tfe2
        visualize.plot_curve(tfe2,period=period)

    if 0:
        randoms = np.random.normal(size=(10,3))
        randoms_out = cadence_noisify(randoms,[5,'first',6])
        print randoms_out
        print randoms

    if 0:
        noisification_dict = get_noisification_dict()
        print noisification_dict
