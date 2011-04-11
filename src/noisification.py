##
## contains functions to noisify curves
##
## by James Long
## date Dec 21, 2010
## modified March 24, 2011

import numpy as np

# args contains:
# 1. (estimated) period of clean curve
# 2. times to sample continuous curves at
# 3. 
def smooth(tfe,args):
    print 1

# selects some of the tfes, selection is based on args
def cadence_noisify(tfe,args):
    # first sort the tfes
    positions = tfe[:,0].argsort()
    tfe = tfe[positions,:]
    # args[1] = 'first' if we want a contiguous selection of points
    if args[1] == 'first':
        if len(args) == 2:
            tfe = tfe[0:args[0],]
        if len(args) == 3:
            tfe = tfe[args[2]:(args[2] + args[0]),]
    # args[1] = 'random' if we want a random selection of measurements
    if args[1] == 'random':
        to_keep = np.random.permutation(tfe.shape[0])[:args[0]]
        tfe = tfe[to_keep,]
        positions = tfe[:,0].argsort()
        tfe = tfe[positions,:]
    # return a time ordered set of measurements
    return(tfe)

def identity(tfe,args):
    return(tfe)

def get_noisification_dict():
    noisification_dict = {'cadence_noisify':cadence_noisify,'identity':identity}
    return(noisification_dict)

if __name__ == '__main__':
    if 1:
        randoms = np.random.normal(size=(10,3))
        randoms_out = cadence_noisify(randoms,[5,'first',6])
        print randoms_out
        print randoms
    if 0:
        noisification_dict = get_noisification_dict()
        print noisification_dict
