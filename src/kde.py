########## 
########## 
########## Experiments plotting pdfs using python 
##########  
##########
########## by James Long 
########## date: 4/7/2011 
########## 

import numpy as np
import scipy.stats
from matplotlib import pyplot as plt

##
## produce a few kdes
## args: a is a dict with keys as names of variables
## and values the realizations of r.v. to make kde from
##
def produceKDE(a):
    ## check that a satisfies requirements
    if type(a) != type({}):
        print "argument must be a dictionary"
        return(0)
    for i in a.values():
        if i.dtype != 'float32':
            print "each value in a must have dtype float32"
            return(0)

    ## make kdes
    gkde = []
    keys = a.keys()
    for i in keys:
        gkde.append(scipy.stats.gaussian_kde(a[i]))

    ## determine borders of display window
    mina = min(map(lambda array1:array1.min(),a.values()))
    maxa = max(map(lambda array1:array1.max(),a.values()))
    diff = (maxa - mina)*.1
    ind = np.linspace(mina - diff,maxa + diff,101)

    ## evaluate the kde at some points
    kdepdf = []
    for i in gkde:
        kdepdf.append(i.evaluate(ind))

    ## plot the kdes
    plt.figure()
    for i in range(len(kdepdf)):
        plt.plot(ind,kdepdf[i],label=keys[i])
    ll = plt.legend(loc='upper right')
    plt.show()


