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

#x = np.linspace(mina,maxa)
#h = plt.plot(x, rv.pdf(x))


def produceKDE(a):
    gkde=scipy.stats.gaussian_kde(a)
    mina = a.min()
    maxa = a.max()
    diff = (maxa - mina)*.1
    ind = np.linspace(mina - diff,maxa + diff,101)
    kdepdf = gkde.evaluate(ind)
    plt.figure()
    plt.plot(ind, kdepdf, label='kde', color="g")
    plt.show()


if __name__ == '__main__':
    if 1:
        rv = scipy.stats.pareto(3,loc=0,scale=20)
        a = rv.rvs(1000)
        produceKDE(a)
