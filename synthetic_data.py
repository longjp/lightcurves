#####
##### generate some synthetic light curves
#####
##### by James Long
##### date Jan 31, 2011
#####

#### goals:
#### 1. create program that will generate light curves of different varieties, store them in db
#### 2. read up on pickling and oop for python and see if we can improve on 1.

## questions:
## 1. should I pack all arguments into a list
## 2. nobs doesn't always make sense

import numpy as np
import visualize


def poisson_process_cadence(nobs=100,rate=1,timeframe=False):
    cadence = np.random.exponential(rate,nobs)
    for i in range(len(cadence)-1):
        cadence[i+1] = cadence[i] + cadence[i+1]
    return cadence

def sinusoidal(cadence,period=np.pi,phase=0.,mag=1,mag_off=0,error=0):
    if error == 0:
        errors = np.zeros(cadence.size)
    else:
        errors = np.random.normal(loc=0,scale=error,size=cadence.size)
    magnitudes = mag*np.sin(2*np.pi*(((cadence - period*phase) % period)) / period) + mag_off + errors
    tfe = np.column_stack((cadence,magnitudes,errors))
    return tfe

def detached(cadence,period=np.pi,phase=0.,mag_off=0,error=0,depth1=.7,depth2=.8,flat_frac=.5):
    if error == 0:
        errors = np.zeros(cadence.size)
    else:
        errors = np.random.normal(loc=0,scale=error,size=cadence.size)
    
    # get the first depression
    in_first_depression = (((cadence - phase*period) % period) / period )  / ( (1 - flat_frac) / 2 ) < 1
    where_in_first_depression = -1*depth1*np.sin(np.pi*(((cadence - phase*period) % period) / period )  \
                                                     / ( (1 - flat_frac) / 2 ))
    first_depression = in_first_depression * where_in_first_depression

    # get the second depression
    in_second_depression = (((cadence - phase*period - period / 2) % period) / period )  / ( (1 - flat_frac) / 2 ) < 1
    where_in_second_depression = -1*depth2*np.sin(np.pi*(((cadence - phase*period - period / 2) % period) / period )  / ( (1 - flat_frac) / 2 ))
    second_depression = in_second_depression * where_in_second_depression

    magnitudes = mag_off + first_depression + second_depression + errors 
    tfe = np.column_stack((cadence,magnitudes,errors))
    return(tfe)


if __name__ == "__main__":
    # test sinusoidal
    if 0:
        period = 1000000.
        mag = 1.
        phase = -1. / 4.
        print phase
        cadence = poisson_process_cadence(200,10)
        tfe = sinusoidal(cadence,period=period,phase=phase,mag=mag,mag_off=17)
        visualize.plot_curve(tfe,freq= 1 / period)
    # test detached (perhaps with flat_frac = 0. this becomes attached)
    if 1:
        period = np.pi
        depth1 = 1.3
        depth2 = .8
        flat_frac = .0
        phase = .4
        cadence = poisson_process_cadence(nobs=2000,rate=10)
        tfe = detached(cadence,period=period,phase=phase,mag_off=0,error=depth1/10,depth1=depth1,depth2=depth2,flat_frac=flat_frac)
        visualize.plot_curve(tfe,freq= 1 / period)
        print tfe
