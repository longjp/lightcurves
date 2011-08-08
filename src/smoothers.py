import numpy as np
from scipy import interpolate, randn
import scipy.interpolate
import matplotlib.pyplot as plt
import scipy.stats
import synthetic_data
## smooth a curve
import supsmu


## p246 in scipy tutorial for good
## info on this

## recommended for smoothing s=m-sqrt(2*m)
def spline(tfe,period,class_name="Unknown"):
    tfe[:,0] = tfe[:,0] % period
    positions = tfe[:,0].argsort()
    tfe = tfe[positions,:]
    ##smoothing = (tfe[:,0].size + np.sqrt(2*tfe[:,0].size))
    smoothing = pow(tfe[:,0].size,3/2)
    tck = interpolate.splrep(tfe[:,0],tfe[:,1],w=(1/tfe[:,2]),
                             s=smoothing,k=3,task=0)
    xs = np.linspace(0,period,1000)
    ys = interpolate.splev(xs,tck,der=0)
    plt.figure()
    plt.plot(xs,ys,tfe[:,0],tfe[:,1],'x')
    plt.show()

def supersmoother(tfe,period,class_name="Unknown"):
    tfe[:,0] = (tfe[:,0] % period) / period
    positions = tfe[:,0].argsort()
    tfe = tfe[positions,:]
    typevec = np.float32
    x = tfe[:,0].astype(typevec)
    y = tfe[:,1].astype(typevec)
    w = 1 / (tfe[:,2].astype(typevec))
    iper = np.array([2]).astype(np.int32)
    span = np.array([0.0]).astype(typevec)
    alpha = np.array([-1.0]).astype(typevec)
    smo = np.zeros(tfe.shape[0]).astype(typevec)
    sc = np.zeros(tfe.shape[0]*7).reshape((tfe.shape[0],7)).astype(typevec)
    supsmu.supsmu(x,y,w,iper,span,alpha,smo,sc)
    print "smo is:"
    print smo
    plt.figure()
    plt.title(class_name)
    plt.plot(tfe[:,0],smo,'o',tfe[:,0],tfe[:,1],'x')
    plt.show()


if __name__ == '__main__':
	aSurvey = synthetic_data.surveySetup()
	aSurvey.generateCurve()
	tfe = np.c_[aSurvey.times,aSurvey.fluxes,aSurvey.errors]
	period = aSurvey.period_this
	smoothing_test.supersmoother(tfe,period,aSurvey.class_name)	 
