import synthetic_data
import visualize
import numpy as np


reload(synthetic_data)
reload(visualize)
reload(np)

cadence = synthetic_data.poisson_process_cadence(nobs=100,rate=1,timeframe=False)
sinusoidal = synthetic_data.sinusoidal(cadence,period=np.pi,phase=0.,mag=1,mag_off=0,error=.1)


detached = synthetic_data.detached(cadence,period=np.pi,phase=0.,mag_off=0,error=.1,depth1=.7,depth2=.8,flat_frac=.5)

visualize.plot_curve(sinusoidal,freq=1/np.pi,plot_folded=True,plot_unfolded=True,classification='Sinusoidal')

visualize.plot_curve(detached,freq=1/np.pi,plot_folded=True,plot_unfolded=True,classification='Detached')



