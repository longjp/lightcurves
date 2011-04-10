import numpy as np
import supsmu

print supsmu.__doc__
print supsmu.supsmu.__doc__
print supsmu.smooth.__doc__


typevec = np.float32
n = 100
x = np.linspace(start=0.0,stop=1.0,num=n).astype(typevec)
y = pow(x,2) + x + 4
w = np.ones(n).astype(typevec)
iper = np.array([1]).astype(np.int32)
span = np.array([.2]).astype(typevec)
alpha = np.array([0.0]).astype(typevec)
smo = np.zeros(n).astype(typevec)
sc = np.zeros(n*7).reshape((100,7)).astype(typevec)

supsmu.supsmu(x,y,w,iper,span,alpha,smo,sc)

# line 176 is assigning all smo values to 1, this is the
# problem. changing this value to something else results
# in printing another value
