###
### for deriving features without using a db
###


import derive_features
import numpy as np
import glob
import smoothers
import sys
#from matplotlib import pyplot as plt
from multiprocessing import Pool


##
## used so TCP won't print anywhere
##
class dummyStream:
	''' dummyStream behaves like a stream but does nothing. '''
	def __init__(self): pass
	def write(self,data): pass
	def read(self,data): pass
	def flush(self): pass
	def close(self): pass

def read_tfe(filename):
    tfe = np.fromfile(filename,sep=" ")
    tfe = tfe.reshape((tfe.size / 3, 3))
    return tfe

def write_features(features,filename):
    g = open(filename,'w')
    for i in features:
        g.write(i + " " + features[i] + "\n")
    g.close()


def compute_residuals(tfe,period):
    smo = smoothers.supersmooth(tfe,period,normalize_times=False)
    tfe[:,1] = np.mean(tfe[:,1]) + tfe[:,1] - smo

def derive_smoothed(args):
    print args
    file_in = args[0]
    file_out = args[1]
    tfe = read_tfe(file_in)
    features = derive_features.get_features(tfe)
    period = 1 / float(features["freq1_harmonics_freq_0"])
    # plt.close()
    # plt.figure(1)
    # plt.scatter(tfe[:,0] % period,tfe[:,1])
    # plt.savefig("lc_orig.pdf")
    compute_residuals(tfe,period)
    features = derive_features.get_features(tfe)
    period = 1 / float(features["freq1_harmonics_freq_0"])
    # plt.close()
    # plt.figure(1)
    # plt.scatter(tfe[:,0] % period,tfe[:,1])
    # plt.savefig("lc_folded.pdf")
    write_features(features,file_out)

## grab all filenames from folder of interest
## grab all expected resulting filenames
## keep non-duplicates
## parallelize work


if __name__ == "__main__":
    if 0:
        filename = "../data/ogle-rr-i/OGLE-BLG-RRLYR-02792.dat"
        tfe = np.fromfile(filename,sep=" ")
        tfe = tfe.reshape((tfe.size / 3, 3))
        features = derive_features.get_features(tfe)
        features.keys()
        features.values()
        filename2 = "test.dat"
        derive_smoothed(filename,filename2)

    
    if 1:
        folder = "../data/ogle-rr-i/"
        extension = ".dat"
        filepaths = glob.glob(("%s/*" + extension) % (folder))
        filepaths = filepaths[0:10]
        ##print filepaths
        names = map(lambda i:i.split('/')[-1],filepaths)
        out_folder = "../data_processed/eclipse/"
        outfilepaths = map(lambda x: out_folder + x,names)
        args = zip(filepaths,outfilepaths)
        ##print args
        p = Pool(2)
        p.map(derive_smoothed,args)
        #for i in filepaths:

        #     derive_smoothed(i,out_folder + output)
