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
    orig_out = sys.stdout 
    sys.stdout = dummyStream()
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
    sys.stdout = orig_out

## grab all filenames from folder of interest
## grab all expected resulting filenames
## keep non-duplicates
## parallelize work

def get_filenames(folder,extension):
    filepaths = glob.glob(("%s/*" + extension) % (folder))
    return map(lambda i:i.split('/')[-1],filepaths)
	    


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
        ## folder for input, output, and file extension
        in_folder = "../data/ogle-rr-i/"
        out_folder = "../data_processed/eclipse/"
        extension = ".dat"

	## get names of all files with extension in both folders
	in_names = get_filenames(in_folder,extension)
	out_names = get_filenames(out_folder,extension)

	## subset only those files we have not already analyzed
	in_names = set(in_names)
	out_names = set(out_names)
	names = list(in_names.symmetric_difference(out_names))
	
	## construct arguments and run
	args = map(lambda x: [in_folder + x, out_folder + x],names)
        p = Pool(2)
        p.map(derive_smoothed,args)
