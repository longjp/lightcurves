###
### for deriving features without using a db
###


import derive_features
import numpy as np
import glob
import smoothers
from matplotlib import pyplot as plt

filename = "../data/ogle-rr-i/OGLE-BLG-RRLYR-02792.dat"
tfe = np.fromfile(filename,sep=" ")
tfe = tfe.reshape((tfe.size / 3, 3))
features = derive_features.get_features(tfe)
features.keys()
features.values()
filename2 = "test.dat"


derive_smoothed(filename,filename2)


filenames
folder = "../data/ogle-rr-i/"
extension = ".dat"
filepaths = glob.glob(("%s/*" + extension) % (folder))


for i in filepaths:
    tfe = read_tfe(i)
    print i




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

def derive_smoothed(file_in,file_out):
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
