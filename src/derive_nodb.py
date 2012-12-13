###
### for deriving features without using a db
###


import numpy as np
import glob
import smoothers
import sys
from multiprocessing import Pool
import os
import warnings


os.environ.update({"TCP_DIR":"TCP/"})
warnings.simplefilter("ignore",DeprecationWarning) 
sys.path.append(os.path.abspath(os.environ.get("TCP_DIR") + 'Software/feature_extract'))
sys.path.append(os.path.abspath(os.environ.get("TCP_DIR") + 'Software/feature_extract/Code'))
from Code import *
import db_importer
import pprint
sys.path.append(os.environ.get("TCP_DIR") + 'Software/feature_extract/MLData')
import arffify










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


def get_filenames(folder,extension):
    filepaths = glob.glob(("%s/*" + extension) % (folder))
    return map(lambda i:i.split('/')[-1],filepaths)


def get_features(tfe):
    signals_list = []
    gen = generators_importers.from_xml(signals_list)
    pseudo_xml = xmlify(tfe)
    gen.generate(xml_handle = pseudo_xml)
    gen.sig.add_features_to_xml_string(gen.signals_list)
    return gen.sig.x_sdict['features']['Clear:table64084']


def xmlify(tfe):
    #print tfe[0]
    #print np.arange(tfe[0])
    #for row,contents in range
    xml = ""
    for i in range(np.shape(tfe)[0]):
        xml = xml + """<TR row='""" + repr(i) + """'><TD>""" + repr(tfe[i,0]) + """</TD><TD>""" + repr(tfe[i,1]) + """</TD><TD>""" + repr(tfe[i,2]) + """</TD></TR>\n"""
    complete_xml = wrap_xml(xml)
    return(complete_xml)


def wrap_xml(xml):
    beginning = """<?xml version="1.0"?>
<VOSOURCE version="0.04">
	<COOSYS ID="J2000" equinox="J2000." epoch="J2000." system="eq_FK5"/>
  <history>
    <created datetime="2010-09-13 02:23:56.140367" codebase="db_importer.pyc" codebase_version="9-Aug-2007"/>
  </history>
  <ID>148010</ID>
  <WhereWhen>
    <Description>Best positional information of the source</Description>
    <Position2D unit="deg">
      <Value2>
        <c1>0.1818096</c1>
        <c2>45.2533336</c2>
      </Value2>
      <Error2>
        <c1>0.001</c1>
        <c2>0.001</c2>
      </Error2>
    </Position2D>
  </WhereWhen>
  <VOTimeseries version="0.04">
    <TIMESYS>
			<TimeType ucd="frame.time.system?">MJD</TimeType> 
			<TimeZero ucd="frame.time.zero">0.0 </TimeZero>
			<TimeSystem ucd="frame.time.scale">UTC</TimeSystem> 
			<TimeRefPos ucd="pos;frame.time">TOPOCENTER</TimeRefPos>
		</TIMESYS>

    <Resource name="db photometry">
      <TABLE name="Clear:table64084">
        <FIELD name="t" ID="col1" system="TIMESYS" datatype="float" unit="day"/>
        <FIELD name="m" ID="col2" ucd="phot.mag;em.opt.Clear:table64084" datatype="float" unit="mag"/>
        <FIELD name="m_err" ID="col3" ucd="stat.error;phot.mag;em.opt.Clear:table64084" datatype="float" unit="mag"/>
        <DATA>
          <TABLEDATA>"""
    end = """            </TABLEDATA>
          </DATA>
        </TABLE>
      </Resource>
    </VOTimeseries>
</VOSOURCE>"""
    return(beginning + xml + end)



def derive_smoothed(args):
    #print args
    file_in = args[0]
    file_out_orig = args[1]
    file_out_residual = args[2]
    tfe = read_tfe(file_in)
    orig_out = sys.stdout 
    sys.stdout = dummyStream()
    features = get_features(tfe)
    write_features(features,file_out_orig)
    period = 1 / float(features["freq1_harmonics_freq_0"])
    # # plt.close()
    # # plt.figure(1)
    # # plt.scatter(tfe[:,0] % period,tfe[:,1])
    # # plt.savefig("lc_orig.pdf")
    compute_residuals(tfe,period)
    features = get_features(tfe)
    # period = 1 / float(features["freq1_harmonics_freq_0"])
    # # plt.close()
    # # plt.figure(1)
    # # plt.scatter(tfe[:,0] % period,tfe[:,1])
    # # plt.savefig("lc_folded.pdf")
    write_features(features,file_out_residual)
    sys.stdout = orig_out


if __name__ == "__main__":
    if 0:
        filename = "../data/ogle-rr-i/OGLE-BLG-RRLYR-02792.dat"
        tfe = np.fromfile(filename,sep=" ")
        tfe = tfe.reshape((tfe.size / 3, 3))
        features = get_features(tfe)
        features.keys()
        features.values()
        filename2 = "test.dat"
        derive_smoothed(filename,filename2)

    
    if 1:
        ## folder for input, output, and file extension
        in_folder = "../data/asas_tfe/"
	out_orig = "../data_processed/asas_orig/"
        out_residual = "../data_processed/asas_residual/"
        extension = ".dat"

	## get names of all files with extension in both folders
	in_names = get_filenames(in_folder,extension)
	out_names = get_filenames(out_orig,extension)
	out_names2 = get_filenames(out_residual,extension)

	## subset only those files not in both out_orig and out_residual
	in_names = set(in_names)
	out_names = set(out_names)
	out_names2 = set(out_names2)
	names = list(in_names - (out_names & out_names2))
	
	## construct arguments and run
	args = map(lambda x: [in_folder + x, out_orig + x, out_residual + x],
		   names)
        p = Pool(20)
        p.map(derive_smoothed,args)
