##
## code for interfacing the database with TCP code
## for deriving L-S and other features which are
## ultimately used to classify light curves
##
## by James Long
## date Dec 13, 2010
## updated March 25, 2011
##


## TODO: change except OperationalError: to something else, these exceptions don't
## actually catch anything

import pdb
import os, sys
import warnings
import numpy as np
import create_database
import sqlite3
import noisification
from time import time

os.environ.update({"TCP_DIR":"TCP/"})
warnings.simplefilter("ignore",DeprecationWarning) 

sys.path.append(os.path.abspath(os.environ.get("TCP_DIR") + 'Software/feature_extract'))
sys.path.append(os.path.abspath(os.environ.get("TCP_DIR") + 'Software/feature_extract/Code'))
#print sys.path

#print "The dirs are printing"
#print dir()
from Code import *
#print dir()


import db_importer
import pprint


import glob
import re
from multiprocessing import Process, Value, Lock

sys.path.append(os.environ.get("TCP_DIR") + 'Software/feature_extract/MLData')
import arffify


######
###### to do:
###### 1. recode aquisition of tfes so that a process grabs all tfes for all
######    of its curves at once, rather than accessing the 20 or so times to get
######    tfe that match an original_source_id. the tfes can be sorted by 
######    original source id / python can split them into a list of tfes
######    NOTE: this can also solve problem of multiple searches for the same
######         set of tfes (as when we are noisifying the same curve several times)
######         just order source_info by original_source_id (column 2), then 
######         have the process grab the 20 tfe's it's currently working on
######         at each individuals tfe do the appropriate subset



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

def tolist(db_info):
 list1 = []
 for i in db_info:
  list1.append(i[0])
 return(list1)


####
#### things to change
#### 1. 
#### 2.
def derive_features_par(source_ids,noise_dict,cursor,connection,cadence_dict={},number_processors=1,delete_existing=True):
    features_columns = create_database.get_pragma(cursor)

    # obtain information about source_ids you are deriving features for
    # should the following two lines to avoid injection attacks 
    # but sqlite put max of 1000 on this
    #sql_cmd = """SELECT source_id, original_source_id, noisification, noise_args FROM sources WHERE source_id IN (""" + (len(source_ids) * "?,")[:-1] + ")"    
    #cursor.execute(sql_cmd,source_ids)
    # so instead we use "dangerous" form
    sql_cmd = """SELECT source_id, original_source_id, noisification, noise_args FROM sources WHERE source_id IN """ + repr(tuple(source_ids))     
    cursor.execute(sql_cmd)
    source_info = cursor.fetchall()

    # set up multiprocessing
    sourcenumber = Value('i',0)
    l = Lock()
    l1 = []
    for i in np.arange(number_processors):
        l1.append(Process(target=derive_features, args=(source_info, \
				cursor,connection,sourcenumber,l, \
				delete_existing,features_columns, \
			        noise_dict,cadence_dict)))
        l1[i].start()
    for i in np.arange(number_processors):
        l1[i].join()
    print "done extracting LS features"

def derive_features(source_info,cursor,connection,sourcenumber,l,delete_existing,features_columns,noise_dict,cadence_dict):
    # setup lists we will be using
    features_dicts = []
    the_ids = []

    while 1:
        # get a bunch of filepaths
        # increment the filepaths for other processes to get
        # don't let other processes touch this
        l.acquire()
        current_source_ids = range(sourcenumber.value,min(len(source_info) \
						,sourcenumber.value + 20))
        sourcenumber.value = min(sourcenumber.value + 20,len(source_info))
        l.release()
        
        # if there are no more sources for which to derive features  
        # write sources to db and then exit
        if len(current_source_ids) == 0:
            while 1:
                try:
                    l.acquire()
                    enter_features(features_dicts,the_ids,cursor, \
					   delete_existing)
                    connection.commit()
                    l.release()
                    features_dicts = False
                    break
                except OperationalError:
                   time.sleep(1)
                   pass
        if features_dicts == False:
            break

        ## what is our progress, about
        print "have grabbed up to about: " + repr(current_source_ids[0]) \
	    + " / " + repr(len(source_info))

	## determine whether to get smoothed fluxes or unsmoothed
	table_names = []
	for current_source in current_source_ids:
		if source_info[current_source][2].find('smoothed')==-1:
			table_names.append('measurements')
		else:
			table_names.append('measurements_smoothed')

	print table_names

        ## get tfes for all current_source_ids
        ## have to be careful not to double access the db
        tfes = []
        l.acquire()
        for i in range(len(current_source_ids)):
            time_begin = time()
            tfes.append(create_database.get_measurements( \
			    (source_info[current_source_ids[i]])[1],cursor,table=table_names[i]))
            time_end = time()
            print "tfe time is: " + repr(time_end - time_begin)
        l.release()

        # get features for the sources
        for i in range(len(tfes)):
            the_ids.append((source_info[current_source_ids[i]])[0])

	    # noisify the tfes
	    noisification_arguments = eval(source_info[current_source_ids[i]][3])
	    noisification_arguments.append(cadence_dict)
	    tfe_to_process = noise_dict[ (source_info[current_source_ids[i]])[2] ]  ( tfes[i],  noisification_arguments)

            # have TCP get features, but not print anything
            orig_out = sys.stdout 
            sys.stdout = dummyStream()
            time_start = time()

            raw_features = get_features(tfe_to_process)
            time_end = time()
            sys.stdout = orig_out
            print "time to derive features for this curve: " + repr(time_end - time_start)


            # change the features around a bit so they fit in db
            raw_features = features_in_table(raw_features,features_columns)
            raw_features[0].append('source_id')
            raw_features[1].append((source_info[current_source_ids[i]]) [0])
            features_dicts.append(raw_features)

        # try to write if more than 100 in queue
        if len(features_dicts) >= 100:
            try:
                l.acquire()
		print "====writing a batch of 100 to the disk===="
                enter_features(features_dicts,the_ids,cursor,delete_existing)
                connection.commit()
                l.release()
                features_dicts = []
                the_ids = []
            except OperationalError:
                pass



##
## write a bunch of features to features table
##
##
def enter_features(features_dicts,the_ids,cursor,delete_existing=True):
    for i in range(len(the_ids)):
        # delete existing features for source if requested
        if delete_existing:
            sql_query = """DELETE FROM features WHERE source_id = """ + repr(the_ids[i])
            cursor.execute(sql_query)

        # enter newly derived features for source
        sql_query = """INSERT INTO features(""" + ', '.join(features_dicts[i][0]) + """) values (""" + ','.join(['?']*len(features_dicts[i][0])) + """)"""
        cursor.execute(sql_query,features_dicts[i][1])

# returns a dict, a subdict of features_dict containing
# only those keys in features_dict which are column
# names of the features table
def features_in_table(features_dict,columns):
    for i in columns:
        if not(i in features_dict.keys()):
            columns.remove(i)
    feat_values = []
    for i in columns:
        feat_values.append(features_dict[i])    
    return([columns,feat_values])

def get_features(tfe):
    signals_list = []
    gen = generators_importers.from_xml(signals_list)
    pseudo_xml = xmlify(tfe)
    gen.generate(xml_handle = pseudo_xml)
    gen.sig.add_features_to_xml_string(gen.signals_list)
    return(gen.sig.x_sdict['features']['Clear:table64084'])


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




if __name__ == "__main__":
    # update the names in derived_features_list.txt file (need an entry in db to do this)
    # should probably change to tfes are randomly generated, don't need db at all
    if 0:
        # make connection
        connection = sqlite3.connect('../db/astronomy.db')
        cursor = connection.cursor()

        # get tfes for a source
        sql_query = """SELECT source_id FROM sources LIMIT 1"""
        cursor.execute(sql_query)
        db_info = cursor.fetchall()
	source_id = db_info[0][0]
	tfe = create_database.get_measurements(source_id,cursor)
	print tfe

	# derive features and print feature names to derived features file
        the_features = get_features(tfe)
	print the_features
	print the_features.keys()
	g = open('../db/derived_features_list.txt','w')
	for i in the_features.keys():
		g.write(i + '\n')


    if 1:
        # make connection
        connection = sqlite3.connect('../db/simulated_astronomy.db')
        cursor = connection.cursor()

        # get all source ids
        sql_query = """SELECT source_id FROM sources"""
        cursor.execute(sql_query)
        db_info = cursor.fetchall()
        j = []
        for i in db_info:
            j.append(i[0])

	j = j[0:5]
        # derive features with a single processor
        #begin_time_single = time() 
        #derive_features_par(j,cursor,connection,number_processors=1)
        #end_time_single = time()

        # derive features with 2 processors
	noise_dict = noisification.get_noisification_dict()
	begin_time_2 = time()   

	derive_features_par(j,noise_dict,cursor,connection,cadence_dict={},number_processors=2,delete_existing=True)
        end_time_2 = time()

        # what is the time difference?
        #print "single time: " + repr(end_time_single - begin_time_single)
        #print "2 time: " + repr(end_time_2 - begin_time_2)
