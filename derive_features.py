##
## code for interfacing the database with TCP code
## for deriving L-S and other features which are
## ultimately used to classify light curves
##
## by James Long
## date Dec 13, 2010
##

import pdb
import os, sys
import warnings
import numpy as np
import create_database

os.environ.update({"TCP_DIR":"TCP/"})
print os.environ.get("TCP_DIR")
print os.path.abspath(os.environ.get("TCP_DIR") + 'Software/feature_extract')


warnings.simplefilter("ignore",DeprecationWarning) 

sys.path.append(os.path.abspath(os.environ.get("TCP_DIR") + 'Software/feature_extract'))
sys.path.append(os.path.abspath(os.environ.get("TCP_DIR") + 'Software/feature_extract/Code'))

from Code import *
import db_importer
import pprint


import glob
import re

sys.path.append(os.environ.get("TCP_DIR") + 'Software/feature_extract/MLData')
import arffify

##
## wrap get_features in a function that accepts a source_id and
## outputs a row in the features table, should delete any entry
## in features that is associated with source_id (by default)
## 
def enter_features(source_id,cursor,delete_existing=True):
    tfe = create_database.get_measurements(source_id,cursor)
    features_dict = get_features(tfe)
    features_dict = features_in_table(features_dict,cursor)
    features_dict[0].append('source_id')
    features_dict[1].append(source_id)

    # delete existing features for source if requested
    if delete_existing:
        sql_query = """DELETE FROM features WHERE source_id = """ + repr(source_id)
        cursor.execute(sql_query)
        print "removing derived features for: " + repr(source_id)

    # enter newly derived features for source
    sql_query = """INSERT INTO features(""" + ', '.join(features_dict[0]) + """) values (""" + ','.join(['?']*len(features_dict[0])) + """)"""
    cursor.execute(sql_query,features_dict[1])
    print "derived features for: " + repr(source_id)

# returns a dict, a subdict of features_dict containing
# only those keys in features_dict which are column
# names of the features table
def features_in_table(features_dict,cursor):
    sql_cmd = """PRAGMA table_info(features);"""
    cursor.execute(sql_cmd)
    db_info = cursor.fetchall()
    columns = []
    for i in db_info:
        columns.append(str(i[1]))
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
