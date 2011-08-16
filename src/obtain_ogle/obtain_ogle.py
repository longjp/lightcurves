########## 
########## 
########## OBTAIN TFE OF OGLE SOURCES FROM THE INTERNET 
##########   
##########
########## by James Long 
########## date: 8/15/2011 
########## 


## 1. go to: http://ogledb.astrouw.edu.pl/~ogle/CVS/
## 2. click on one of the meta classes on the left
## 3. select whatever class(es) you want
## 4. save file
### THIS PROGRAM EXTRACTS SOURCE_IDS FROM THAT FILE, GETS TFEs FOR SOURCE FROM
### INTERNET, AND THEN SAVES IN XML FILES

import urllib
import numpy as np

def get_source_ids(filename):
    g = open(filename,'r')
    source_ids = []
    lines = g.readlines()
    for i in lines:
        if i[0] == "#":
            continue
        source_ids.append(i.split('\t')[0])
    return(source_ids)

def get_classes(filename):
    g = open(filename,'r')
    classes = []
    lines = g.readlines()
    for i in lines:
        if i[0] == "#":
            continue
        classes.append(i.split('\t')[3])
    return(classes)


def get_v_band(source_id):
    url_beginning = """http://ogledb.astrouw.edu.pl/~ogle/CVS/data/V/"""
    url_end = """.dat"""
    url = url_beginning + source_id[-2:] + '/' + source_id + url_end
    print 'fetching: ' + url
    try:
        f = urllib.urlopen(url)
    except IOError:
        print "unable to open: " + url
        return(0)
    html = f.readlines()
    print html
    tfe = np.ndarray((len(html),3))
    try:
        for i in range(len(html)):
            tfe[i,:] = html[i][:-1].split()
    except ValueError:
        print "could not get tfes. exiting loop . . ."
        tfe = 0
    return(tfe)

def xmlify(tfe,source_class):
    xml = ""
    for i in range(np.shape(tfe)[0]):
        xml = xml + """<TR row='""" + repr(i+1) + """'><TD>""" + repr(tfe[i,0]) + """</TD><TD>""" + repr(tfe[i,1]) + """</TD><TD>""" + repr(tfe[i,2]) + """</TD></TR>\n"""
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
    middle = """            </TABLEDATA>
          </DATA>
        </TABLE>
      </Resource>
    </VOTimeseries>
  <Classifications>
    <Classification type="human">
      <source type="tcp">
        <link></link>
        <name>None</name>
        <version>1.0</version>
        <comments></comments>
      </source>
      <class name=" """

    end = """" dbname="tutor" prob="1.000000">
      </class>
    </Classification>
  </Classifications>
</VOSOURCE>"""
    xml = beginning + xml + middle[:-1] + source_class + end
    return(xml)

def xml_out(xml,filename):
    g = open(filename,'w')
    g.write(xml)
    g.close()

if __name__ == "__main__":
    filename = "mira.dat"
    source_ids = get_source_ids(filename)
    classes = get_classes(filename)
    ##print source_ids
    ##print classes
    failure = []
    for i in range(len(source_ids)):
        source_id = source_ids[i]
        source_class = classes[i]
        tfe = get_v_band(source_id)
        if isinstance(tfe,int):
            failure.append(source_id)
            continue
        xml = xmlify(tfe,source_class)
        xml_out(xml,'ogle/' + source_id + '.xml')
    print failure
    g = open('failures.txt','w')
    for i in failure:
        g.write(i + '\n')
    g.close()
