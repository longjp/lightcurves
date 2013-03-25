import numpy as np
import glob
from bs4 import BeautifulSoup
from time import time
import re

# accepts xml, returns list of [number_points,class]
def get_info(xml):
       info = []
       #time1 = time()
       soup = BeautifulSoup(xml,"xml")
       #time2 = time()
       #print "soup time is: " + repr(time2 - time1)
       # get the attributes

       #time1 = time()
       classification = get_class(soup)
       measurements = get_time_flux_error(soup)
       num_points = np.shape(measurements)[0]
       position = get_position(soup)
       time_flux_error = get_time_flux_error(soup)
       #time2 = time()
       #print "getting data time is: " + repr(time2 - time1)

       # load the attributes into info
       info.append(num_points)
       info.append(classification)
       info.extend(position)
       return([info,time_flux_error])

# accepts xml soup, outputs location of object and errors
def get_position(soup):
       c1 = "NA"
       c2 = "NA"
       e1 = "NA"
       e2 = "NA"
       # c1 = float(soup.position2d.value2.c1.string)
       # c2 = float(soup.position2d.value2.c2.string)
       # e1 = float(soup.position2d.error2.c1.string)
       # e2 = float(soup.position2d.error2.c2.string)
       position = [c1,e1,c2,e2]
       return position

# accepts xml soup, outputs the best classification
def get_class(soup):
       ## match upper and lower case class
       classes = soup.findAll('Classification')
       classes.extend(soup.findAll('classification'))
       class1classes = classes[0].findAll('class')
       class_level = np.ndarray(shape=(len(class1classes)))
       for i in range(len(class1classes)):
           class_level[i] = class1classes[i]['dbname'].count(':')
       index_max = np.argmax(class_level)
       classification = class1classes[index_max]['name']
       return(classification)

# accepts xml soup, outputs ndarray of time, flux, flux error measurements
def get_time_flux_error(soup):
    tabledata = soup.findAll('TR')
    measurements = np.ones(shape=(len(tabledata),3))
    for i in range(len(tabledata)):
        the_tds = tabledata[i].findAll('TD')
        measurements[i,] = float(the_tds[0].string), float(the_tds[1].string), float(the_tds[2].string)
    return measurements

# enter a set of filepaths with xml, get a dictionary with keys as classes
# and values as number of representatives from that class
def get_classes(filepaths):
    class_dict = {}
    for i in filepaths:
        print i
        f = open(i,'r')
        xml = f.read()
        f.close()
        soup = BeautifulSoup(xml,'xml')
        classification = get_class(soup)
        class_dict[classification] = class_dict.get(classification,0) + 1
    return(class_dict)


def get_tfe(xml):
       measurements = re.compile(r"""<TR row=['"][0-9]+['"]><TD>([0-9\.]+)</TD><TD>([0-9\.]+)</TD><TD>([0-9\.]+)</TD>""")
       text = measurements.findall(xml)
       tfe = np.array(text,dtype=np.float64)
       return(tfe)



if __name__ == "__main__":
    if 1:
        xml_dirpath = "../data/debosscher"
        filepaths = glob.glob("%s/*xml" % (xml_dirpath))
        filepaths = filepaths[4]
        f = open(filepaths,'r')
        xml = f.read()
        tfes = get_tfe(xml)
        print tfes
        print tfes.size / 3
    if 0:
        import re
        xml_dirpath = "../data/asas_full"
        filepaths = glob.glob("%s/*xml" % (xml_dirpath))
        min_points = 10000
        ##filepaths = filepaths[0:3]
        tr_match = re.compile(r'<TR row="[\d]+">')
        points = []
        for i in filepaths:
            print i
            f = open(i,'r')
            xml = f.read()
            f.close()
            a = tr_match.findall(xml)
            points.append(len(a))
        points = np.array(points)
        print np.mean(points)
        print np.min(points)
        print np.max(points)
        print sum(points < 100)

    if 0:
        xml_dirpath = "/home/james/Desktop/python-class/Python-Class-Final-Project/ASAS/asas_sources"
        filepaths = glob.glob("%s/*xml" % (xml_dirpath))
        # take a random subset to speed up processing
        filepaths = filepaths[1:2]
        for i in filepaths:
            f = open(i,'r')
            xml = f.read()
            f.close()
            soup = BeautifulSoup(xml,'xml')
            output = get_time_flux_error(soup)
            classification = get_class(soup)
        print "this is the output"
        print output
        print classification
        #print class_dict.keys()
        #print class_dict

    if 0:
        xml_dirpath = "/home/james/Desktop/python-class/Python-Class-Final-Project/ASAS/asas_sources"
        filepaths = glob.glob("%s/*xml" % (xml_dirpath))
        # take a random subset to speed up processing
        filepaths = filepaths[1:200]
        class_dict = get_classes(filepaths)
        print class_dict.keys()
        print class_dict

    if 0:
        filepath = "/home/james/Desktop/python-class/Python-Class-Final-Project/ASAS/asas_sources/155353.xml"
        f = open(filepath,'r')
        xml = f.read()
        soup = BeautifulSoup(xml,'xml')

        classes = soup.findAll('classification')
        print classes
        print len(classes)
        print classes[0]

        class1classes = classes[0].findAll('class')
        class_level = np.ndarray(shape=(len(class1classes)))
        for i in range(len(class1classes)):
            print class1classes[i]['dbname']
            class_level[i] = class1classes[i]['dbname'].count(':')
        print class_level
        print np.argmax(class_level)
        index_max = np.argmax(class_level)
        classification = class1classes[index_max]['name']
        print classification


















