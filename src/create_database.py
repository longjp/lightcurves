##
## code for setting up the database
##
## by James Long
## created Dec 7, 2010
## updated April 17, 2011
##

import sqlite3
import numpy as np
import xml_manip
import visualize
import noisification
import derive_features
import glob
from multiprocessing import Process, Value, Array, Lock
import time
import math


## TODO: consider implementing with dictionary, associating sources_pragma (keys)
##        with source_info (values)
def noisify_unsmoothed_sources(cursor,source_id,sources_pragma,
                               n_points=[10,20,30,40,50,60,70,80,90,100],
                               n_versions_first=1,n_versions_random=1):
    sources_pragma = sources_pragma[:]
    ## get source information
    sql_cmd = """SELECT """ + ', '.join(sources_pragma) +  """ FROM sources WHERE source_id=(?)"""
    cursor.execute(sql_cmd,[source_id])
    db_info = cursor.fetchall()
    source_info = list(db_info[0])

    ## change information that remains changed for all noisified
    ## entries in sources
    source_info[sources_pragma.index('original_source_id')] = source_info[sources_pragma.index('source_id')]
    source_info[sources_pragma.index('noisification')] = "cadence_noisify"
    n_points_original = source_info[sources_pragma.index('number_points')]

    print sources_pragma
    print source_info
    print n_points_original

    ## check to make sure that total number_points > max(n_points)
    if n_points_original < max(n_points):
        print "WARNING: requested noisified curves are longer than original curve"
        print "cancelling noisifying of sources . . ."
        return(0)

    ## get rid of source_id
    del source_info[sources_pragma.index('source_id')]
    del sources_pragma[sources_pragma.index('source_id')]	


    print sources_pragma
    print source_info
    print n_points_original


    ## add noisified sources entries
    for j in n_points:
        source_info[sources_pragma.index('number_points')] = j
        sql_cmd = assembleSQLCommand("sources",sources_pragma)
        ## create ordered sources
        for k in range(n_versions_first):
            if n_versions_first > 1:
                offset  = int(math.floor((float(n_points_original - j) / (n_versions_first - 1)) * k))
            else:
                offset = 0
            source_info[sources_pragma.index('noise_args')] =  "[" + repr(j) + ",'first'," + repr(offset) + "]"
            print source_info
            cursor.execute(sql_cmd, source_info)
        ## create random sources
        for k in range(n_versions_random):
            source_info[sources_pragma.index('noise_args')] = "[" + repr(j) + ",'random']"
            print source_info
            cursor.execute(sql_cmd, source_info) 



## TODO: consider implementing with dictionary, associating sources_pragma (keys)
##        with source_info (values)
def noisify_smoothed_sources(cursor,source_id,sources_pragma,
                             survey_dict={'hipparcos_train':'ogle','ogle_train':'hip'},
                             n_points=[10,20,30,40,50,60,70,80,90,100],
                             complete_curve=True,
                             n_versions_first=1,n_versions_random=1):
    sources_pragma = sources_pragma[:]
    ## get source information
    sql_cmd = """SELECT """ + ', '.join(sources_pragma) +  """ FROM sources WHERE source_id=(?)"""
    cursor.execute(sql_cmd,[source_id])
    db_info = cursor.fetchall()
    source_info = list(db_info[0])
    sql_cmd = """SELECT freq1_harmonics_freq_0 FROM features WHERE source_id=(?)"""
    cursor.execute(sql_cmd,[source_id])
    db_info = cursor.fetchall()
    ## actually twice the estimated period b/c this is what we are smoothing the curves on
    period = 2*(1/db_info[0][0])

    ## change information that remains changed for all noisified
    ## entries in sources
    source_info[sources_pragma.index('original_source_id')] = source_info[sources_pragma.index('source_id')]
    source_info[sources_pragma.index('noisification')] = "cadence_noisify_smoothed"
    n_points_original = source_info[sources_pragma.index('number_points')]
    survey = source_info[sources_pragma.index('survey')]
    noisified_cadence = survey_dict[survey]

    ## get rid of source_id
    del source_info[sources_pragma.index('source_id')]
    del sources_pragma[sources_pragma.index('source_id')]	

    ## add noisified sources entries
    for j in n_points:
        source_info[sources_pragma.index('number_points')] = j
        sql_cmd = assembleSQLCommand("sources",sources_pragma)
        ## create ordered sources
        for k in range(n_versions_first):
            source_info[sources_pragma.index('noise_args')] =  "['" + noisified_cadence + "','first'," + repr(j) + ',' + repr(period) + "]"
            print source_info
            cursor.execute(sql_cmd, source_info)
        ## create random sources
        for k in range(n_versions_random):
            source_info[sources_pragma.index('noise_args')] =  "['" + noisified_cadence + "','random'," + repr(j) + ',' + repr(period) + "]"
            print source_info
            cursor.execute(sql_cmd, source_info) 
    if complete_curve:
            source_info[sources_pragma.index('noise_args')] =  "['" + noisified_cadence + "','first','all'," + repr(period) + "]"
            print source_info
            cursor.execute(sql_cmd, source_info)
        




### not being used
def noisify_sources(cursor,source_info,column_names,noisification,
                    noise_args,number_points):
    ## make local copies
    source_info = source_info[:]
    column_names = column_names[:]
    ## check that arguments are consistent
    if(len(noisification) != len(noise_args) or 
       len(noise_args) != len(number_points)):
        print "lengths of arguments do not match"
        return(0)
    ## replace original_source_id with the source_id
    try:
        source_info[column_names.index('original_source_id')] = source_info[column_names.index('source_id')]
    except ValueError:
        print "no original_source_id or source_id"
        return(0)
    ## delete the source_id
    del source_info[column_names.index('source_id')]
    del column_names[column_names.index('source_id')]	
    ## make sql insertions
    for i in range(len(noisification)):
        source_info[column_names.index('noisification')] = noisification[i]
        source_info[column_names.index('noise_args')] = noise_args[i]
        source_info[column_names.index('number_points')] = number_points[i]
        sql_cmd = assembleSQLCommand("sources",column_names)
        cursor.execute(sql_cmd,source_info)


## return the column names of a table
def get_pragma(cursor,table="features"):
    """Return the column names of the features table as a list of strings."""
    sql_cmd = """PRAGMA table_info(""" + table + """);"""
    cursor.execute(sql_cmd)
    db_info = cursor.fetchall()
    columns = []
    for i in db_info:
        columns.append(str(i[1]))
    return(columns)

## input source_id, output time, flux, and error in an ndarray
def get_measurements(source_id,cursor,table='measurements'):
    """ Return tfe for a particular source_id,cursor """
    sql_cmd = "SELECT time, flux, error FROM " + table + " WHERE source_id = (?)"
    cursor.execute(sql_cmd,[source_id])
    db_info = cursor.fetchall()
    tfe = np.array(db_info)
    return(tfe)

## puts time, flux, and flux_err into measurements table
## ideally this table should have a band column for what 
## filter it was observed in
def insert_measurements(cursor,last_id,measurements,table='measurements'):
    for row in measurements:
        sql_cmd = """INSERT INTO """ + table + """(time,flux,error,source_id) values (?,?,?,?)"""
        row_list = row.tolist()
        row_list.append(last_id)
        cursor.execute(sql_cmd,row_list)

## puts together an sql insert query - specify table and columns to enter
def assembleSQLCommand(table_name,curve_info_names):
    sql_cmd = """insert into """ + table_name +  """ (""" + ','.join(curve_info_names) + """) values (""" + ('?,' * len(curve_info_names))[:-1] + """)"""
    return(sql_cmd)

## loads data into sources and measurements, used by ingest_xml
def enter_record(curve_info,curve_info_names,tfe,cursor):
    sql_cmd = assembleSQLCommand("sources",curve_info_names)
    cursor.execute(sql_cmd, curve_info)

    ## find last insert number
    cursor.execute("""SELECT last_insert_rowid()""")
    last_id = cursor.fetchall()[0][0]

    ## set original_source_id to source_id since 
    ## these are all original sources
    sql_cmd = """UPDATE sources SET original_source_id=(?) WHERE source_id=(?)"""
    cursor.execute(sql_cmd,(last_id,last_id))

    ## now insert measurement data
    insert_measurements(cursor,last_id,tfe)

## wraps enter_record, insert a lot of records
def enter_records(all_curves,all_curves_info,tfes,cursor,connection):
    for i in range(len(all_curves)):
        enter_record(all_curves[i],all_curves_info,tfes[i],cursor)


## puts a new record in the table sources and fills in the table measurements
## may want to work on speeding up computation between mark 1 and mark 2
## additional args ,survey='',original_number=False
def ingest_xml(filepaths,cursor,connection,
               filenumber,l,survey):
    # setup lists we will be using
    all_curves = []
    tfes = []

    while 1:
        ## get a bunch of filepaths, 
        ## increment the filepaths for other processes to get
        l.acquire()
        current_filenumbers = range(filenumber.value,
                                    min(len(filepaths),
                                        filenumber.value + 20))
        filenumber.value = min(filenumber.value + 20,len(filepaths))
        l.release()

        ## if nothing to grab, writes remaining data to database and exits
        if len(current_filenumbers) == 0:
            while 1:
                l.acquire()
                all_curve_info = ["number_points", 
                                  "classification", "c1", "e1",
                                  "c2", "e2", "raw_xml","survey",
                                  "xml_filename"]
                enter_records(all_curves,all_curve_info,tfes,
                              cursor,connection)
                connection.commit()
                l.release()
                all_curves = False
                break
        if all_curves == False:
            break

        # get data for all current_filenumbers, will enter records into db if
        # not being used and queue is getting large ( > 100)
        for current_filenumber in current_filenumbers:
            filepath = filepaths[current_filenumber]
            f = open(filepath,'r')
            xml = f.read()
            f.close()
            # get information from xml and put it into sources table
            xml_filename = filepath.split('/').pop()
            # begin1 = time()
            # mark 1
            curve_info = xml_manip.get_info(xml)
            # mark 2
            # end1 = time()
            # print "getting data from xml time is: " + repr(end1 - begin1)
            curve_info[0].append(xml)
            curve_info[0].append(survey)
            curve_info[0].append(xml_filename)
            all_curves.append(curve_info[0])
            tfes.append(curve_info[1])
            #sql_cmd = """SELECT datetime('now')"""
            #cursor.execute(sql_cmd)
            #db_info = cursor.fetchall()
            #curve_info[0].append(db_info[0][0])

            # try to enter info in db, if being used just keep going
            if(len(all_curves) > 100):
                l.acquire()
                all_curve_info = ["number_points", 
                                  "classification", "c1", "e1",
                                  "c2", "e2", "raw_xml","survey",
                                  "xml_filename"]
                enter_records(all_curves,all_curve_info,tfes,
                              cursor,connection)
                connection.commit()
                l.release()
                all_curves = []
                tfes = []
            print "successfully ingested: " + filepath
        print (repr(max(current_filenumbers) + 1) +
               " / " + repr(len(filepaths)))

# for inserting all .xml files in a folder, wraps ingest_xml function
def ingest_many_xml(folder,cursor,connection,
                    survey='',number_processors=1):
    filepaths = glob.glob("%s/*xml" % (folder))

    # info about the injest
    print "%s/*xml" % (folder)
    print "ingesting " + repr(len(filepaths)) + " sources . . ."

    ## set up multiprocessing
    filenumber = Value('i',0)
    l = Lock()
    l1 = []
    for i in np.arange(number_processors):
        l1.append(Process(target=ingest_xml,args=(filepaths, \
                      cursor,connection,filenumber,l,survey)))
        l1[i].start()
    for i in np.arange(number_processors):
        l1[i].join()
    connection.commit()
    
# creates table sources and table measurements if they do not exist
# deletes all records if REMOVE_RECORDS=TRUE
# should we get rid of n_points in sources file?
def create_db(cursor,features_file=False,REMOVE_RECORDS=False):
        sql_cmd = """CREATE TABLE IF NOT EXISTS sources (source_id INTEGER PRIMARY KEY AUTOINCREMENT, original_source_id INTEGER, noisification TEXT DEFAULT 'identity',noise_args TEXT DEFAULT "[]",number_points INTEGER, date TEXT,classification TEXT,survey TEXT,true_period REAL DEFAULT NULL,c1 REAL,e1 REAL,c2 REAL,e2 REAL,xml_filename TEXT,raw_xml TEXT);"""
        cursor.execute(sql_cmd)        
        sql_cmd = """CREATE TABLE IF NOT EXISTS measurements (measurements_id INTEGER PRIMARY KEY AUTOINCREMENT, time REAL, flux REAL, error REAL, source_id INTEGER, FOREIGN KEY(source_id) REFERENCES sources(source_id));"""
        cursor.execute(sql_cmd)
        sql_cmd = """CREATE TABLE IF NOT EXISTS measurements_smoothed (measurements_id INTEGER PRIMARY KEY AUTOINCREMENT, time REAL, flux REAL, error REAL, source_id INTEGER, FOREIGN KEY(source_id) REFERENCES sources(source_id));"""
        cursor.execute(sql_cmd)
        if features_file == False:
            ## could change this so automatically generates features file
            print "Could not create / check existence of features table because no filename for features names was provided"

        else:
            try:
                f = open(features_file,'r')
                features = f.readlines()
            except IOError:
                print "Invalid Features file"
                return 0
            features_string = ""
            for i in features:
                features_string = features_string + ',' + i[:-1] + ' ' + 'REAL'
            features_string = """CREATE TABLE IF NOT EXISTS features (source_id""" + features_string + ",FOREIGN KEY(source_id) REFERENCES sources(source_id));"
            cursor.execute(features_string)
        ## should probably build some check here, ask person first b/c this could destroy a lot of data
        if(REMOVE_RECORDS):
            sql_cmd = """DELETE FROM sources"""
            cursor.execute(sql_cmd)
            sql_cmd = """DELETE FROM measurements"""
            cursor.execute(sql_cmd)
            sql_cmd = """DELETE FROM features"""
            cursor.execute(sql_cmd)
