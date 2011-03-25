##
## code for setting up the astronomy.db database
##
## by James Long
## created Dec 7, 2010
##

import sqlite3
import numpy as np
import xml_manip
import visualize
import noisification
import derive_features
import glob
from multiprocessing import Process, Value, Array, Lock
from time import time

###
### notes / improvements
###
##
## 1. enter record like functions should accept name of columns so that 
##   function with different amounts of data can call them
##


# input source_id, output time, flux, and error in an ndarray
def get_measurements(source_id,cursor):
    sql_cmd = "SELECT time, flux, error FROM measurements WHERE source_id = (?)"
    cursor.execute(sql_cmd,[source_id])
    db_info = cursor.fetchall()
    tfe = np.array(db_info)
    return(tfe)

# puts time, flux, and flux_err into measurements table
# ideally this table should have a band column for what 
# filter it was observed in
def insert_measurements(cursor,last_id,measurements):
    for row in measurements:
        sql_cmd = """INSERT INTO measurements(time,flux,error,source_id) values (?,?,?,?)"""
        row_list = row.tolist()
        row_list.append(last_id)
        cursor.execute(sql_cmd,row_list)

# puts together an sql insert query - specify table and columns to enter
def assembleSQLCommand(table_name,curve_info_names):
    sql_cmd = """insert into """ + table_name +  """(""" + ','.join(curve_info_names) + """) values (""" + ('?,' * len(curve_info_names))[:-1] + """)"""
    return(sql_cmd)

# loads data into sources and measurements, used by ingest_xml
def enter_record(curve_info,curve_info_names,tfe,cursor,original_number=False):
    # earlier we used line below, all references to this function that
    # haven't been changed should have these names put in argument
    # curve_info_names
    #sql_cmd = """insert into sources(number_points, classification, c1, e1, c2, e2, raw_xml,survey,xml_filename,date) values (?,?,?,?,?,?,?,?,?,?)"""
    sql_cmd = assembleSQLCommand("sources",curve_info_names)
    cursor.execute(sql_cmd, curve_info)

    # find last insert number
    cursor.execute("""SELECT last_insert_rowid()""")
    last_id = cursor.fetchall()[0][0]

    # if curve is an original, assign is original_source_id = source_id, otherwise
    # make original_source_id <- original_source
    if not original_number:
        sql_cmd = """update sources set original_source_id=(?) where source_id = """ + repr(last_id)
    else:
        sql_cmd = """update sources set original_source_id=(?) where source_id = """ + repr(original_number)
    cursor.execute(sql_cmd,[last_id])

    # now insert measurement data
    insert_measurements(cursor,last_id,tfe)


def enter_records(all_curves,tfes,cursor,connection,original_number=False):
    for i in range(len(all_curves)):
        enter_record(all_curves[i],tfes[i],cursor,original_number=original_number)
    connection.commit()


# puts a new record in the table sources and fills in the table measurements
# may want to work on speeding up computation between mark 1 and mark 2
# additional args ,survey='',original_number=False
def ingest_xml(filepaths,cursor,connection,filenumber,l,survey,original_number):

    # setup lists we will be using
    all_curves = []
    tfes = []

    while 1:
        # get a bunch of filepaths, increment the filepaths for other processes to get
        l.acquire()
        current_filenumbers = range(filenumber.value,min(len(filepaths),filenumber.value + 20))
        filenumber.value = min(filenumber.value + 20,len(filepaths))
        l.release()

        # if nothing to grab, writes remaining data to database and exits
        if len(current_filenumbers) == 0:            
            while 1:
                try:
                    l.acquire()
                    enter_records(all_curves,tfes,cursor,connection,original_number=original_number)
                    l.release()
                    all_curves = False
                    break
                except OperationalError:
                    time.sleep(1)
                    pass
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
            sql_cmd = """SELECT datetime('now')"""
            cursor.execute(sql_cmd)
            db_info = cursor.fetchall()
            curve_info[0].append(db_info[0][0])

            # try to enter info in db, if being used just keep going
            if(len(all_curves) > 100):
                l.acquire()
                enter_records(all_curves,tfes,cursor,connection, \
                                  original_number=original_number)
                l.release()
                all_curves = []
                tfes = []
            print "successfully got info from: " + filepath
        print repr(max(current_filenumbers) + 1) + " / " + repr(len(filepaths))



# for inserting all .xml files in a folder, wraps ingest_xml function
def ingest_many_xml(folder,cursor,connection,survey='', \
                        original_number=False,number_processors=1):
    filepaths = glob.glob("%s/*xml" % (folder))

    # info about the injest
    print "%s/*xml" % (folder)
    print "ingesting " + repr(len(filepaths)) + " sources . . ."

    # set up multiprocessing
    filenumber = Value('i',0)
    l = Lock()
    l1 = []
    for i in np.arange(number_processors):
        l1.append(Process(target=ingest_xml,args=(filepaths, \
                      cursor,connection,filenumber,l,survey,original_number)))
        l1[i].start()
    for i in np.arange(number_processors):
        l1[i].join()

# creates table sources and table measurements if they do not exist
# deletes all records if REMOVE_RECORDS=TRUE
def create_db(cursor,features_file=False,REMOVE_RECORDS=False):
        sql_cmd = """CREATE TABLE IF NOT EXISTS sources (source_id INTEGER PRIMARY KEY AUTOINCREMENT, original_source_id INTEGER, noisification TEXT DEFAULT NULL,noise_args TEXT DEFAULT NULL,number_points INTEGER, date TEXT,classification TEXT,survey TEXT,true_period REAL DEFAULT NULL,c1 REAL,e1 REAL,c2 REAL,e2 REAL,xml_filename TEXT,raw_xml TEXT);"""
        cursor.execute(sql_cmd)        
        sql_cmd = """CREATE TABLE IF NOT EXISTS measurements (measurements_id INTEGER PRIMARY KEY AUTOINCREMENT, time REAL, flux REAL, error REAL, source_id INTEGER, FOREIGN KEY(source_id) REFERENCES sources(source_id));"""
        cursor.execute(sql_cmd)
        if features_file == False:
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
        # should probably build some check here, ask person first b/c this could destroy a lot of data
        if(REMOVE_RECORDS):
            sql_cmd = """DELETE FROM sources"""
            cursor.execute(sql_cmd)
            sql_cmd = """DELETE FROM measurements"""
            cursor.execute(sql_cmd)
            sql_cmd = """DELETE FROM features"""
            cursor.execute(sql_cmd)


if __name__ == "__main__":
    # test of ingest_many_xml function, wrapper to ingest_xml
    if 0:
        features_file = "derived_features_list.txt"
        folder = "test"
        connection = sqlite3.connect('astronomy.db')
        cursor = connection.cursor()

        # make sure sources table exists
        create_db(cursor,features_file=features_file,REMOVE_RECORDS=False)
        connection.commit()

        ingest_many_xml(folder,cursor,survey="debosscher",original_number=False)

    if 0:
        # set up database connection
        features_file = "derived_features_list.txt"
        connection = sqlite3.connect('astronomy.db')
        cursor = connection.cursor()
        
        # make sure sources table exists
        create_db(cursor,features_file=features_file,REMOVE_RECORDS=True)
        connection.commit()

        # injest a few sources
        practice_injests(cursor)
        connection.commit()

        # display only some of the record
        sql_cmd = """SELECT source_id,original_source_id,noisification,classification,number_points,survey,date,xml_filename FROM sources LIMIT 10"""        
        cursor.execute(sql_cmd)
        db_info = cursor.fetchall()
        print db_info

        # get some info on the measurements table
        sql_cmd = """SELECT source_id,count(*) FROM measurements GROUP BY source_id LIMIT 10"""
        cursor.execute(sql_cmd)
        db_info = cursor.fetchall()
        print db_info

        # save everything and close connection
        connection.commit()
        cursor.close()

    # test noisification methods
    if 0:
        # setup connection
        connection = sqlite3.connect('astronomy.db')
        cursor = connection.cursor()

        #noisfy some curve
        sql_cmd = """SELECT source_id FROM sources LIMIT 2"""        
        cursor.execute(sql_cmd)
        db_info = cursor.fetchall()
        source_id = db_info[0][0]
        noisification.sigma_noisification(cursor,source_id)

        # display only some of the record
        sql_cmd = """SELECT source_id,original_source_id,noisification,classification,number_points,survey,date,xml_filename FROM sources LIMIT 10"""        
        cursor.execute(sql_cmd)
        db_info = cursor.fetchall()
        print db_info


    # for testing how to derive features
    if 0:
        # connect to db
        connection = sqlite3.connect('astronomy.db')
        cursor = connection.cursor()

        # get a source_id - doesn't really matter from which source
        sql_cmd = """SELECT source_id FROM sources LIMIT 2"""        
        cursor.execute(sql_cmd)
        db_info = cursor.fetchall()
        source_id = db_info[1][0]

        # create record in features for this source with derived features
        derive_features.enter_features(source_id,cursor)

        # display only some of the record
        sql_cmd = """SELECT source_id,freq1_harmonics_freq_0 FROM features WHERE source_id =""" + repr(source_id) + """LIMIT 10"""
        cursor.execute(sql_cmd)
        db_info = cursor.fetchall()
        print db_info



        connection.commit()
        cursor.close()

