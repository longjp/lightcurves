#####
##### contains functions to output db info to files that could be used
##### by R or VOsource programs
#####
##### by James Long
##### date Jan 11, 2011
#####


#####
##### tfeOutput needs to be corrected
#####

import sqlite3
import numpy as np


## input an ndarray of source ids, output a file where 
## the first line is names of features, and each additional
## line is value of those features for particular source
def tfeOutput(source_ids,cursor,filename):
    # convert source_ids to integers
    j = 0
    for i in source_ids:
        source_ids[j] = repr(i)
        j += 1

    # get column names from measurements table
    sql_cmd = """PRAGMA table_info(measurements);"""
    cursor.execute(sql_cmd)
    pragma = cursor.fetchall()
    columns_to_get = []
    for i in pragma:
        columns_to_get.append(i[1])


    # get desired rows in features and sources table
    rows_to_get = '(' + ','.join(source_ids) + ')'
    sql_cmd = """SELECT * FROM measurements WHERE source_id IN """ + rows_to_get
    cursor.execute(sql_cmd)
    db_info = cursor.fetchall()

    # now write to file
    column_names = '; '.join(columns_to_get)
    g = open(filename,'w')
    g.write(column_names + '\n')
    for i in db_info:
        output1 = ''
        for j in i:
            output1 += str(j) + '; '
        output1 = output1[:-2]
        g.write(output1 + '\n')
    g.close()



## input an ndarray of source ids, output a file where 
## the first line is names of features, and each additional
## line is value of those features for particular source
def outputRfile(source_ids,cursor,filename):
    # convert source_ids to integers
    j = 0
    for i in source_ids:
        source_ids[j] = repr(i)
        j += 1

    print source_ids
    
    # get column names from features table
    sql_cmd = """PRAGMA table_info(features);"""
    cursor.execute(sql_cmd)
    pragma = cursor.fetchall()
    columns_to_get = []
    for i in pragma:
        columns_to_get.append('features.' + i[1])
    columns_to_get.append('sources.original_source_id')
    columns_to_get.append('sources.noisification')
    columns_to_get.append('sources.classification')
    columns_to_get.append('sources.survey')
    columns_to_get.append('sources.c1')
    columns_to_get.append('sources.c2')
    columns_to_get.append('sources.e1')
    columns_to_get.append('sources.e2')    

    # get desired rows in features and sources table
    columns_to_get_comma = ', '.join(columns_to_get)
    rows_to_get = '(' + ','.join(source_ids) + ')'
    sql_cmd = """SELECT """ + columns_to_get_comma + """ FROM sources, features WHERE sources.source_id = features.source_id AND features.source_id IN """ + rows_to_get
    cursor.execute(sql_cmd)
    db_info = cursor.fetchall()

    sql_cmd = """SELECT source_id FROM features"""
    cursor.execute(sql_cmd)
    db_info2 = cursor.fetchall()

    sql_cmd = """SELECT source_id FROM sources"""
    cursor.execute(sql_cmd)
    db_info3 = cursor.fetchall()

    print db_info2
    print db_info3
    print columns_to_get_comma
    print rows_to_get
    print db_info

    # now write to file
    column_names = ';'.join(columns_to_get)
    g = open(filename,'w')
    g.write(column_names + '\n')
    for i in db_info:
        output1 = ''
        for j in i:
            output1 += str(j) + ';'
        output1 = output1[:-2]
        g.write(output1 + '\n')
    g.close()


if __name__ == "__main__":
    if 1:
        connection = sqlite3.connect('astronomy.db')
        cursor = connection.cursor()

        # get all source ids in features table
        sql_cmd = """SELECT source_id FROM features"""
        cursor.execute(sql_cmd)
        db_info = cursor.fetchall()
        source_ids = []
        for i in db_info:
            source_ids.append(i[0])
        print source_ids
        
        # output the file
        outputRfile(source_ids,cursor,'outputRtest.txt')
        tfeOutput(source_ids,cursor,'tfe.txt')
