#####
##### contains functions to output db info to files that could be used
##### by R or VOsource programs
#####
##### by James Long
##### date Jan 11, 2011
##### modified March 28, 2011
#####

import sqlite3
import numpy as np
import create_database

## input an 1-darray of source ids, output a file where 
## the first line is names is measurements_id, time, flux, error, source_id
## remaining lines are those values for particular sources
def tfeOutput(source_ids,cursor,filename,table_name="measurements"):
    '''This is documentation'''
    # convert source_ids to integers
    j = 0
    for i in source_ids:
        source_ids[j] = repr(i)
        j += 1

    ## now using function from create_database to get 
    ## pragma of measurements table
    columns_to_get = create_database.get_pragma(cursor,table=table_name)

    # get desired rows in features and sources table
    rows_to_get = '(' + ','.join(source_ids) + ')'
    sql_cmd = """SELECT * FROM """ + table_name + """ WHERE source_id IN """ + rows_to_get
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



def outputOriginalOnly(source_ids,cursor,filename):
    # convert source_ids to integers
    j = 0
    for i in source_ids:
        source_ids[j] = repr(i)
        j += 1
    ## get names of features
    columns_to_get = create_database.get_pragma(cursor,table='features')
    ## TODO: put these in try / except
    columns_to_get.remove('n_points')
    columns_to_get.remove('min')
    columns_to_get.remove('max')
    columns_to_get.remove('median')
    columns_to_get = map(lambda feature_name:'features.'+feature_name,
                         columns_to_get)
    columns_to_get.append('sources.classification')

    # get desired rows in features and sources table
    columns_to_get_comma = ', '.join(columns_to_get)
    rows_to_get = '(' + ','.join(source_ids) + ')'
    sql_cmd = """SELECT """ + columns_to_get_comma + """ FROM sources, features WHERE sources.source_id = features.source_id AND features.source_id IN """ + rows_to_get
    cursor.execute(sql_cmd)
    db_info = cursor.fetchall()


    ## rename columns
    columns_to_get = map(lambda i:i.split('.')[1],columns_to_get)

    ## write to file
    column_names = ';'.join(columns_to_get)
    g = open(filename,'w')
    g.write(column_names + '\n')
    for i in db_info:
        output1 = ''
        for j in i:
            output1 += str(j) + ';'
        output1 = output1[:-1]
        g.write(output1 + '\n')
    g.close()


def outputIntervals(source_ids,cursor,filename):
    ''' outputs feature intervals to a data file '''
        # convert source_ids to integers
    j = 0
    for i in source_ids:
        source_ids[j] = repr(i)
        j += 1
    ## get names of features
    columns_to_get = create_database.get_pragma(cursor,table='features')
    ## TODO: put these in try / except
    columns_to_get.remove('n_points')
    columns_to_get.remove('min')
    columns_to_get.remove('max')
    columns_to_get.remove('median')
    columns_to_get.remove('source_id')
    columns_to_get = map(lambda feature_name:'features.'+feature_name,
                         columns_to_get)
    columns_to_get_min = map(lambda feature_name:'min('+feature_name+')',
                         columns_to_get)
    columns_to_get_max = map(lambda feature_name:'max('+feature_name+')',
                         columns_to_get)
    columns_to_get = columns_to_get_min + columns_to_get_max
    columns_to_get.append('sources.classification')
    columns_to_get.append('sources.original_source_id')

    # get desired rows in features and sources table
    columns_to_get_comma = ', '.join(columns_to_get)
    rows_to_get = '(' + ','.join(source_ids) + ')'
    sql_cmd = """SELECT """ + columns_to_get_comma + """ FROM sources, features WHERE sources.source_id = features.source_id AND features.source_id IN """ + rows_to_get + """ GROUP BY sources.original_source_id"""
    cursor.execute(sql_cmd)
    db_info = cursor.fetchall()

    ## rename columns
    for i in range(len(columns_to_get)):
        if columns_to_get[i][0:4] == "max(":
            columns_to_get[i] = columns_to_get[i][5:-1] + "U"
        if columns_to_get[i][0:4] == "min(":
            columns_to_get[i] = columns_to_get[i][5:-1] + "L"
    columns_to_get = map(lambda i:i.split('.')[1],columns_to_get)
    columns_to_get[-1] = "source_id"
    
    ## write to file
    column_names = ';'.join(columns_to_get)
    g = open(filename,'w')
    g.write(column_names + '\n')
    for i in db_info:
        output1 = ''
        for j in i:
            output1 += str(j) + ';'
        output1 = output1[:-1]
        g.write(output1 + '\n')
    g.close()



## input an 1-darray of source ids, output a file where 
## the first line is names of features, and each additional
## line is value of those features for particular source
def outputRfile(source_ids,cursor,filename):
    # convert source_ids to integers
    j = 0
    for i in source_ids:
        source_ids[j] = repr(i)
        j += 1
    
    # get column names from features table
    columns_to_get = create_database.get_pragma(cursor,table='features')
    columns_to_get = map(lambda feature_name:'features.'+feature_name,
                         columns_to_get)
    columns_to_get.append('sources.xml_filename')
    columns_to_get.append('sources.original_source_id')
    columns_to_get.append('sources.noisification')
    columns_to_get.append('sources.noise_args')
    columns_to_get.append('sources.true_period')
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

    # now write to file
    column_names = ';'.join(columns_to_get)
    g = open(filename,'w')
    g.write(column_names + '\n')
    for i in db_info:
        output1 = ''

        for j in i:
            output1 += str(j) + ';'
        output1 = output1[:-1]
        g.write(output1 + '\n')
    g.close()





if __name__ == "__main__":
    if 1:
        connection = sqlite3.connect('../db/simulated_astronomy.db')
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
