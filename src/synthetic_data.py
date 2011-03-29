#####
##### generate some synthetic light curves
#####
##### by James Long
##### date Jan 31, 2011
#####

#### goals:
#### 1. create program that will generate light curves of different varieties, store them in db
#### 2. read up on pickling and oop for python and see if we can improve on 1.

## questions:
## 1. should I pack all arguments into a list
## 2. nobs doesn't always make sense
## 3. could use cadences that exactly matched asas

import numpy as np
import visualize
import sqlite3
import create_database

def poisson_process_cadence(nobs=100,rate=1,timeframe=False):
    cadence = np.random.exponential(rate,nobs)
    for i in range(len(cadence)-1):
        cadence[i+1] = cadence[i] + cadence[i+1]
    return cadence

def sinusoidal(cadence,period=np.pi,phase=0.,mag=1,mag_off=0,error=0):
    if error == 0:
        errors = np.zeros(cadence.size)
    else:
        errors = np.random.normal(loc=0,scale=error,size=cadence.size)
    magnitudes = mag*np.sin(2*np.pi*(((cadence - period*phase) % period)) / period) + mag_off + errors
    tfe = np.column_stack((cadence,magnitudes,errors))
    return tfe

def detached(cadence,period=np.pi,phase=0.,mag_off=0,error=0,depth1=.7,depth2=.8,flat_frac=.5):
    if error == 0:
        errors = np.zeros(cadence.size)
    else:
        errors = np.random.normal(loc=0,scale=error,size=cadence.size)
    
    # get the first depression
    in_first_depression = (((cadence - phase*period) % period) / period )  / ( (1 - flat_frac) / 2 ) < 1
    where_in_first_depression = -1*depth1*np.sin(np.pi*(((cadence - phase*period) % period) / period )  \
                                                     / ( (1 - flat_frac) / 2 ))
    first_depression = in_first_depression * where_in_first_depression

    # get the second depression
    in_second_depression = (((cadence - phase*period - period / 2) % period) / period )  / ( (1 - flat_frac) / 2 ) < 1
    where_in_second_depression = -1*depth2*np.sin(np.pi*(((cadence - phase*period - period / 2) % period) / period )  / ( (1 - flat_frac) / 2 ))
    second_depression = in_second_depression * where_in_second_depression

    magnitudes = mag_off + first_depression + second_depression + errors 
    tfe = np.column_stack((cadence,magnitudes,errors))
    return(tfe)

def generate_and_store_curves(ncurves,points_per_curve,cursor,connection,survey="Synthetic"):
    # get the current date/time
    sql_cmd = """SELECT datetime('now')"""
    cursor.execute(sql_cmd)
    db_info = cursor.fetchall()
    current_date = db_info[0][0]
    # generate and curve and store it in the db
    for i in range(ncurves):
        # generation
        type1 = np.random.uniform() > .5
        cadence = poisson_process_cadence(nobs=points_per_curve,rate=10)    
        if type1:
            period = 10 * np.random.uniform()
            phase = np.random.uniform()
            mag = np.random.uniform()*3 + 2
            mag_off = np.random.uniform()*3 + 2
            tfe = sinusoidal(cadence,period=period,phase=phase, \
                                 mag=mag,mag_off=mag_off,error=mag/20)
            source_class = "sinusoidal"
        else:
            period = 10 * np.random.uniform()
            phase = np.random.uniform()
            mag_off = np.random.uniform()*3 + 2
            depth1 = np.random.uniform() + .3
            depth2 = np.random.uniform() + .3
            flat_frac = np.random.uniform()
            tfe = detached(cadence,period=period,phase=phase,mag_off=mag_off, \
                               error=depth1/10,depth1=depth1,depth2=depth2, \
                               flat_frac=flat_frac)
            source_class = "detached"
        # storage
        curve_info = [points_per_curve,source_class,0,0,0,0,None, \
                          survey,0,current_date,period]
        curve_info_names = ["number_points","classification","c1","e1","c2","e2","raw_xml","survey","xml_filename","date","true_period"]
        print source_class
        print curve_info
        create_database.enter_record(curve_info,curve_info_names,tfe,cursor,original_number=-1)

    # save changes to the db
    connection.commit()


if __name__ == "__main__":
    if 1:
        features_file = "derived_features_list.txt" # where we define features
        
        connection = sqlite3.connect('astronomy.db')
        cursor = connection.cursor()
        create_database.create_db(cursor,features_file=features_file,\
                                  REMOVE_RECORDS=False)
        connection.commit()

        ncurves = 10
        generate_and_store_curves(ncurves,200,cursor,connection)
    # test sinusoidal
    if 0:
        period = 1000000.
        mag = 1.
        phase = -1. / 4.
        print phase
        cadence = poisson_process_cadence(200,10)
        tfe = sinusoidal(cadence,period=period,phase=phase,mag=mag,mag_off=17)
        visualize.plot_curve(tfe,freq= 1 / period)
    # test detached (perhaps with flat_frac = 0. this becomes attached)
    if 0:
        period = np.pi
        depth1 = 1.3
        depth2 = .8
        flat_frac = .0
        phase = .4
        cadence = poisson_process_cadence(nobs=2000,rate=10)
        tfe = detached(cadence,period=period,phase=phase,mag_off=0,error=depth1/10,depth1=depth1,depth2=depth2,flat_frac=flat_frac)
        visualize.plot_curve(tfe,freq= 1 / period)
        print tfe
