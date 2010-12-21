###
### tools for visualizing curves
###
### by James Long
### date Dec 7, 2010

import sqlite3
import numpy as np
import create_database
from matplotlib import pyplot as plt

## shows folded and/or unfolded curves
def plot_curve(tfe,freq=0,plot_folded=True,plot_unfolded=True,classification='Unknown Class',survey='Unknown'):
    # check to make sure arguments are consistent
    if freq == 0 and plot_folded == True:
        print "Need a frequency to fold on. Add freq as an argument to the function call."
        return
    if not plot_folded and not plot_unfolded:
        print "No graphs requested!"
        return

    # make title and phase times
    title = classification + ' from ' + survey
    folded = tfe[:,0] * freq % 1

    # both folded and unfolded
    if plot_unfolded and plot_folded:
        plt.figure(1)
        plt.subplot(211)
        plt.title(title)
        plt.plot(tfe[:,0],tfe[:,1],'r.')
        plt.xlabel("Time")
        plt.ylabel("Flux")
        plt.subplot(212)
        plt.plot(folded,tfe[:,1],'r.')
        plt.ylabel("Flux")
        plt.xlabel("Phase")

    # only unfolded plot
    if plot_unfolded and not plot_folded:
        plt.plot(tfe[:,0],tfe[:,1],'r.')
        plt.title(title)
        plt.xlabel("Time")
        plt.ylabel("Flux")


    # only folded plot
    if not plot_unfolded and plot_folded:
        plt.plot(folded,tfe[:,1],'r.')
        plt.title(title)
        plt.ylabel("Flux")
        plt.xlabel("Phase")

    plt.show()
    

# input source_ids and access to database -> plot all the source_ids
def plot_set_of_curves(source_ids,cursor):
    for i in source_ids:
        tfe = create_database.get_measurements(i,cursor)
        sql_cmd = """SELECT classification, survey FROM sources WHERE source_id = (?)"""
        cursor.execute(sql_cmd,[i])
        db_info = cursor.fetchall()
        freq = 2.58423833943 # should be a function to get the frequency 
        plot_curve(tfe,freq=freq,classification=db_info[0][0],survey=db_info[0][1])



if __name__ == "__main__":
    if 1:
        connection = sqlite3.connect('astronomy.db')
        cursor = connection.cursor()

        # get a source_id from table
        sql_cmd = "SELECT source_id,survey,classification FROM sources"
        cursor.execute(sql_cmd)
        db_info = cursor.fetchall()
        source_ids = []
        for i in range(len(db_info)):
            source_ids.append(db_info[i][0])
        plot_set_of_curves(source_ids,cursor)


    if 0:
        connection = sqlite3.connect('astronomy.db')
        cursor = connection.cursor()

        # get a source_id from table
        sql_cmd = "SELECT source_id,survey,classification FROM sources"
        cursor.execute(sql_cmd)
        db_info = cursor.fetchall()
        source_id = [db_info[4][0]]
        survey = db_info[4][1]
        classification = db_info[4][2]

        # get all measurements from 
        sql_cmd = "SELECT time, flux, error FROM measurements WHERE source_id = (?)"
        cursor.execute(sql_cmd,source_id)
        db_info = cursor.fetchall()
        tfe = np.array(db_info)

        # send everything to plot curve so we can have a nice plot
        plot_curve(tfe,freq=2.58423833943,classification=classification,survey=survey)
