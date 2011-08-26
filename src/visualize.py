###
### tools for visualizing curves
###
### by James Long
### date August 8, 2011
###

import sqlite3
import numpy as np
import create_database
import smoothers
from matplotlib import pyplot as plt

###
### write code to save to file
###

## shows folded and/or unfolded curves
def plot_curve(tfe,period=0,plot_folded=True,
               plot_unfolded=True,smooth_folded=True,
               classification='Unknown Class',
               survey='Unknown Survey',
               show_plot=True,
               save_figure=False,
               save_figure_name='figure.pdf'):

    ## check to make sure arguments are consistent
    if period == 0 and plot_folded == True:
        print "Need a period to fold on. Add period as an argument to the function call."
        return
    if not plot_folded and not plot_unfolded:
        print "No plots requested!"
        return

    ## don't want to overwrite plots
    plt.close()

    ## make title and phase times
    title = classification + ' from ' + survey

    ## both folded and unfolded
    if plot_unfolded and plot_folded:
        plt.figure(1)
        plt.subplot(211)
        plt.title(title)
        plt.plot(tfe[:,0],tfe[:,1],'r.')
        plt.xlabel("Time")
        plt.ylabel("Flux")
        ## smooth on 
        plt.subplot(212)
        smo = smoothers.supersmooth(tfe,period)
        plt.plot(tfe[:,0],smo,tfe[:,0],tfe[:,1],'x')
        plt.xlabel("Phased using period: " + repr(period))
        plt.ylabel("Flux")
        ## smooth on estimated period
        ## plt.subplot(213)
        ## smo = smoothers.supersmooth(tfe,1/2)
        ## plt.plot(tfe[:,0],smo,'o',tfe[:,0],tfe[:,1],'x')

    ## only unfolded plot
    if plot_unfolded and not plot_folded:
        plt.plot(tfe[:,0],tfe[:,1],'r.')
        plt.title(title)
        plt.xlabel("Time")
        plt.ylabel("Flux")

    ## only folded plot
    if not plot_unfolded and plot_folded:
        plt.plot(folded,tfe[:,1],'r.')
        plt.title(title)
        plt.ylabel("Flux")
        plt.xlabel("Phase")

    ## either save figure or show figure
    if save_figure:
        plt.savefig(save_figure_name)
    if show_plot:
        plt.show()



if __name__ == "__main__":
    if 1:
        connection = sqlite3.connect('simulated_astronomy.db')
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
