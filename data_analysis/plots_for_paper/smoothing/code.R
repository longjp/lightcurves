########## 
########## 
########## PLOT RESULTS FROM SMOOTHING EXAMPLES
##########
##########
########## by James Long 
########## date: 1/8/2011 
########## 

rm(list=ls(all=TRUE))
set.seed(22071985)
options(width=50)
source('~/Rmodules/Rfunctions.R')
library('randomForest')
library('rpart')
library('xtable')

## the most interesting features
good_features = c(
  "features.p2p_scatter_over_mad",
  "features.small_kurtosis",
  "features.p2p_scatter_2praw",
  "features.beyond1std",
  "features.freq1_harmonics_amplitude_0",
  "features.freq1_harmonics_amplitude_1",
  "features.qso_log_chi2nuNULL_chi2nu",      
  "features.percent_difference_flux_percentile",
  "features.qso_log_chi2_qsonu",
  "features.flux_percentile_ratio_mid20",
  "features.freq1_harmonics_rel_phase_3",      
  "features.flux_percentile_ratio_mid80",      
  "features.skew",      
  "features.median_buffer_range_percentage",   
  "features.fold2P_slope_90percentile",        
  "features.median_absolute_deviation",      
  "features.flux_percentile_ratio_mid35",
  "features.freq1_harmonics_freq_0",      
  "features.flux_percentile_ratio_mid65",
  "features.freq_signif",
  "features.std",                  
  "features.scatter_res_raw",        
  "features.fold2P_slope_10percentile",      
  "features.flux_percentile_ratio_mid50",
  "features.percent_amplitude",
  "features.amplitude",       
  "features.p2p_scatter_pfold_over_mad",
  "features.p2p_ssqr_diff_over_var",
  "features.stetson_j",
  "features.stetson_k")






## load the data
features = '../../../data_processed/cadence_comparison/ogle_versus_hipparcos.dat'
tfe = '../../../data_processed/cadence_comparison/tfe_ogle_versus_hipparcos.dat'
data1total = read.table(features,sep=';',header=TRUE)
time_flux = read.table(tfe,sep=';',header=TRUE)


## examine this
nrow(data1total)
nrow(time_flux)

table(data1total$sources.survey)
table(data1total$sources.noisification)

names(data1total)
table(data1total$sources.noisification)


## break up by obs type
data1total$obs_type[data1total$sources.survey ==
                    "ogle_test"] = "ogle_test"
data1total$obs_type[data1total$sources.survey ==
                    "hipparcos_test"] = "hipparcos_test"
data1total$obs_type[data1total$sources.survey == "ogle_train" &
                    data1total$sources.noisification !=
                    "cadence_noisify_smoothed"] = "ogle_train"
data1total$obs_type[(data1total$sources.survey ==
                    "hipparcos_train" &
                    data1total$sources.noisification !=
                    "cadence_noisify_smoothed")] =
                    "hipparcos_train"
to_select = (data1total$sources.survey ==
                    "ogle_train" &
                    data1total$sources.noisification ==
                    "cadence_noisify_smoothed" &
                    grepl("hip",data1total$sources.noise_args))
data1total$obs_type[to_select] = "ogle_train_smoothed_hipparcos"
to_select = (data1total$sources.survey == "ogle_train" &
             data1total$sources.noisification ==
             "cadence_noisify_smoothed" &
             grepl("ogle",data1total$sources.noise_args))
data1total$obs_type[to_select] = "ogle_train_smoothed_ogle"
to_select = (data1total$sources.survey == "hipparcos_train" &
             data1total$sources.noisification ==
             "cadence_noisify_smoothed" &
             grepl("hip",data1total$sources.noise_args))
data1total$obs_type[to_select] =
             "hipparcos_train_smoothed_hipparcos"
to_select = (data1total$sources.survey == "hipparcos_train" &
             data1total$sources.noisification ==
             "cadence_noisify_smoothed" &
             grepl("ogle",data1total$sources.noise_args))
data1total$obs_type[to_select] = "hipparcos_train_smoothed_ogle"
table(data1total$obs_type)


## now change some noisification to identity
sum(grepl("all",data1total$sources.noise_args))
to_select = grepl("all",data1total$sources.noise_args)
data1total$sources.noisification[to_select] = "identity"
sum(data1total$sources.noisification == "identity")
data1total$sources.original_source_id[data1total$sources.noisification == "identity"] = data1total$features.source_id[data1total$sources.noisification == "identity"]


sum(data1total$sources.original_source_id == data1total$features.source_id)



## now dedupe
contains.random = grepl("random",data1total$sources.noise_args)
sum(contains.random)
data1total$contains.random = contains.random
data1total$is_original = 1*(data1total$sources.original_source_id ==
  data1total$features.source_id)
data1total = dedupe(data1total,
  c("features.n_points","sources.original_source_id",
    "contains.random","is_original","obs_type")
  )




ProduceKDE = function(feature,
  train_name,n_points){
  which_points = ((data1total$obs_type == train_name) &
                  (data1total$features.n_points == n_points) &
                  (data1total$row_id == 0) &
                  (!data1total$contains.random))
  feature_vals = data1total[which_points,feature]
  class_vals = data1total$sources.classification[which_points]
  class_vals = as.character(class_vals)
  class_vals[class_vals != "Mira"] = "Other"
  ordering = order(class_vals)
  print(length(class_vals))
  return(list(feature_vals[ordering],class_vals[ordering]))
}




### currently figure 9 in paper
the_data = ProduceKDE("features.amplitude","ogle_train",10)
pdf('amplitude_ogle_10.pdf')
DrawKDES(the_data[[1]],the_data[[2]],
         trim=.01,xlab="amplidude (mags)",
         cex.lab=1.4,line.width=4)
dev.off()
the_data = ProduceKDE("features.amplitude","hipparcos_train",10)
pdf('amplitude_hip_10.pdf')
DrawKDES(the_data[[1]],the_data[[2]],trim=.01,
         xlab="amplidude (mags)",cex.lab=1.4,line.width=4)
dev.off()


the_data = ProduceKDE("features.p2p_scatter_over_mad",
  "ogle_train",10)
pdf('p2p_ogle_10.pdf')
DrawKDES(the_data[[1]],the_data[[2]],trim=.01,
         xlab="P2PS",cex.lab=1.4,line.width=4)
dev.off()

the_data = ProduceKDE("features.p2p_scatter_over_mad",
  "hipparcos_train",10)
pdf('p2p_hip_10.pdf')
DrawKDES(the_data[[1]],the_data[[2]],trim=.01,
         xlab="P2PS",cex.lab=1.4,line.width=4)
dev.off()











### ANALYZE RESULTS FOR HIPPARCOS TEST

rm(list=ls(all=TRUE))
set.seed(22071985)
options(width=50)
source('~/Rmodules/Rfunctions.R')

a_test_name = "hipparcos_test"
a_train_name = "hipparcos_train"
RData = fileOutLoc(paste('../../synthetic_smoothing/RData/',a_test_name,a_train_name,sep=""))
load(RData('randomForestNoisificationResults.RData'))
errorsSD.toplot = array(0,c(5,length(points.levels),3))
errorsSD.toplot[1,,] = errorsSD[1,,]
errorsSD.toplot[4,,] = errorsSD[4,,]

a_train_name = "ogle_train"
RData = fileOutLoc(paste('../../synthetic_smoothing/RData/',a_test_name,a_train_name,sep=""))
load(RData('randomForestNoisificationResults.RData'))
errorsSD.toplot[2,,] = errorsSD[4,,]
errorsSD.toplot[5,,] = errorsSD[1,,]


a_train_name = "ogle_train_smoothed_hipparcos"
RData = fileOutLoc(paste('../../synthetic_smoothing/RData/',a_test_name,a_train_name,sep=""))
load(RData('randomForestNoisificationResults.RData'))
errorsSD.toplot[3,,] = errorsSD[4,,]


pdf('hipparcosTestCadence.pdf')
plotLines(errorsSD.toplot,points.levels,
          ylab="Error Rate",xlab="Number Flux Test Set",
          maintitle="",
          cex.lab=1.4)
legend("topright",c("Hipparcos Cadence Naive","Ogle Cadence Noisified","Ogle Smoothed To Hipparcos - Noisified","Hipparcos Cadence Noisified","Ogle Naive"),col=c(1,2,3,4,5),lwd=2,cex=1.4,title="Training Sets",pch=1:5)
dev.off()





### ANALYSIS RESULTS FOR OGLE TEST
rm(list=ls(all=TRUE))
set.seed(22071985)
options(width=50)
source('~/Rmodules/Rfunctions.R')


a_test_name = "ogle_test"
a_train_name = "ogle_train"
RData = fileOutLoc(paste('../../synthetic_smoothing/RData/',a_test_name,a_train_name,sep=""))
load(RData('randomForestNoisificationResults.RData'))
errorsSD.toplot = array(0,c(5,length(points.levels),3))
errorsSD.toplot[1,,] = errorsSD[1,,]
errorsSD.toplot[4,,] = errorsSD[4,,]


a_train_name = "hipparcos_train"
RData = fileOutLoc(paste('../../synthetic_smoothing/RData/',a_test_name,a_train_name,sep=""))
load(RData('randomForestNoisificationResults.RData'))
errorsSD.toplot[2,,] = errorsSD[4,,]
errorsSD.toplot[5,,] = errorsSD[1,,]

a_train_name = "hipparcos_train_smoothed_ogle"
RData = fileOutLoc(paste('../../synthetic_smoothing/RData/',a_test_name,a_train_name,sep=""))
load(RData('randomForestNoisificationResults.RData'))
errorsSD.toplot[3,,] = errorsSD[4,,]


pdf('ogleTestCadence.pdf')
plotLines(errorsSD.toplot,points.levels,
          ylab="Error Rate",xlab="Number Flux Test Set",
          maintitle="",cex.lab=1.4)
legend("topright",
       c("Ogle Cadence Naive","Hipparcos Cadence Noisified","Hipparcos Smoothed To Ogle - Noisified","Ogle Cadence Noisified","Hipparcos Naive"),
       col=c(1,2,3,4,5),lwd=2,cex=1.4,
       title="Training Sets",pch=1:5)
dev.off()
