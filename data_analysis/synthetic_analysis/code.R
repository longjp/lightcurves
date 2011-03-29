####
#### by James Long
#### date March 28, 2011
####
####

# to do
# 1. add more error to sinusoidal?

# clear out history
rm(list=ls(all=TRUE))

# get the features
data1 = read.table('sources00001.dat',sep=';',header=TRUE)
# get the tfe
time_flux = read.table('tfe00001.dat',sep=';',header=TRUE)


names(data1)
head(data1)
sum(is.na(data1))

names(time_flux)
head(time_flux)
sum(is.na(time_flux))



###
### examine folded curves
###
### returns data frame of times and flux measurement
### for source in data1_row

fold_curve = function(times,freq){
	number_periods = times * freq
	folded = number_periods - floor(number_periods)
	return(folded)
}

obs_vec = (1:nrow(data1))[data1$features.source_id == data1$sources.original_source_id]
par(mfcol=c(2,1),ask=TRUE)
for(i in obs_vec){
  relevant_curves = subset(time_flux,subset=(source_id==data1$sources.original_source_id[i]))[,2:3]
  first_30 = 1*(rank(relevant_curves[,1]) < 30) + 1
  plot(relevant_curves[,1],relevant_curves[,2],main=data1[i,"sources.classification"],ylab='Flux',col=first_30)
  folded_times = fold_curve(relevant_curves[,1],data1[i,"features.freq1_harmonics_freq_0"])
  plot(folded_times,relevant_curves[,2],ylab='Flux',col=first_30)
}






GenerateClassifierSubsets = function(n.points,n.subsets){
  return(0)
}



- 1 point
- 5 point
- random
- naive



data1$sources.noisification[1:10]

no.noise = data1$sources.noisification == "identity"
noise = data1$sources.noisification == "cadence_noisify"
mean((data1$sources.true_period[no.noise] - (1 / data1$features.freq1_harmonics_freq_0[no.noise]))^2)

mean((data1$sources.true_period[noise] - (1 / data1$features.freq1_harmonics_freq_0[noise]))^2)



no.noise = subset(data1,subset=(sources.noisification=="identity"),select=c("sources.true_period","sources.original_source_id","features.freq1_harmonics_freq_0"))
noise = subset(data1,subset=(sources.noisification=="cadence_noisify"),select=c("sources.original_source_id","features.freq1_harmonics_freq_0"))
names.no.noise = names(no.noise)
names.no.noise[length(names.no.noise)] = "noise_free_freq"
names(no.noise) = names.no.noise

noise.comparison = merge(no.noise,noise)
noise.comparison$noise_free_freq = 1 / noise.comparison$noise_free_freq
noise.comparison$features.freq1_harmonics_freq_0 = 1 / noise.comparison$features.freq1_harmonics_freq_0

plot(noise.comparison$sources.true_period,noise.comparison$features.freq1_harmonics_freq_0,ylim=c(0,10))
abline(0,3,col='grey')
abline(0,2,col='grey')
abline(0,1,col='grey')
abline(0,1/2,col='grey')
abline(0,1/3,col='grey')


plot(noise.comparison$sources.true_period,noise.comparison$noise_free_freq)
abline(0,3,col='grey')
abline(0,2,col='grey')
abline(0,1,col='grey')
abline(0,1/2,col='grey')
abline(0,1/3,col='grey')


##### to write for this file
# 1. simple classifier
# 2. guess of true period as a function of number of points
#    - maybe # wrong is a better measure
#    - or # not at some harmonic
# 3. visualize tfe's with true_period and guessed
# 4. classifier performance as a function of number points

# to deliver:
# 1. error rate as a function of points for:
# - naive
# - noisification 1 x
# - noisification 2 x
# - noisification 5 x
# - random noisification (random subset of points)
#   this is the natural extreme of non-matching
# 2. a few confusion matrices
# 3. correct period as a function of # of points (relate w/ 1.)
# 4. some work on matching cadences
# - in N-W a good idea, other smoothers
# 5. lots of images of curves so we can discuss parameters

