####
#### by James Long
#### date March 28, 2011
####
####


data1 = read.table('sources00001.txt',sep=';',header=TRUE)
names(data1)
head(data1)



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

