#####
##### is there a relationship between flux and flux error?
#####
##### by James Long
##### date Jan 24, 2011
#####


tfe = read.table('tfe.txt',header=T,sep=';')
tfe$measurements_id = NULL
tfe$time = NULL
tfe$source_id = NULL


to_use = runif(nrow(tfe)) > .999
pdf('flux_error.pdf')
plot(tfe$flux[to_use],tfe$error[to_use],col='#00000020',main='Flux versus flux error for a fraction of ASAS',xlab='Magnitude',ylab='Error')
dev.off()


