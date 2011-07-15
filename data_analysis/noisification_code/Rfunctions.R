####
#### useful R functions
####



###
### graphical
###

# FUNCTION: returns a function which appends the
# correct folder name to any argument
# called at the beginning of an R script
# :::usage:::
# graphics = graphics_output('paper1/figures/')
# pdf(graphics('plot1.pdf'))
fileOutLoc = function(folder_name=''){
  graphics = function(filename){
    return(paste(folder_name,filename,sep=''))
  }
  return(graphics)
}

# FUNCTION: given and n x p (x 3) matrix  
# create n lines of length p, x axis
# specified by argument x-vals, if results
# is an (n x p x 3) array then third dim
# provides standard errors which are
# plotted in a lighter color


plotLines = function(results,x.vals,xlab=NULL,ylab=NULL,maintitle=NULL,ymin=NULL,ymax=NULL,linecolors=NULL,hash.freq=1,sd.freq=1){
  if(length(dim(results)) == 3) point.est = results[,,1]
  if(length(dim(results)) == 2) point.est = results
  if(is.null(linecolors)) linecolors = 1:nrow(point.est)
  if(length(linecolors)==1) linecolors = rep(linecolors,nrow(point.est))

  xmin = min(x.vals) #- .05*(max(x.vals) - min(x.vals))
  xmax = max(x.vals) #+ .05*(max(x.vals) - min(x.vals))
  if(is.null(ymax)){
    ymax = max(point.est) + .05*(max(point.est) - min(point.est))
  }
  if(is.null(ymin)){
    ymin = min(point.est) - .05*(max(point.est) - min(point.est))
  }
  # print the point estimates
  plot(c(xmin,xmax),c(ymin,ymax),xlab=xlab,ylab=ylab,main=maintitle,col=0)
  # make the standard errors
  if(length(dim(results)) == 3){
    width.error.bar = (xmax - xmin) / 300
    for(i in 1:nrow(point.est)){
      for(j in 1:length(x.vals)){
        ## only draw the lines at multiples of what we want
        if(j %% sd.freq == 0){
          lines(rep(x.vals[j],2),c(results[i,j,2],results[i,j,3]),type='l')
          lines(c(x.vals[j] - width.error.bar,x.vals[j] + width.error.bar),
                rep(results[i,j,2],2),
                type='l')
          lines(c(x.vals[j] - width.error.bar,x.vals[j] + width.error.bar),
                rep(results[i,j,3],2),
                type='l')
        }
      }
    }
  }
  ## make the lines
  for(i in 1:nrow(point.est)){
    points(x.vals,point.est[i,],type='l',col=linecolors[i],lwd=1.5)
    ## only put hash marks ever hash.freq vals on x
    new.x.vals = x.vals[(1:length(x.vals)) %% hash.freq == 0]
    new.point.est = point.est[i,][(1:length(point.est[i,])) %% hash.freq == 0]
    points(new.x.vals,new.point.est,type='p',pch=i,col=linecolors[i],lwd=3)
  }
  # print standard errors if given
}




plotTree = function(atree,maintitle="A CART Tree"){
  plot(atree,margin=.1,uniform=TRUE,main=maintitle)
  atree$frame[,1] = sub("features.","",atree$frame[,1])
  atree$frame[,1] = sub("features.","",atree$frame[,1])
  text(atree,use.n=TRUE)
}



plotLightCurve = function(tfe,
  xLabel="Time",yLabel="m",maintitle="A Lightcurve",
  sd.errors=1,width.error.bar=1,cex=.5,reverse=TRUE){

  tfe[,2] = -1*tfe[,2]
  # if there is no main title make the margin at the top very small
  if(maintitle == "") par(mar=c(5.1,4.1,1,2.1))

  # set the width of the error bars
  xmin = min(tfe[,1])
  xmax = max(tfe[,1])
  ymin = min(tfe[,2] - sd.errors * tfe[,3])
  ymax = max(tfe[,2] + sd.errors * tfe[,3])

  # set the width of the error bar relative to x-axis
  width.error.bar = width.error.bar * (xmax - xmin) / 300
  # set up the plot to the get right dimensions
  plot(c(xmin,xmax),c(ymin,ymax),col=0,
       main=maintitle,xlab=xLabel,ylab=yLabel,yaxt='n')
  if(!reverse)   axis(2)
  else {
    yaxis = (ymax - ymin) * (0:4) / 4 + ymin
    axis(2,at=yaxis,labels=-round(yaxis,1),las=2)
  }
  ## draw the error bars
  for(i in 1:nrow(tfe)){
    lines(rep(tfe[i,1],2),c(tfe[i,2] - sd.errors*tfe[i,3],
                            tfe[i,2] + sd.errors*tfe[i,3]),type='l',lwd=.5)
    lines(c(tfe[i,1] - width.error.bar,tfe[i,1] + width.error.bar),
          rep(tfe[i,2] - sd.errors*tfe[i,3],2),type='l')
    lines(c(tfe[i,1] - width.error.bar,tfe[i,1] + width.error.bar),
          rep(tfe[i,2] + sd.errors*tfe[i,3],2),type='l')
  }
  # put the points on the plot
  points(tfe[,1],tfe[,2],pch=19,cex=cex)
}







## convert matrix into 3-D array with [,,1]
## containing original matrix and [,,2] and
## [,,3] containing standard errors
computeStandardErrors = function(matrix1,n,sderror=1,additional.var=0){
  results = array(0,dim=c(nrow(matrix1),ncol(matrix1),3))
  results[,,1] = matrix1
  results[,,2] = matrix1 - sderror * sqrt((matrix1 * (1 - matrix1) / n) +
           additional.var)
  results[,,3] = matrix1 + sderror * sqrt((matrix1 * (1 - matrix1) / n) +
           additional.var)
  return(results)
}



####
####
####
dedupe = function(data.f,columns.separate){
  o1 = do.call(order,as.data.frame(data.f[,columns.separate]))
  data.f = data.f[o1,]
  duped = 1*duplicated(data.f[,columns.separate])
  row_id = rep(0,length(duped))
  for(i in 2:length(row_id)){
    row_id[i] = duped[i]*(row_id[i-1] + 1)
  }
  data.f$row_id = row_id
  return(data.f)
}






##########
########## for plotting light curves
##########


ids = data1clean$features.source_id[
  data1clean$sources.classification=="Delta Scuti"]
ids = data1clean$features.source_id
ids = ids[1:100]
par(mfcol=c(2,1),ask=TRUE)
for(id in ids){
measurements = time_flux[time_flux$source_id == id,c("time","flux","error")]
period = 1 / data1clean$features.freq1_harmonics_freq_0[data1clean$features.source_id == id]
period
classification = data1clean$sources.classification[data1clean$features.source_id == id]
plot(measurements$time,measurements$flux)
plot((measurements$time %% period) / period,measurements$flux,main=classification)
}
