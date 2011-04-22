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

### TODO: turn grey lines into verical bars
plotLines = function(results,x.vals,xlab=NULL,ylab=NULL,maintitle=NULL,ymin=NULL,ymax=NULL,linecolors=NULL){
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
    ymin = min(point.est) + .05*(max(point.est) - min(point.est))
  }
  # print the point estimates
  plot(c(xmin,xmax),c(ymin,ymax),xlab=xlab,ylab=ylab,main=maintitle,col=0)
  # make the standard errors
  if(length(dim(results)) == 3){
    for(i in 1:nrow(point.est)){
     # following produces grey dotted line error bars
     # points(points.levels,results[i,,2],type='l',col='grey',lwd=.5,lty=2)
     # points(points.levels,results[i,,3],type='l',col='grey',lwd=.5,lty=2)
      for(j in 1:length(points.levels)){
        lines(rep(points.levels[j],2),c(results[i,j,2],results[i,j,3]),type='l')
      }
    }
  }
  # make the lines
  for(i in 1:nrow(point.est)){
    points(points.levels,point.est[i,],type='l',col=linecolors[i],lwd=1.5)
    points(points.levels,point.est[i,],type='p',pch=i,col=linecolors[i],lwd=2.5)
  }
  # print standard errors if given

}




plotTree = function(atree,maintitle="A CART Tree"){
  plot(atree,margin=.1,uniform=TRUE,main=maintitle)
  atree$frame[,1] = sub("features.","",atree$frame[,1])
  atree$frame[,1] = sub("features.","",atree$frame[,1])
  text(atree,use.n=TRUE)
}







## convert matrix into 3-D array with [,,1]
## containing original matrix and [,,2] and
## [,,3] containing standard errors
computeStandardErrors = function(matrix1,n,sderror=1){
  results = array(0,dim=c(nrow(matrix1),ncol(matrix1),3))
  results[,,1] = matrix1
  results[,,2] = matrix1 - sderror * sqrt(matrix1 * (1 - matrix1) / n)
  results[,,3] = matrix1 + sderror * sqrt(matrix1 * (1 - matrix1) / n)
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
########## testing area
##########
