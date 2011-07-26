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




######
###### FOR MAKING NICE SCATTERPLOT KDES still beta
######



Draw3dScatterplot = function(feat,classes,xlab="Feature Density",
  class.cut=.01,slack.level=.1){

  ## REMOVE DATA LESS THAN class.cut QUANTILE AND GREATER THAN
  ## 1 - class.cut QUANTILE SO KDES NOT TOO WIDE
  a = aggregate(feat,list(classes=classes),
    function(x){quantile(x,c(class.cut,1-class.cut))})
  to.keep = rep(TRUE,length(feat))
  for(i in 1:nrow(a)){
    class.of.interest = (classes == a[i,1])
    to.keep[class.of.interest] = ((feat[class.of.interest] < a[i,2][1,2]) &
                                  (feat[class.of.interest] > a[i,2][1,1]))
  }
  classes = classes[to.keep]
  feat = feat[to.keep]


  ## NOW MAKE THE KDES
  n = length(classes)
  p = length(table(classes))
  cl.num = as.numeric(classes)
  gr.min = min(feat)
  gr.max = max(feat)
  slack = slack.level*(gr.max - gr.min)
  gr = seq(gr.min - slack,gr.max + slack,length.out=2000)
  sp.grid = cbind(rep(gr,p),sort(rep(1:p,2000)),rep(0,p*2000))
  for(ii in 1:p){
    d1 = density(feat[cl.num==ii],n = 2000,
      from=min(gr),to=max(gr))
    sp.grid[((ii-1)*2000 +1) : (2000*ii), 3] = d1$y
  }

  ## GET SOME AESTHETICS GOING
  tc = paste(tim.colors(n=p)[c(seq(1,p,3),seq(2,p,3),
                          seq(3,p,3))],"60",sep="")
  cols = tc[sp.grid[,2]]

  layout(matrix(c(1,2), 1, 2, byrow = TRUE),
         widths=c(1,8), heights=c(2,2))
  par(mar=c(3,0,0,0))
  s3d = scatterplot3d(sp.grid[,1],(p+1)-sp.grid[,2],
    sp.grid[,3],type='n',color=cols,pch='',box=F,
    angle=90,scale.y=5,axis=F,
    y.ticklabs=sort(levels(classes),decreasing=T),
    xlim=c(gr.min,gr.max),lab=c(6,p),grid=FALSE,mar=c(5,0,0,0))
  text(s3d$xyz.convert(rep(min(sp.grid[,1]),p), (1:p)+.4, rep(0,p)),
       labels=abbreviate(sort(levels(classes),decreasing=TRUE),minlength=6),
       cex=.8,col='gray10',pos=4)
  s3d = scatterplot3d(sp.grid[,1],(p+1)-sp.grid[,2],
    sp.grid[,3],type='h',color=cols,pch='',box=TRUE,
    angle=90,scale.y=5,axis=FALSE,
    y.ticklabs=sort(levels(classes),decreasing=T),
    xlim=c(gr.min - slack,gr.max + slack),lab=c(6,p),mar=c(5,0,0,0),grid=TRUE,
    ylab="",xlab="")
  col.lines = scatterplotGrid(sp.grid[,1],(p+1)-sp.grid[,2],
    sp.grid[,3],type='h',color=cols,pch='',box=TRUE,
    angle=90,scale.y=5,axis=FALSE,
    y.ticklabs=sort(levels(classes),decreasing=T),
    xlim=c(gr.min - slack,gr.max + slack),lab=c(6,p),mar=c(5,0,0,0),grid=TRUE,
    ylab="",xlab="")
  axis(1,labels=col.lines[[2]],at=col.lines[[1]])
  title(xlab=xlab,cex.lab=1.2)
}



## PURPOSE: FIND THE NAME OF THE TICK MARKS AND WHERE THE TICK MARKS ARE LOCATED
##
##
scatterplotGrid = function (x, y = NULL, z = NULL, color = par("col"), pch = NULL, 
    main = NULL, sub = NULL, xlim = NULL, ylim = NULL, zlim = NULL, 
    xlab = NULL, ylab = NULL, zlab = NULL, scale.y = 1, angle = 40, 
    axis = TRUE, tick.marks = TRUE, label.tick.marks = TRUE, 
    x.ticklabs = NULL, y.ticklabs = NULL, z.ticklabs = NULL, 
    y.margin.add = 0, grid = TRUE, box = TRUE, lab = par("lab"), 
    lab.z = mean(lab[1:2]), type = "p", highlight.3d = FALSE, 
    mar = c(5, 3, 4, 3) + 0.1, col.axis = par("col.axis"), col.grid = "grey", 
    col.lab = par("col.lab"), cex.symbols = par("cex"), cex.axis = 0.8 * 
        par("cex.axis"), cex.lab = par("cex.lab"), font.axis = par("font.axis"), 
    font.lab = par("font.lab"), lty.axis = par("lty"), lty.grid = par("lty"), 
    lty.hide = NULL, lty.hplot = par("lty"), log = "", ...) 
{
    x.scal <- y.scal <- z.scal <- 1
    xlabel <- if (!missing(x)) 
        deparse(substitute(x))
    ylabel <- if (!missing(y)) 
        deparse(substitute(y))
    zlabel <- if (!missing(z)) 
        deparse(substitute(z))
    if (highlight.3d && !missing(color)) 
        warning("color is ignored when highlight.3d = TRUE")
    if (!is.null(d <- dim(x)) && (length(d) == 2) && (d[2] >= 
        4)) 
        color <- x[, 4]
    else if (is.list(x) && !is.null(x$color)) 
        color <- x$color
    xyz <- xyz.coords(x = x, y = y, z = z, xlab = xlabel, ylab = ylabel, 
        zlab = zlabel, log = log)
    if (is.null(xlab)) {
        xlab <- xyz$xlab
        if (is.null(xlab)) 
            xlab <- ""
    }
    if (is.null(ylab)) {
        ylab <- xyz$ylab
        if (is.null(ylab)) 
            ylab <- ""
    }
    if (is.null(zlab)) {
        zpplab <- xyz$zlab
        if (is.null(zlab)) 
            zlab <- ""
    }
    if (length(color) == 1) 
        color <- rep(color, length(xyz$x))
    else if (length(color) != length(xyz$x)) 
        stop("length(color) ", "must be equal length(x) or 1")
    angle <- (angle%%360)/90
    yz.f <- scale.y * abs(if (angle < 1) angle else if (angle > 
        3) angle - 4 else 2 - angle)
    yx.f <- scale.y * (if (angle < 2) 
        1 - angle
    else angle - 3)
    if (angle > 2) {
        temp <- xyz$x
        xyz$x <- xyz$y
        xyz$y <- temp
        temp <- xlab
        xlab <- ylab
        ylab <- temp
        temp <- xlim
        xlim <- ylim
        ylim <- temp
    }
    angle.1 <- (1 < angle && angle < 2) || angle > 3
    angle.2 <- 1 <= angle && angle <= 3
    dat <- cbind(as.data.frame(xyz[c("x", "y", "z")]), col = color)
    if (!is.null(xlim)) {
        xlim <- range(xlim)
        dat <- dat[xlim[1] <= dat$x & dat$x <= xlim[2], , drop = FALSE]
    }
    if (!is.null(ylim)) {
        ylim <- range(ylim)
        dat <- dat[ylim[1] <= dat$y & dat$y <= ylim[2], , drop = FALSE]
    }
    if (!is.null(zlim)) {
        zlim <- range(zlim)
        dat <- dat[zlim[1] <= dat$z & dat$z <= zlim[2], , drop = FALSE]
    }
    n <- nrow(dat)
    if (n < 1) 
        stop("no data left within (x|y|z)lim")
    y.range <- range(dat$y[is.finite(dat$y)])
    if (type == "p" || type == "h") {
        y.ord <- rev(order(dat$y))
        dat <- dat[y.ord, ]
        if (length(pch) > 1) 
            if (length(pch) != length(y.ord)) 
                stop("length(pch) ", "must be equal length(x) or 1")
            else pch <- pch[y.ord]
        if (length(cex.symbols) > 1) 
            if (length(cex.symbols) != length(y.ord)) 
                stop("length(cex.symbols) ", "must be equal length(x) or 1")
            else cex.symbols <- cex.symbols[y.ord]
        daty <- dat$y
        daty[!is.finite(daty)] <- mean(daty[is.finite(daty)])
        if (highlight.3d && !(all(diff(daty) == 0))) 
            dat$col <- rgb(seq(0, 1, length = n) * (y.range[2] - 
                daty)/diff(y.range), g = 0, b = 0)
    }
    p.lab <- par("lab")
    y.range <- range(dat$y[is.finite(dat$y)], ylim)
    y.prty <- pretty(y.range, n = lab[2], min.n = max(1, min(0.5 * 
        lab[2], p.lab[2])))
    y.scal <- round(diff(y.prty[1:2]), digits = 12)
    y.add <- min(y.prty)
    dat$y <- (dat$y - y.add)/y.scal
    y.max <- (max(y.prty) - y.add)/y.scal
    if (!is.null(ylim)) 
        y.max <- max(y.max, ceiling((ylim[2] - y.add)/y.scal))
    x.range <- range(dat$x[is.finite(dat$x)], xlim)
    x.prty <- pretty(x.range, n = lab[1], min.n = max(1, min(0.5 * 
        lab[1], p.lab[1])))
    x.scal <- round(diff(x.prty[1:2]), digits = 12)
    dat$x <- dat$x/x.scal
    x.range <- range(x.prty)/x.scal
    x.max <- ceiling(x.range[2])
    x.min <- floor(x.range[1])
    if (!is.null(xlim)) {
        x.max <- max(x.max, ceiling(xlim[2]/x.scal))
        x.min <- min(x.min, floor(xlim[1]/x.scal))
    }
    x.range <- range(x.min, x.max)
    z.range <- range(dat$z[is.finite(dat$z)], zlim)
    z.prty <- pretty(z.range, n = lab.z, min.n = max(1, min(0.5 * 
        lab.z, p.lab[2])))
    z.scal <- round(diff(z.prty[1:2]), digits = 12)
    dat$z <- dat$z/z.scal
    z.range <- range(z.prty)/z.scal
    z.max <- ceiling(z.range[2])
    z.min <- floor(z.range[1])
    if (!is.null(zlim)) {
        z.max <- max(z.max, ceiling(zlim[2]/z.scal))
        z.min <- min(z.min, floor(zlim[1]/z.scal))
    }
    z.range <- range(z.min, z.max)
    if (angle.2) {
        x1 <- x.min + yx.f * y.max
        x2 <- x.max
    }
    else {
        x1 <- x.min
        x2 <- x.max + yx.f * y.max
    }
    temp <- strwidth(format(rev(y.prty))[1], cex = cex.axis/par("cex"))
    if (angle.2) 
        x1 <- x1 - temp - y.margin.add
    else x2 <- x2 + temp + y.margin.add




    i <- x.min:x.max
    list1 = list(i, z.min, i + (yx.f * y.max), yz.f * y.max + 
      z.min, col = col.grid, lty = lty.grid)
   return(list(i,x.ticklabs <- format(i * x.scal)))
  }






###
### used with noisification / denoisification to construct rf_formula
###
GetFormula = function(){
  data1_features = names(data1)[grep("features.*",names(data1))]
  to_remove = c("features.n_points","features.source_id",
    "features.max_slope","features.min",
    "features.linear_trend","features.max",
    "features.weighted_average","features.median")
  data1_features = data1_features[!(data1_features %in%
    to_remove)]
  rf_formula = formula(paste("sources.classification ~ ",
    paste(data1_features,collapse=" + ")))
  return(list(rf_formula,data1_features))
}
