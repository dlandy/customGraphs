
#' plot.simplex.noah
#' 
#' This function is by Noah Silbert
#' designed to prettily plot on the surfaces of simplexes.
#' It does not allow you to set colors
#' @param x a vector of values between 0 and 1, representing the weight on the first dimension.
#' @param y a vector of values between 0 and 1, representing the weight on the second dimension.
#' @param z a vector of values between 0 and 1, representing the weight on the third dimension.
#' @param a 
#' @param x3
#' @param y3.off
#' @param new.plot
#' @param xlim
#' @param ylim
#' @param xlab
#' @param ylab
#' @param zlab
#' @return plots a pretty graph
#' @export
plot.simplex.noah <- function(theta, data, a = 1, x3 = .5, y3.off = 0,
                              new.plot = T, xlm = c(0,1), ylm = c(0,1),
                              xlab="(1,0,0)", ylab="(0,1,0)", zlab="(0,0,1)"){
  
  y3 = .5*(1-a*sqrt(3)/2) + y3.off
  x1 = x3-.5*a;
  x2 = x3+.5*a;
  y1 = y3 + a*sqrt(3)/2;
  y2 = y3 + a*sqrt(3)/2;
  
  nr = nrow(theta)
  nc = ncol(theta)
  rtot = vector(length=nr)
  clrm = vector(length=nr)
  if(new.plot){
    plot.new()
  }
  plot.window(xlim=xlm,ylim=ylm)
  for(i in 1:nr){
    #    rtot[i] = sum(data[i,])
    #    if(rtot[i]>1){
    #      ctemp = data[i,]/sum(data[i,])
    #    }else{
    #      ctemp = data[i,]
    #    }
    #    clrm[i] = rgb(ctemp[1],ctemp[2],ctemp[3])
    clrm[i] = rgb(data[i,1], data[i,2], data[i,3])
    xi = x3-theta[i,1]*a/2+theta[i,2]*a/2
    yi = y3+sum(theta[i,1:2])*a*sqrt(3)/2
    points(xi,yi,pch=19,col=clrm[i])
  }
  lines(c(x3,x1),c(y3,y1),lty=2)
  lines(c(x3,x2),c(y3,y2),lty=2)
  lines(c(x1,x2),c(y1,y2),lty=2)
  # maybe need to make cex values flexible
  text(x1-0.02*a,y1-0.01*a,xlab,pos=3,cex=1.25)
  text(x2+0.02*a,y2-0.01*a,ylab,pos=3,cex=1.25)
  text(x3,y3,zlab,pos=1,cex=1.25)
}


#' plot.simplex.noah2
#' 
#' This function is by Noah Silbert
#' designed to prettily plot on the surfaces of simplexes.
#' It does not allow you to set colors
#' @param x a vector of values between 0 and 1, representing the weight on the first dimension.
#' @param y a vector of values between 0 and 1, representing the weight on the second dimension.
#' @param z a vector of values between 0 and 1, representing the weight on the third dimension.
#' @param a 
#' @param x3
#' @param y3.off
#' @param new.plot
#' @param xlim
#' @param ylim
#' @param xlab
#' @param ylab
#' @param zlab
#' @return plots a pretty graph
#' @export
plot.simplex.noah.2 <- function(theta, data, a = 1, x3 = .5, y3.off = 0,
                                new.plot = T, xlm = c(0,1), ylm = c(0,1),
                                pch=19, alpha=.8, cex=1.25, lex=.85, tscl=1,
                                labs=c("(1,0,0)","(0,1,0)","(0,0,1)")){
  
  y3 = .5*(1-a*sqrt(3)/2) + y3.off
  x1 = x3-.5*a;
  x2 = x3+.5*a;
  y1 = y3 + a*sqrt(3)/2;
  y2 = y3 + a*sqrt(3)/2;
  
  nr = nrow(theta)
  nc = ncol(theta)
  rtot = vector(length=nr)
  clrm = vector(length=nr)
  if(new.plot){
    plot.new()
  }
  plot.window(xlim=xlm,ylim=ylm)
  for(i in 1:nr){
    rtot[i] = sum(data[i,])
    if(rtot[i]>1){
      ctemp = data[i,]/sum(data[i,])
    }else{
      ctemp = data[i,]
    }
    ctemp = ctemp/tscl
    clrm[i] = rgb(ctemp[1],ctemp[2],ctemp[3],alpha)
    
    xi = x3-theta[i,1]*a/2+theta[i,2]*a/2
    yi = y3+sum(theta[i,1:2])*a*sqrt(3)/2
    if(length(pch)==1){
      points(xi,yi,pch=pch,col=clrm[i],cex=cex)
    }else if(length(pch)==3){
      ridx = which.max(data[i,])
      points(xi,yi,pch=pch[ridx],col=clrm[i],cex=cex)
    }
  }
  lines(c(x3,x1),c(y3,y1),lty=2)
  lines(c(x3,x2),c(y3,y2),lty=2)
  lines(c(x1,x2),c(y1,y2),lty=2)
  # maybe need to make cex values flexible
  text(x1,y1,labs[1],pos=3,cex=lex)#.75)
  text(x2,y2,labs[2],pos=3,cex=lex)#.75)
  text(x3,y3,labs[3],pos=1,cex=lex)#.75)
}




#' plot.simplex
#' 
#' This function adapts code by Noah Silbert, 
#' designed to prettily plot on the surfaces of simplexes.
#' It allows you to set colors
#' @param x a vector of values between 0 and 1, representing the weight on the first dimension.
#' @param y a vector of values between 0 and 1, representing the weight on the second dimension.
#' @param z a vector of values between 0 and 1, representing the weight on the third dimension.
#' @param color the color to scale everything in terms of
#' @param a 
#' @param x3
#' @param y3.off
#' @param new.plot
#' @param xlim
#' @param ylim
#' @param xlab
#' @param ylab
#' @param zlab
#' @param colorOption legal values include "color" and "grayscale"
#' @return a pretty graph
#' @export
plot.simplex <- function(x, y, z, color=0, a=1, x3=0.5, y3.off=0, new.plot=T, xlim=c(0,1), ylim=c(0,1),
                         xlab="Center", ylab="Simple", zlab="Dual", colorOption="color"){
  # x, y, and z are just vectors
  t = matrix(nrow=length(x), ncol=3)
  t[,1] <- x
  t[,2] <- y
  t[,3] <- z
  
  da = matrix(nrow=length(x),ncol=3)
  if(colorOption=="color"){
    da[,1] = (color*248+(1-color)*0)/255    #color
    da[,2] = (color*109+(1-color)*196)/255    #color
    da[,3] = (color*118+(1-color)*191)/255    #color
  } else if(colorOption=="grayScale"){
    da[,1] = color    #color
    da[,2] = color   #color
    da[,3] = color    #color
    
  }
  plot.simplex.noah(t, da, a, x3, y3.off, new.plot, xlim, ylim, xlab, ylab, zlab)
  
}




#' plot.interaction
#' 
#' This function adapts code by Noah Silbert, 
#' designed to prettily plot on the surfaces of simplexes.
#' It allows you to set colors
#' @param x a vector of values between 0 and 1, representing the weight on the first dimension.
#' @param y a vector of values between 0 and 1, representing the weight on the second dimension.
#' @param z a vector of values between 0 and 1, representing the weight on the third dimension.
#' @param color the color to scale everything in terms of
#' @param a 
#' @param x3
#' @param y3.off
#' @param new.plot
#' @param xlim
#' @param ylim
#' @param xlab
#' @param ylab
#' @param zlab
#' @param colorOption legal values include "color" and "grayscale"
#' @return a pretty graph
#' @export
plot.interaction <- function (x.factor, trace.factor, response, fun = mean, type = c("l", 
                                                                                     "p", "b"), legend = TRUE, trace.label = deparse(substitute(trace.factor)), 
                              fixed = FALSE, xlab = deparse(substitute(x.factor)), ylab = ylabel, 
                              xlim = c(0,0),
                              ylim = range(cells, na.rm = TRUE), lty = nc:1, col = 1, pch = c(1L:9, 
                                                                                              0, letters), xpd = NULL, leg.bg = par("bg"), leg.bty = "n", 
                              xtick = FALSE, xaxt = par("xaxt"), axes = TRUE, ...) 
{
  ylabel <- paste(deparse(substitute(fun)), "of ", deparse(substitute(response)))
  type <- match.arg(type)
  cells <- tapply(response, list(x.factor, trace.factor), fun)
  nr <- nrow(cells)
  nc <- ncol(cells)
  xvals <- 1L:nr
  xvals <- sort(unique(x.factor))
  if (is.ordered(x.factor)) {
    wn <- getOption("warn")
    options(warn = -1)
    xnm <- as.numeric(levels(x.factor))
    options(warn = wn)
    if (!any(is.na(xnm))) 
      xvals <- xnm
  }
  xlabs <- rownames(cells)
  ylabs <- colnames(cells)
  nch <- max(sapply(ylabs, nchar, type = "width"))
  if (is.null(xlabs)) 
    xlabs <- as.character(xvals)
  if (is.null(ylabs)) 
    ylabs <- as.character(1L:nc)
  if(xlim[1]==0 & xlim[2]==0){
    xlim <- range(xvals)
    xlim <- xlim + c(-0.2/nr, if (legend) 0.2 + 0.02 * nch else 0.2/nr) * 
      diff(xlim)
  }
  xleg <- xlim[1] + 0.05 * diff(xlim)
  xleg <- xlim[1] + 0.05 * diff(xlim)
  matplot(xvals, cells, ..., type = type, xlim = xlim, ylim = ylim, 
          xlab = xlab, ylab = ylab, axes = axes, xaxt = "n", col = col, 
          lty = lty, pch = pch)
  if (axes && xaxt != "n") {
    axisInt <- function(x, main, sub, lwd, bg, log, asp, 
                        ...) axis(1, x, ...)
    mgp. <- par("mgp")
    if (!xtick) 
      mgp.[2L] <- 0
    axisInt(1, at = xvals, labels = xlabs, tick = xtick, 
            mgp = mgp., xaxt = xaxt, ...)
  }
  if (legend) {
    yrng <- diff(ylim)
    yleg <- ylim[2L] - 0.1 * yrng
    if (!is.null(xpd) || {
      xpd. <- par("xpd")
      !is.na(xpd.) && !xpd. && (xpd <- TRUE)
    }) {
      op <- par(xpd = xpd)
      on.exit(par(op))
    }
    text(xleg, ylim[2L] - 0.05 * yrng, paste("  ", trace.label), 
         adj = 0)
    if (!fixed) {
      ord <- sort.list(cells[nr, ], decreasing = TRUE)
      ylabs <- ylabs[ord]
      lty <- lty[1 + (ord - 1)%%length(lty)]
      col <- col[1 + (ord - 1)%%length(col)]
      pch <- pch[ord]
    }
    legend(xleg, yleg, legend = ylabs, col = col, pch = if (type %in% 
                                                            c("p", "b")) 
      pch, lty = if (type %in% c("l", "b")) 
        lty, bty = leg.bty, bg = leg.bg)
  }
  invisible()
}



