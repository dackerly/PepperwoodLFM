solveForX <- function(cs,yval=0.7) {
  # y = m*x + b
  # (y-b)/m = x
  #cs <- coefficients(fit)
  return((yval-cs[1])/cs[2])
}

inversePlotData <- function(mm,cfit,pts=100)
{
  xy <- data.frame(
    x=seq(mm[1],mm[2],length.out=pts),
    y=NA,
    iy=NA)
  xy$y = cfit[1] + cfit[2]*xy$x
  xy$iy = 1/xy$y
  names(xy) <- c('MWP','iLFM','LFM')
return(xy)
}

# test function
# xy <- inversePlotData(c(-8,0),c(1,-0.15))
# head(xy)
# plot(iy~x,data=xy,type='l')
