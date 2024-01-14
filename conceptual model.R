## conceptual model
lfm.sat <- c(0.9,0.5)
slp <- -0.15
min.MWP <- c(-6,-3)

(max.iLFM <- lfm.sat + min.MWP*slp)
(min.LFM <- 1/max.iLFM)

plot(c(-7,0),c(0,2),type='n')
x <- list()
y <- list()
for (i in 1:2) {
  x[[i]] <- c(0,min.MWP[i])
  y[[i]] <- c(lfm.sat[i],max.iLFM[i])
  lines(x[[i]],y[[i]],type='b',cex=2,pch=c(1,19))
}


lines(c(x1,x2),c(y1,y2))
