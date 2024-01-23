## conceptual model
source('scripts/lfm_functions.R')
spres <- read.csv('results/species-results.csv')
head(spres)
slp <- -0.198

#MWPvec <- seq(-6,0,length.out=25)
ilfm.sat.vec <- seq(0.25,1.25,length.out=25)
MWPcrit <- c()
for (i in 1:length(ilfm.sat.vec)) MWPcrit[i] <- solveForX(c(ilfm.sat.vec[i],slp),1/0.7)
lfm.sat.vec <- 1/ilfm.sat.vec
plot(MWPcrit,lfm.sat.vec,xlim=c(-8,0),ylim=c(0,4),type='l',lwd=2,xlab='Minimum MWP (MPa)',ylab='LFM @ MWP=0')

for (i in 1:length(ilfm.sat.vec)) MWPcrit[i] <- solveForX(c(ilfm.sat.vec[i],slp),1/0.5)
lfm.sat.vec <- 1/ilfm.sat.vec
lines(MWPcrit,lfm.sat.vec,xlim=c(-8,0),ylim=c(0,4),lwd=1)

for (i in 1:length(ilfm.sat.vec)) MWPcrit[i] <- solveForX(c(ilfm.sat.vec[i],slp),1/1)
lfm.sat.vec <- 1/ilfm.sat.vec
lines(MWPcrit,lfm.sat.vec,xlim=c(-8,0),ylim=c(0,4),lwd=1)

#points(I(1/model1.int)~minWP,data=spres[which(spres$study=='Coastal'),],pch=19)
text(I(1/model1.int)~minWP,data=spres[which(spres$study=='Coastal'),],labels=letters)

# conceptual figure, in inverted space - need to flip to LFM# conceptual figulettersre, in inverted space - need to flip to LFM
lfm.sat <- c(0.9,0.5)
slp <- -0.2
min.MWP <- c(-6,-3)

xy <- inversePlotData(c(min.MWP[1],0),c(lfm.sat[1],slp))
head(xy)
plot(xy$MWP,xy$LFM)

(MWPcrit <- solveForX(c(lfm.sat[2],slp),1/0.7))

(max.iLFM <- lfm.sat + min.MWP*slp)
(min.LFM <- 1/max.iLFM)

op=par(mfrow=c(1,2))
plot(c(-7,0),c(0,3),type='n',xlab='MWP',ylab='1/LFM')
x <- list()
y <- list()
for (i in 1:2) {
  x[[i]] <- c(0,min.MWP[i])
  y[[i]] <- c(lfm.sat[i],max.iLFM[i])
  lines(x[[i]],y[[i]],type='b',cex=2,pch=c(1,19))
}

xy1 <- inversePlotData(c(0,min.MWP[1]),c(lfm.sat[1],slp))
xy2 <- inversePlotData(c(0,min.MWP[2]),c(lfm.sat[2],slp))
plot(c(-7,0),c(0,3),type='n',xlab='MWP',ylab='LFM')
lines(xy1$MWP,xy1$LFM,type='l')
head(xy1)
points(xy1[c(1,100),c('MWP','LFM')],cex=2,pch=c(1,19))
lines(xy2$MWP,xy2$LFM,type='l')
points(xy2[c(1,100),c('MWP','LFM')],cex=2,pch=c(1,19))
par(op)


# now plot minLFM as a matrix, as a function of lfm.sat and min.MWP
minLFM <- matrix(NA,100,100)
lfm.sat.v <- seq(0.5,2,length.out=nrow(minLFM))
min.MPW.v <- seq(-3,-8,length.out=ncol(minLFM))

for (i in 1:nrow(minLFM)) for (j in 1:ncol(minLFM)) minLFM[i,j] <- lfm.sat.v[i] + min.MPW.v[j] * slp
image(1/minLFM)

