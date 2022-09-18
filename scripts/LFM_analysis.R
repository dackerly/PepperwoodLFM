## preliminary analysis of 2021 LFM data
rm(list=ls())
library(ggplot2)

lw <- read.csv('data/PWD_Oct2021+Jun2021_LFM_WP_Calcs.csv',as.is=T)
str(lw)

# for analysis, remove one sample of Baccharis that had no LFM
lw <- lw[-which(is.na(lw$Bulk.LFM)),]
lw$Species[which(lw$Species=='CEATHY')] <- 'CEAPAR'
lw$Species[which(lw$Species=='QUEAGRI')] <- 'QUEAGR'

# remove two bad LFM values
lw$Bulk.LFM[c(48,51)] <- NA

# remove BACPIL
lw <- lw[-which(lw$Species=='BACPIL'),]

# sample sizes
table(lw$Species)
write.csv(table(lw$Species,lw$Site),'results/sampletable.csv')
table(lw$Species,lw$Site)
nrow(lw)

# ID rows
rOct <- which(lw$Sampling=='Oct21')
rJun <- which(lw$Sampling=='Jun22')

# Quick look!
plot(lw$Midday.mean,lw$Bulk.LFM,pch=19,col='red',ylim=c(0,2.5),xlab='Midday water potential (MPa)',ylab='LFM')
abline(h=0.7,lty=2)
points(lw$Midday.mean[rJun],lw$Bulk.LFM[rJun],pch=19,col='black')

# how much of fall variation due to WP
cor(lw$Midday.mean[rJun],lw$Bulk.LFM[rJun])
cor(lw$Midday.mean[rOct],lw$Bulk.LFM[rOct],use='pair')
cor(lw$Midday.mean,lw$Bulk.LFM,use='pair')

table(lw$Species,lw$Sampling)
length(which(lw$Sampling=='Jun22'))


# plotting inverse of LFM makes it fairly linear
lw$iBulk.LFM <- 1/lw$Bulk.LFM
plot(lw$Midday.mean,lw$iBulk.LFM,log='')

# plot all data with lines by species
plot(iBulk.LFM~Midday.mean,data=lw)
abline(h=1/0.7,lty=2)
(spp <- sort(unique(lw$Species[which(lw$Sampling=='Jun22')])))
for (i in 1:length(spp))
{
  r1 <- which(lw$Species==spp[i])
  fit <- lm(iBulk.LFM[r1]~Midday.mean[r1],data=lw)
  abline(fit)
}

# solve lm for chosen y val
solveForX <- function(fit,yval=0.7) {
  # y = m*x + b
  # (y-b)/m = x
  cs <- coefficients(fit)
  return((yval-cs[1])/cs[2])
}

## show individual species
par(mar=c(5,5,3,1))
spSel <- c('QUEDUR')
r1 <- which(lw$Species==spSel)
plot(lw$Midday.mean[r1],lw$Bulk.LFM[r1],pch=19,main=spSel[1])
abline(h=0.7,lty=2)
fit <- lm(lw$Bulk.LFM[r1]~lw$Midday.mean[r1])
abline(fit)
solveForX(fit)

plot(lw$Midday.mean[r1],lw$iBulk.LFM[r1],pch=19,main=spSel[1])
abline(h=1/0.7,lty=2)
fit <- lm(lw$iBulk.LFM[r1]~lw$Midday.mean[r1])
abline(fit)
solveForX(fit,1/0.7)

## compare to Pivovaroff study
p <- read.csv('data/other_studies/Pivovaroff_WP_vs_LFM.csv',as.is=T)
head(p)
p$bulkLFM <- p$LFM_./100
table(p$Species)

m <- read.csv('data/other_studies/seki_lfm_mpa_09062022.csv')
head(m)
dim(m)
table(m$spp)
m$lfm <- m$lfm/100
summary(m$lfm)
summary(m$mpa)

minX <- min(c(m$mpa,p$WP_md_MPa,lw$Midday.mean),na.rm=T)
maxX <- max(c(m$mpa,p$WP_md_MPa,lw$Midday.mean),na.rm=T)

par(mar=c(5,5,1,1))
plot(p$WP_md_MPa,p$bulkLFM,pch=19, xlab='Midday water potential (MPa)',ylab='Bulk live fuel moisture',cex.lab=2,xlim=c(minX,maxX))
points(m$mpa,m$lfm,pch=1,col='darkgreen',cex=1.5)
points(lw$Midday.mean,lw$Bulk.LFM,pch=19,col='red',cex=1.5)

legend(-7.2,2.5,legend = c('Pivovaroff','Boving/Moritz','Pepperwood'),fill = c('black','darkgreen','red'),cex = 1.5)


## Combine our data and P for individual species
par(mar=c(5,5,3,1))
spSel <- c('ADEFAS','ADFA')
spSel <- c('HETARB','HEAR')
spSel <- c('QUEAGR','QUAG')

r1 <- which(lw$Species==spSel[1])
r2 <- which(p$Species==spSel[2])
xlims <- c(min(c(lw$Midday.mean[r1],p$WP_md_MPa[r2]),na.rm=T),max(c(lw$Midday.mean[r1],p$WP_md_MPa[r2]),na.rm=T))
ylims <- c(min(c(lw$Bulk.LFM[r1],p$bulkLFM[r2]),na.rm=T),max(c(lw$Bulk.LFM[r1],p$bulkLFM[r2]),na.rm=T))
plot(lw$Midday.mean[r1],lw$Bulk.LFM[r1],xlim=xlims,ylim=ylims,pch=19,main=spSel[1],xlab='Midday water potential (MPa)',ylab='Life fuel moisture',cex=2)
points(p$WP_md_MPa[r2],p$bulkLFM[r2],col='red',pch=19,cex=2)

## Combine our data and m for individual species
par(mar=c(5,5,3,1))
spSel <- c('QUEKEL','QUKE')

r1 <- which(lw$Species==spSel[1])
r2 <- which(m$spp==spSel[2])
xlims <- c(min(c(lw$Midday.mean[r1],m$mpa[r2]),na.rm=T),max(c(lw$Midday.mean[r1],m$mpa[r2]),na.rm=T))
ylims <- c(min(c(lw$Bulk.LFM[r1],m$lfm[r2]),na.rm=T),max(c(lw$Bulk.LFM[r1],m$lfm[r2]),na.rm=T))
plot(lw$Midday.mean[r1],lw$Bulk.LFM[r1],xlim=xlims,ylim=ylims,pch=19,main=spSel[1],cex=2)
points(m$mpa[r2],m$lfm[r2],col='darkgreen',cex=2)

## barplots of species traits - by season
lw$SpecSeason <- paste(lw$Species,lw$Sampling,sep='-')

(bulkLFM <- tapply(lw$Bulk.LFM,lw$SpecSeason,mean,na.rm=T))

spMeans <- data.frame(Species=substr(names(bulkLFM),1,6),Season=substr(names(bulkLFM),8,12),BulkLFM=bulkLFM,LeafLFM=NA,StemLFM=NA,PredawnWP=NA,MiddayWP=NA,WPdiff=NA)
rownames(spMeans) <- names(bulkLFM)
spMeans$LeafLFM <- tapply(lw$Leaf.LFM,lw$SpecSeason,mean,na.rm=T)
spMeans$StemLFM <- tapply(lw$Stem.LFM,lw$SpecSeason,mean,na.rm=T)
spMeans$PredawnWP <- tapply(lw$Predawn.mean,lw$SpecSeason,mean,na.rm=T)
spMeans$MiddayWP <- tapply(lw$Midday.mean,lw$SpecSeason,mean,na.rm=T)
spMeans$WPdiff <- tapply(lw$WPdiff,lw$SpecSeason,mean,na.rm=T)
rownames(spMeans)

spMeans
spMeans2 <- spMeans[order(spMeans$Season),]
spOrder <- match(c('CEACUN','CEAPAR','ADEFAS','HETARB','QUEDUR','UMBCAL','ARBMEN','PSEMEN','QUEAGR','QUEKEL','QUEDOU','QUEGAR'),spMeans2$Species)
spOrder <- c(spOrder,spOrder+12)
(spMeans3 <- spMeans2[spOrder,])

barplot(spMeans3$MiddayWP[1:12],ylim=c(-6.5,0))
barplot(spMeans3$MiddayWP[13:24])

barplot(spMeans$BulkLFM[1:12],ylim=c(0,1.8))
barplot(spMeans$BulkLFM[13:24],ylim=c(0,1.8))
abline(h=0.7,lty=2)

## how much variation in fall due to WP
cor(spMeans3$MiddayWP[13:24],spMeans$BulkLFM[13:24])

plot(MiddayWP~PredawnWP,data=spMeans[which(spMeans$Season=='Oct21'),],xlim=c(-5,0),ylim=c(-6.5,0))
(fit <- lm(MiddayWP~PredawnWP,data=spMeans))
abline(0,1,lty=2)
abline(fit)

plot(BulkLFM~MiddayWP,data=spMeans[which(spMeans$Season=='Oct21'),],type='n',xlim=c(-7,-2))
text(spMeans$MiddayWP[which((spMeans$Season=='Oct21'))],spMeans$BulkLFM[(spMeans$Season=='Oct21')],spMeans$Species[(spMeans$Season=='Oct21')])

spNm <- sort(unique(spMeans$Species))
plot(BulkLFM~MiddayWP,data=spMeans,type='n',xlim=c(-7,-0.5))
text(spMeans$MiddayWP[which((spMeans$Season=='Jun22'))],spMeans$BulkLFM[(spMeans$Season=='Jun22')],spMeans$Species[(spMeans$Season=='Jun22')],col='black',cex=0.75)
i=1
for (i in 1:length(spNm)) {
  tmp <- spMeans[which(spMeans$Species==spNm[i]),]
  lines(tmp$MiddayWP,tmp$BulkLFM)
}
text(spMeans$MiddayWP[which((spMeans$Season=='Oct21'))],spMeans$BulkLFM[(spMeans$Season=='Oct21')],spMeans$Species[(spMeans$Season=='Oct21')],col='red')


abline(h=0.7,lty=2)

plot(BulkLFM~PredawnWP,data=spMeans[which(spMeans$Season=='Oct21'),],type='n',xlim=c(-5,-0.5),ylim=c(0.5,1.25))
text(spMeans$PredawnWP[which((spMeans$Season=='Oct21'))],spMeans$BulkLFM[(spMeans$Season=='Oct21')],spMeans$Species[(spMeans$Season=='Oct21')])
abline(h=0.7,lty=2)

spMeans <- spMeans[order(spMeans$BulkLFM),]

rownames(spMeans)
#spMeans$sp2 <- c('Cc','Af','Qr','Qd','Cp','Qg','Qa','Qk','Ha','Uc','Am','Pm')
#spMeans$sp6 <- c('Ceacun','Adefas','Quedur','Quedou','Ceapar','Quegar','Queagr','Quekel','Hetarb','Umbcal','Arbmen','Psemen','Bacpil')

op=par(mfrow=c(2,3),mar=c(4,4,2,1))
barplot(spMeans$BulkLFM,main='Bulk LFM',names.arg = spMeans$sp2)
barplot(spMeans$LeafLFM,main='Leaf LFM',names.arg = spMeans$sp2)
barplot(spMeans$StemLFM,main='Stem LFM',names.arg = spMeans$sp2)
barplot(spMeans$MiddayWP,main='Midday WP',names.arg = spMeans$sp2, ylim=c(-6.5,0))
barplot(spMeans$PredawnWP,main='Predawn WP',names.arg = spMeans$sp2, ylim=c(-6.5,0))
barplot(spMeans$WPdiff,main='WP differential',names.arg = spMeans$sp2)
par(op)

pairs(spMeans[,1:6])
plot(spMeans$MiddayWP,spMeans$BulkLFM,type='n')
text(spMeans$MiddayWP,spMeans$BulkLFM,labels=spMeans$sp6)


### GGPLOT ANALYSES
# Inverse analyses

ifit <- lm(iBulk.LFM~Midday.mean,data=lw)
abline(ifit)

gploti <- ggplot(lw) + 
  geom_point(aes(x=Midday.mean, y=iBulk.LFM,color=Species)) +
  geom_abline(slope=ifit$coefficients[2],intercept=ifit$coefficients[1]) + 
  labs(x='Midday mean water potential (MPa)',y='Inverse of live fuel moisture') 

print(gploti)
print(gploti) + geom_hline(yintercept=1/0.7)
gploti + facet_wrap(~Species,ncol=4)

# analog to PV curves
plot(I(-1/(lw$Midday.mean))~I(1-lw$Bulk.LFM))
#plot(I(-1/(lw$Midday.mean))~I(1-lw$Bulk.LFM))


# not inverse analyses
xbp <- which(lw$Species=='BACPIL')

#choose whether to use all points or without bacpil
xx <- lw
xx <- lw[-xbp,]

fit <- lm(Bulk.LFM~Midday.mean,data=xx)

gplot <- ggplot(xx) + 
  geom_point(aes(x=Midday.mean, y=Bulk.LFM,color=Species),size=4) +
  geom_abline(slope=fit$coefficients[2],intercept=fit$coefficients[1]) + 
  labs(x='Midday mean water potential (MPa)',y='Live fuel moisture (%)') 

print(gplot)
print(gplot) + geom_hline(yintercept=0.7)

gplot + facet_wrap(~Species,ncol=4)

plot(Bulk.LFM~Midday.mean,data=lw,pch=19)
abline(fit)


