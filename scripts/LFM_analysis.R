## preliminary analysis of 2021 LFM data
rm(list=ls())
library(ggplot2)

lw <- read.csv('data/PWD_Oct2021_LFM_WP_Calcs.csv',as.is=T)
str(lw)

# Quick look!
plot(lw$Midday.mean,lw$Bulk.LFM)

# plotting inverse of LFM makes it fairly linear
lw$iBulk.LFM <- 1/lw$Bulk.LFM
plot(lw$Midday.mean,1/lw$iBulk.LFM,log='')

ifit <- lm(iBulk.LFM~Midday.mean,data=lw)

gploti <- ggplot(lw) + 
  geom_point(aes(x=Midday.mean, y=iBulk.LFM,color=Species)) +
  geom_abline(slope=ifit$coefficients[2],intercept=ifit$coefficients[1]) + 
  labs(x='Midday mean water potential (MPa)',y='Inverse of live fuel moisture') 

print(gplot)
print(gploti) + geom_hline(yintercept=1/0.7)
gplot + facet_wrap(~Species,ncol=4)

# not inverse
fit <- lm(Bulk.LFM~Midday.mean,data=lw)

gplot <- ggplot(lw) + 
  geom_point(aes(x=Midday.mean, y=Bulk.LFM,color=Species)) +
  geom_abline(slope=fit$coefficients[2],intercept=fit$coefficients[1]) + 
  labs(x='Midday mean water potential (MPa)',y='Live fuel moisture (%)') 

print(gplot)
print(gplot) + geom_hline(yintercept=0.7)

gplot + geom_hline(yintercept=0.7) + facet_wrap(~Species,ncol=4)

objplot(iBulk.LFM~Midday.mean,data=lw,pch=19)
abline(fit)

## barplots of species traits
(bulkLFM <- tapply(lw$Bulk.LFM,lw$Species,mean,na.rm=T))

spMeans <- data.frame(BulkLFM=bulkLFM,LeafLFM=NA,StemLFM=NA,PredawnWP=NA,MiddayWP=NA,WPdiff=NA)
rownames(spMeans) <- names(bulkLFM)
spMeans$LeafLFM <- tapply(lw$Leaf.LFM,lw$Species,mean,na.rm=T)
spMeans$StemLFM <- tapply(lw$Stem.LFM,lw$Species,mean,na.rm=T)
spMeans$PredawnWP <- tapply(lw$Predawn.mean,lw$Species,mean,na.rm=T)
spMeans$MiddayWP <- tapply(lw$Midday.mean,lw$Species,mean,na.rm=T)
spMeans$WPdiff <- tapply(lw$WPdiff,lw$Species,mean,na.rm=T)
spMeans <- spMeans[order(spMeans$BulkLFM),]

op=par(mfrow=c(2,3))
barplot(spMeans$BulkLFM,main='Bulk LFM')
barplot(spMeans$LeafLFM,main='Leaf LFM')
barplot(spMeans$StemLFM,main='Stem LFM')
barplot(spMeans$PredawnWP,main='Predawn WP')
barplot(spMeans$MiddayWP,main='Midday WP')
barplot(spMeans$WPdiff,main='WP differential')
par(op)

rownames(spMeans)
spMeans$spCodes <- c('Cc','Af','Qr','Qd','Cp','Qg','Qa','Qk','Ha','Uc','Am','Pm','Bp')

pairs(spMeans)
plot(spMeans$MiddayWP,spMeans$BulkLFM,type='n')
text(spMeans$MiddayWP,spMeans$BulkLFM,labels=spMeans$spCodes)
