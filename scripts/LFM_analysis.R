## preliminary analysis of 2021 LFM data
rm(list=ls())
library(ggplot2)

lw <- read.csv('data/PWD_Oct2021_LFM_WP_Calcs.csv',as.is=T)
str(lw)

# for analysis, remove one sample of Baccharis that had no LFM
lw <- lw[-which(is.na(lw$Bulk.LFM)),]

# sample sizes
table(lw$Species)
write.csv(table(lw$Species,lw$Site),'results/sampletable.csv')
table(lw$Species,lw$Site)
nrow(lw)

# Quick look!
plot(lw$Midday.mean,lw$Bulk.LFM)

## compare to Pivovaroff study
p <- read.csv('data/other_studies/Pivovaroff_WP_vs_LFM.csv',as.is=T)
head(p)
p$bulkLFM <- p$LFM_./100

par(mar=c(5,5,1,1))
plot(p$WP_md_MPa,p$bulkLFM,pch=19, xlab='Midday water potential (MPa)',ylab='Bulk live fuel moisture',cex.lab=2)
points(lw$Midday.mean,lw$Leaf.LFM,pch=19,col='red',cex=2)
legend(-7,2.5,legend = c('Pivovaroff','this study'),fill = c('black','red'),cex = 2)

# plotting inverse of LFM makes it fairly linear
lw$iBulk.LFM <- 1/lw$Bulk.LFM
plot(lw$Midday.mean,1/lw$iBulk.LFM,log='')

ifit <- lm(iBulk.LFM~Midday.mean,data=lw)

gploti <- ggplot(lw) + 
  geom_point(aes(x=Midday.mean, y=iBulk.LFM,color=Species)) +
  geom_abline(slope=ifit$coefficients[2],intercept=ifit$coefficients[1]) + 
  labs(x='Midday mean water potential (MPa)',y='Inverse of live fuel moisture') 

print(gploti)
print(gploti) + geom_hline(yintercept=1/0.7)
gplot + facet_wrap(~Species,ncol=4)

# not inverse
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

rownames(spMeans)
spMeans$sp2 <- c('Cc','Af','Qr','Qd','Cp','Qg','Qa','Qk','Ha','Uc','Am','Pm','Bp')
spMeans$sp6 <- c('Ceacun','Adefas','Quedur','Quedou','Ceapar','Quegar','Queagr','Quekel','Hetarb','Umbcal','Arbmen','Psemen','Bacpil')

op=par(mfrow=c(2,3),mar=c(4,4,2,1))
barplot(spMeans$BulkLFM,main='Bulk LFM',names.arg = spMeans$sp2)
barplot(spMeans$LeafLFM,main='Leaf LFM',names.arg = spMeans$sp2)
barplot(spMeans$StemLFM,main='Stem LFM',names.arg = spMeans$sp2)
barplot(spMeans$MiddayWP,main='Midday WP',names.arg = spMeans$sp2, ylim=c(-6.5,0))
barplot(spMeans$PredawnWP,main='Predawn WP',names.arg = spMeans$sp2, ylim=c(-6.5,0))
barplot(spMeans$WPdiff,main='WP differential',names.arg = spMeans$sp2)
par(op)

pairs(spMeans)
plot(spMeans$MiddayWP,spMeans$BulkLFM,type='n')
text(spMeans$MiddayWP,spMeans$BulkLFM,labels=spMeans$sp6)




