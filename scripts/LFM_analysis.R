## preliminary analysis of 2021 LFM data
rm(list=ls())
library(ggplot2)

lwa <- read.csv('data/all-data-combined.csv',as.is=T,row.names=1)
lwa$ilfm <- 1/lwa$lfm
head(lwa)

pw <- lwa[lwa$study=='Pepperwood',]
head(pw)
dim(pw)

# ID rows
rOct <- which(pw$date=='21-Oct')
rJun <- which(pw$date=='22-Jun')
length(rOct)
length(rJun)

# sample sizes
table(pw$Species[rOct])
table(pw$Species[rJun])
nrow(pw)

table(pw$Species,pw$date)

# Quick look!
plot(pw$mwp,pw$lfm,pch=19,col='red',ylim=c(0,2.5),xlab='Midday water potential (MPa)',ylab='LFM')
abline(h=0.7,lty=2)
points(pw$mwp[rJun],pw$lfm[rJun],pch=19,col='black')

# how much of fall variation due to WP
cor(pw$mwp[rJun],pw$lfm[rJun])
cor(pw$mwp[rOct],pw$lfm[rOct],use='pair')
cor(pw$mwp,pw$lfm,use='pair')

# plotting inverse of LFM makes it fairly linear
plot(pw$mwp,pw$ilfm,log='')
fit <- lm(ilfm~mwp,data=pw)
coefficients(fit)
abline(h=1/0.7,lty=2)
abline(fit)

# residual diagnostic plot on ANCOVAS with each transformation
plot(pw$ilfm~pw$mwp)
fit <- glm(ilfm~mwp+Species,data=pw)
summary(fit)
plot(fit)
BIC(fit)

# inverse with interactions for different slopes
plot(pw$ilfm~pw$mwp)
fit2 <- glm(ilfm~mwp * Species,data=pw)
summary(fit2)
#plot(fit2)
BIC(fit2) # much higher (less negative) - different slopes not supported

### now analyze each study and all
# solve lm for chosen y val
solveForX <- function(fit,yval=0.7) {
  # y = m*x + b
  # (y-b)/m = x
  cs <- coefficients(fit)
  return((yval-cs[1])/cs[2])
}

ssel <- 'StuntRanch'
ssel <- 'SEKI'
ssel <- 'Pepperwood'
ssel <- 'All'
if (ssel=='All') td <- lwa else td <- lwa[which(lwa$study==ssel),]

# plot all data with lines by species
plot(ilfm~mwp,data=td)
abline(h=1/0.7,lty=2)
fit <- glm(ilfm~mwp,data=td)
summary(fit)
abline(fit,col='red')
mean(td$mwp,na.rm=T)

(spp <- sort(unique(td$Species)))
par(mar=c(5,5,3,1))

i=1
for (i in 1:length(spp))
{
  r1 <- which(td$Species==spp[i])
  print(c(i,length(r1)))
  fit <- lm(ilfm[r1]~mwp[r1],data=td)
  cfit <- coefficients(fit)
  mm <- range(td$mwp[r1],na.rm=T)
  yy <- cfit[1] + cfit[2]*mm
  lines(mm,yy)
}

pres <- data.frame(Species=spp,N=NA,minWP=NA,intercept=NA,slope=NA,critWP=NA,LFM2.5=NA,i.intercept=NA,i.slope=NA,i.critWP=NA,i.LFM2.5=NA)
i=1
for (i in 1:length(spp))
{
  #print(i)
  spSel <- spp[i]
  temp <- td[which(td$Species==spSel),]
  temp <- temp[complete.cases(temp[,c('mwp','lfm')]),]
  pres[i,c('N')] <- nrow(temp)
  pres[i,'minWP'] <- min(temp$mwp)
  #plot(temp$mwp,temp$lfm,pch=19,main=spSel[1])
  #abline(h=0.7,lty=2)
  fit <- lm(temp$lfm~temp$mwp)
  cfit <- coefficients(fit)
  pres[i,c('intercept','slope')] <- cfit
  #abline(fit)
  pres[i,'critWP'] <- solveForX(fit)
  pres[i,'LFM2.5'] <- cfit[1] + cfit[2] * (-2.5)
  
  #plot(temp$mwp,temp$ilfm,pch=19,main=spSel[1])
  abline(h=1/0.7,lty=2)
  fit <- lm(temp$ilfm~temp$mwp)
  cfit <- coefficients(fit)
  pres[i,c('i.intercept','i.slope')] <- cfit
  #abline(fit)
  pres[i,'i.critWP'] <- solveForX(fit,1/0.7)
  pres[i,'i.LFM2.5'] <- 1/(cfit[1] + cfit[2] * (-2.5))
}
pres
write.csv(pres,paste('results/',ssel,'-species-results.csv',sep=''))

plot(pres$minWP,pres$i.intercept)
### end subset analysis

### NEED TO EDIT BELOW HERE

# now plot all species across all studies
# plot all data with lines by species
plot(ilfm~mwp,data=lwa,xlim=c(-9,0))
abline(h=1/0.7,lty=2)
fit <- glm(ilfm~mwp,data=lwa)
summary(fit)
abline(fit)
mean(td$mwp,na.rm=T)

(spp <- sort(unique(lwa$Species)))
i=1
for (i in 1:length(spp))
{
  r1 <- which(lwa$Species==spp[i])
  fit <- lm(ilfm[r1]~mwp[r1],data=lwa)
  cfit <- coefficients(fit)

  mm <- range(lwa$mwp[r1],na.rm=T)
  print(round(c(i,cfit,mm),2))
  yy <- cfit[1] + cfit[2]*mm
  lines(mm,yy,col='red')
}

# model fit
# no species
fit1 <- glm(ilfm~mwp,data=lwa)
BIC(fit1)

fit2 <- glm(ilfm~mwp+Species,data=lwa)
BIC(fit2)

fit3 <- glm(ilfm~mwp*Species,data=lwa)
BIC(fit3)

### good to here

minX <- min(c(m$water_potential,p$WP_md_MPa,pw$mwp),na.rm=T)
maxX <- max(c(m$water_potential,p$WP_md_MPa,pw$mwp),na.rm=T)

par(mar=c(5,5,1,1))
plot(p$WP_md_MPa,p$bulkLFM,pch=19, xlab='Midday water potential (MPa)',ylab='Bulk live fuel moisture',cex.lab=2,xlim=c(minX,maxX))
points(m$water_potential,m$lfm,pch=1,col='darkgreen',cex=1.5)
points(pw$mwp,pw$lfm,pch=19,col='red',cex=1.5)

legend(-7.2,2.5,legend = c('Pivovaroff','Boving/Moritz','Pepperwood'),fill = c('black','darkgreen','red'),cex = 1.5)


## Combine our data and P for individual species
op=par(mar=c(5,5,3,1),mfrow=c(2,2))
spSel <- list()
spSel[[1]] <- c('ADEFAS','ADFA')
spSel[[2]] <- c('HETARB','HEAR')
spSel[[3]] <- c('QUEAGR','QUAG')

for (i in 1:3) {
  r1 <- which(pw$Species==spSel[[i]][1])
  r2 <- which(p$Species==spSel[[i]][2])
  xlims <- c(min(c(pw$mwp[r1],p$WP_md_MPa[r2]),na.rm=T),max(c(pw$mwp[r1],p$WP_md_MPa[r2]),na.rm=T))
  ylims <- c(min(c(pw$lfm[r1],p$bulkLFM[r2]),na.rm=T),max(c(pw$lfm[r1],p$bulkLFM[r2]),na.rm=T))
  plot(pw$mwp[r1],pw$lfm[r1],xlim=xlims,ylim=ylims,pch=19,main=spSel[[i]][1],xlab='Midday water potential (MPa)',ylab='Life fuel moisture',cex=2)
  points(p$WP_md_MPa[r2],p$bulkLFM[r2],col='red',pch=19,cex=2)
}
## Combine our data and m for individual species
spSel[[4]] <- c('QUEKEL','QUKE')

r1 <- which(pw$Species==spSel[[4]][1])
r2 <- which(m$spp==spSel[[4]][2])
xlims <- c(min(c(pw$mwp[r1],m$water_potential[r2]),na.rm=T),max(c(pw$mwp[r1],m$water_potential[r2]),na.rm=T))
ylims <- c(min(c(pw$lfm[r1],m$lfm[r2]),na.rm=T),max(c(pw$lfm[r1],m$lfm[r2]),na.rm=T))
plot(pw$mwp[r1],pw$lfm[r1],xlim=xlims,ylim=ylims,pch=19,main=spSel[[4]][1],cex=2)
points(m$water_potential[r2],m$lfm[r2],col='darkgreen',cex=2,pch=19)

par(op)

## barplots of species traits - by season
pw$SpecSeason <- paste(pw$Species,pw$Sampling,sep='-')

(bulkLFM <- tapply(pw$lfm,pw$SpecSeason,mean,na.rm=T))

spMeans <- data.frame(Species=substr(names(bulkLFM),1,6),Season=substr(names(bulkLFM),8,12),BulkLFM=bulkLFM,LeafLFM=NA,StemLFM=NA,PredawnWP=NA,MiddayWP=NA,WPdiff=NA)
rownames(spMeans) <- names(bulkLFM)
spMeans$LeafLFM <- tapply(pw$Leaf.LFM,pw$SpecSeason,mean,na.rm=T)
spMeans$StemLFM <- tapply(pw$Stem.LFM,pw$SpecSeason,mean,na.rm=T)
spMeans$PredawnWP <- tapply(pw$Predawn.mean,pw$SpecSeason,mean,na.rm=T)
spMeans$MiddayWP <- tapply(pw$mwp,pw$SpecSeason,mean,na.rm=T)
spMeans$WPdiff <- tapply(pw$WPdiff,pw$SpecSeason,mean,na.rm=T)
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

pairs(spMeans[,3:8])
plot(spMeans$MiddayWP,spMeans$BulkLFM,type='n')
text(spMeans$MiddayWP,spMeans$BulkLFM,labels=spMeans$sp6)


### GGPLOT ANALYSES
# Inverse analyses

ifit <- lm(ilfm~mwp,data=pw)
abline(ifit)

gploti <- ggplot(pw) + 
  geom_point(aes(x=mwp, y=ilfm,color=Species)) +
  geom_abline(slope=ifit$coefficients[2],intercept=ifit$coefficients[1]) + 
  labs(x='Midday mean water potential (MPa)',y='Inverse of live fuel moisture') 

print(gploti)
print(gploti) + geom_hline(yintercept=1/0.7)
gploti + facet_wrap(~Species,ncol=4)

# analog to PV curves
plot(I(-1/(pw$mwp))~I(1-pw$lfm))
#plot(I(-1/(pw$mwp))~I(1-pw$lfm))


# not inverse analyses
xbp <- which(pw$Species=='BACPIL')

#choose whether to use all points or without bacpil
xx <- pw
xx <- pw[-xbp,]

fit <- lm(lfm~mwp,data=xx)

gplot <- ggplot(xx) + 
  geom_point(aes(x=mwp, y=lfm,color=Species),size=4) +
  geom_abline(slope=fit$coefficients[2],intercept=fit$coefficients[1]) + 
  labs(x='Midday mean water potential (MPa)',y='Live fuel moisture (%)') 

print(gplot)
print(gplot) + geom_hline(yintercept=0.7)

gplot + facet_wrap(~Species,ncol=4)

plot(lfm~mwp,data=pw,pch=19)
abline(fit)


