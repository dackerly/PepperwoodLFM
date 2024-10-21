## preliminary analysis of 2021 LFM data
rm(list=ls())
library(ggplot2)
source('scripts/lfm_functions.R')

lwa <- read.csv('data/all-data-combined.csv',as.is=T,row.names=1)
lwa$ilfm <- 1/lwa$lfm
head(lwa)
table(lwa$study)

lwa <- lwa[-which(lwa$study=='SEKI'),]
sites <- sort(unique(lwa$study))
table(lwa$study)
nrow(lwa)

### plot by site
scolors <- c('orange','black','blue')
lwa$scolors <- scolors[match(lwa$study,sites)]
table(lwa$scolors)

rsamp <- sample(1:1000,1000)
plot(lfm~mwp,data=lwa,type='n',xlab='Midday water potential (Mpa)',ylab='Live fuel moisture',xlim=c(-8,0),ylim=c(0,3.5))
#points(lfm~mwp,data=lwa[rsamp,],pch=1,cex=1,col=lwa$scolors)

points(lfm~mwp,data=lwa[lwa$study=='Sedgwick',],pch=5,cex=1,col=lwa$scolors[lwa$study=='Sedgwick'])
points(lfm~mwp,data=lwa[lwa$study=='StuntRanch',],pch=15,cex=1,col=lwa$scolors[lwa$study=='StuntRanch'])
points(lfm~mwp,data=lwa[lwa$study=='Pepperwood',],pch=19,cex=1.2,col=lwa$scolors[lwa$study=='Pepperwood'])
legend(-7,3,sites,fill=NULL,border='white')
points(rep(-6.7,3),c(2.84,2.64,2.44),pch=c(19,5,15),col=scolors,cex=2)

# list of data sets
sites <- sort(unique(lwa$study))
table(lwa$study)
lwa$coastal <- T
lwa$coastal[which(lwa$study=='SEKI')] <- F

# run analyses for each site, for all coastal, and then for All
rsel <- list()
slabel <- sites
for (i in 1:length(sites)) rsel[[i]] <- which(lwa$study==sites[i])
i <- i+1
rsel[[i]] <- which(lwa$coastal)
i <- i+1
rsel[[i]] <- 1:nrow(lwa)
slabel <- c(slabel,'Coastal','All')
length(rsel)
slabel

stres <- data.frame(study=slabel,Nspec=NA,Nobs=NA,BIC.no.int=NA,common.slope=NA,BIC.int=NA)
spres <- list()
scoeff <- list()
pvals <- list()

pform <- 'Reg' # or c('Inv','Reg')
sppFac <- 'SpeciesSite' # c('Species','SpeciesSite')
slpModel <- 'SpSpecific' # c('Common','SpSpecific')

i=4
#for (i in 1:length(rsel))
for (i in 4:4)
{
  td <- lwa[rsel[[i]],]
  if (sppFac=='SpeciesSite') td$Species <- td$Sp.Site
  stres$Nobs[i] <- nrow(td)
  (spp <- sort(unique(td$Species)))
  stres$Nspec[i] <- length(spp)
  spres[[i]] <- data.frame(study=rep(slabel[i],length(spp)),species=spp,Nobs=NA,minWP=NA,critWP=NA,minLFM=NA,model1.int=NA,model1.slp=NA,model2.int=NA,model2.slp=NA)
  
  # do slopes of species relationships differ in this data set
  fit1 <- glm(ilfm~mwp+Species,data=td)
  stres$BIC.no.int[i] <- BIC(fit1)
  stres$common.slope[i] <- coefficients(fit1)[2]
  scoeff[[i]] <- coefficients(fit1)
  pvals[[i]] <- fit1$fitted.values
  spres[[i]]$model1.int[1] <- coefficients(fit1)[1]
  spres[[i]]$model1.int[-1] <- coefficients(fit1)[1] + coefficients(fit1)[-c(1:2)]
  spres[[i]]$model1.slp <- coefficients(fit1)[2]
  fit2 <- glm(ilfm~mwp*Species,data=td)
  stres$BIC.int[i] <- BIC(fit2)
  
  op=par(mar=c(5,5,3,1))
  fit <- glm(ilfm~mwp,data=td)
  mm <- range(td$mwp,na.rm=T)
  summary(fit)
  xy <- inversePlotData(mm,coefficients(fit)) 
  
  if (pform=='Reg') {
    plot(lfm~mwp,data=td,main=slabel[i],xlim=c(-8,0),ylim=c(0,4),ylab='LFM',xlab='MWP')
    abline(h=0.7,lty=2)
    lines(LFM~MWP,data=xy,lwd=3,col='red') 
  } else {
    plot(ilfm~mwp,data=td,main=slabel[i],xlim=c(-8,0),ylim=c(0,2.5),ylab='1/LFM',xlab='MWP')    
    abline(h=1/0.7,lty=2)
    lines(iLFM~MWP,data=xy,lwd=3,col='red')
  }
  mean(td$mwp,na.rm=T)
  
  s=1
  for (s in 1:length(spp))
  {
    r1 <- which(td$Species==spp[s])
    spres[[i]]$Nobs[s] <- length(r1)
    spres[[i]]$minWP[s] <- min(td$mwp[r1],na.rm=T)
    spres[[i]]$minLFM[s] <- min(td$lfm[r1],na.rm=T)
    #print(c(i,length(r1)))
    fit <- lm(ilfm[r1]~mwp[r1],data=td)
    cfit <- coefficients(fit)
    spres[[i]][s,c('model2.int','model2.slp')] <- cfit
    mm <- range(td$mwp[r1],na.rm=T)
    yy <- cfit[1] + cfit[2]*mm
    
    if (slpModel == 'SpSpecific') {
      xy <- inversePlotData(mm,cfit) 
      x0y <- inversePlotData(c(mm[2],0),cfit) 
      spres[[i]]$critWP[s] <- solveForX(cfit,yval=1/0.7)
    } else {
      cs <- c(spres[[i]]$model1.int[s],spres[[i]]$model1.slp[s])
      xy <- inversePlotData(mm,cs)
      x0y <- inversePlotData(c(mm[2],0),cs)
      spres[[i]]$critWP[s] <- solveForX(cs,yval=1/0.7)
    }
    
    if (pform=='Reg') {
      lines(LFM~MWP,data=xy,col=td$scolors[r1[1]],lwd=2) 
      lines(LFM~MWP,data=x0y,col=td$scolors[r1[1]],lwd=2,lty=3) 
    } else {
      lines(iLFM~MWP,data=xy,col=td$scolors[r1[1]],lwd=2)
      lines(iLFM~MWP,data=x0y,col=td$scolors[r1[1]],lwd=2,lty=3) 
    }
  }
  par(op)
}

stres
spres[[1]]
spres[[4]]

spres.all <- spres[[1]]
for (i in 2:length(spres)) spres.all <- rbind(spres.all,spres[[i]])
dim(spres.all)
table(spres.all$study)
head(spres.all)
tail(spres.all)

spres.all$SpCode6 <- substr(spres.all$species,1,6)

td <- spres.all[which(spres.all$study=='Coastal'),]
dim(td)
plot(minLFM~I(1/model1.int),data=td);abline(h=0.7)
plot(minLFM~minWP,data=td);abline(h=0.7)
plot(I(1/model1.int)~minWP,data=td);abline(h=0.7)

fit <- glm(minLFM~minWP+model1.int,data=td)
summary(fit)
plot(fit)

write.csv(stres,'results/summary-results.csv')
write.csv(spres.all,'results/species-results.csv')

# correlate with traits

spresc <- spres.all[which(spres.all$study=='Coastal'),]
#spt <- read.csv('data/species-traits-withseki.csv')
spt <- read.csv('data/species-traits.csv')

names(spt)
s2s <- match(spresc$SpCode6,spt$SpCode)
spresc$PLC50.stem <- spt$PLC50.stem[s2s]
spresc$PSI.TLP <- spt$PSI.TLP[s2s]
spresc$LDMC <- spt$LDMC[s2s]

names(spresc)

op=par(mfrow=c(1,2))
plot(minWP~PLC50.stem,data=spresc,pch=19,xlab='PLC50 (MPa)',ylab='Minimum MWP (MPa)')
#abline(0,1)
cor(spresc$minWP,spresc$PLC50.stem,use = 'pair')
spresc[,c('minWP','PLC50.stem')]

rsel <- which(spresc$SpCode6 %in% c('SALLEU','SALMEL'))
cor(spresc$minWP[-c(rsel)],spresc$PLC50.stem[-c(rsel)],use = 'pair')
points(minWP~PLC50.stem,data=spresc[rsel,],pch=1,cex=2)

#plot(I(1/model1.int)~LDMC,data=spresc,pch=19)
#cor(1/spresc$model1.int,spresc$LDMC,use = 'pair')
#plot(minWP~LDMC,data=spresc,pch=19)
cor(spresc$minWP,spresc$LDMC,use = 'pair')

plot(I(1/model1.int)~PSI.TLP,data=spresc,pch=19,xlab='Osmotic potential at TLP (MPa)',ylab='LFM at MWP=0')
cor(1/spresc$model1.int,spresc$PSI.TLP,use = 'pair')
par(op)

plot(minLFM~PSI.TLP,data=spresc,pch=19,xlab='Osmotic potential at TLP (MPa)',ylab='LFM at MWP=0')
cor(spresc$minLFM,spresc$PSI.TLP,use = 'pair')
cor(spresc$minLFM,spresc$PLC50.stem,use = 'pair')

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


#### END HERE
### spres.all summarizes alspresc### spres.all summarizes all results, by site and species




# 
# i=1
# td <- lwa[rsel[[i]],]
# length(pvals[[1]])
# plot(td$mwp,pvals[[1]],ylim=c(0,2))
# scoeff[[1]]
# mean(td$mwp)
# points(mean(td$mwp),scoeff[[1]][1]+scoeff[[1]][2]*mean(td$mwp),pch=19)
# abline(h=scoeff[[1]][1])
# plot(td$ilfm,pvals[[1]])
# table(td$Species,useNA='always')
# length(td$mwp)
# # show regression slopes from a common slope glm
# 
# 
# 
# 
# 
# 
# pres <- data.frame(Species=spp,N=NA,minWP=NA,intercept=NA,slope=NA,critWP=NA,LFM2.5=NA,i.intercept=NA,i.slope=NA,i.critWP=NA,i.LFM2.5=NA)
# i=1
# for (i in 1:length(spp))
# {
#   #print(i)
#   spSel <- spp[i]
#   temp <- td[which(td$Species==spSel),]
#   temp <- temp[complete.cases(temp[,c('mwp','lfm')]),]
#   pres[i,c('N')] <- nrow(temp)
#   pres[i,'minWP'] <- min(temp$mwp)
#   #plot(temp$mwp,temp$lfm,pch=19,main=spSel[1])
#   #abline(h=0.7,lty=2)
#   fit <- lm(temp$lfm~temp$mwp)
#   cfit <- coefficients(fit)
#   pres[i,c('intercept','slope')] <- cfit
#   #abline(fit)
#   pres[i,'critWP'] <- solveForX(fit)
#   pres[i,'LFM2.5'] <- cfit[1] + cfit[2] * (-2.5)
#   
#   #plot(temp$mwp,temp$ilfm,pch=19,main=spSel[1])
#   abline(h=1/0.7,lty=2)
#   fit <- lm(temp$ilfm~temp$mwp)
#   cfit <- coefficients(fit)
#   pres[i,c('i.intercept','i.slope')] <- cfit
#   #abline(fit)
#   pres[i,'i.critWP'] <- solveForX(fit,1/0.7)
#   pres[i,'i.LFM2.5'] <- 1/(cfit[1] + cfit[2] * (-2.5))
# }
# pres
# write.csv(pres,paste('results/',ssel,'-species-results.csv',sep=''))
# 
# plot(pres$minWP,pres$i.intercept)
# 
# ### end subset analysis
# 
# ### NEED TO EDIT BELOW HERE
# 
# # now plot all species across all studies
# # plot all data with lines by species
# plot(ilfm~mwp,data=lwa,xlim=c(-9,0))
# abline(h=1/0.7,lty=2)
# fit <- glm(ilfm~mwp,data=lwa)
# summary(fit)
# abline(fit)
# mean(td$mwp,na.rm=T)
# 
# (spp <- sort(unique(lwa$Species)))
# i=1
# for (i in 1:length(spp))
# {
#   r1 <- which(lwa$Species==spp[i])
#   fit <- lm(ilfm[r1]~mwp[r1],data=lwa)
#   cfit <- coefficients(fit)
#   
#   mm <- range(lwa$mwp[r1],na.rm=T)
#   print(round(c(i,cfit,mm),2))
#   yy <- cfit[1] + cfit[2]*mm
#   lines(mm,yy,col='red')
# }
# 
# # model fit
# # no species
# fit1 <- glm(ilfm~mwp,data=lwa)
# BIC(fit1)
# 
# fit2 <- glm(ilfm~mwp+Species,data=lwa)
# BIC(fit2)
# 
# fit3 <- glm(ilfm~mwp*Species,data=lwa)
# BIC(fit3)
# 
# ### good to here
# 
# minX <- min(c(m$water_potential,p$WP_md_MPa,pw$mwp),na.rm=T)
# maxX <- max(c(m$water_potential,p$WP_md_MPa,pw$mwp),na.rm=T)
# 
# par(mar=c(5,5,1,1))
# plot(p$WP_md_MPa,p$bulkLFM,pch=19, xlab='Midday water potential (MPa)',ylab='Bulk live fuel moisture',cex.lab=2,xlim=c(minX,maxX))
# points(m$water_potential,m$lfm,pch=1,col='darkgreen',cex=1.5)
# points(pw$mwp,pw$lfm,pch=19,col='red',cex=1.5)
# 
# legend(-7.2,2.5,legend = c('Pivovaroff','Boving/Moritz','Pepperwood'),fill = c('black','darkgreen','red'),cex = 1.5)
# 
# 
# ## Combine our data and P for individual species
# op=par(mar=c(5,5,3,1),mfrow=c(2,2))
# spSel <- list()
# spSel[[1]] <- c('ADEFAS','ADFA')
# spSel[[2]] <- c('HETARB','HEAR')
# spSel[[3]] <- c('QUEAGR','QUAG')
# 
# for (i in 1:3) {
#   r1 <- which(pw$Species==spSel[[i]][1])
#   r2 <- which(p$Species==spSel[[i]][2])
#   xlims <- c(min(c(pw$mwp[r1],p$WP_md_MPa[r2]),na.rm=T),max(c(pw$mwp[r1],p$WP_md_MPa[r2]),na.rm=T))
#   ylims <- c(min(c(pw$lfm[r1],p$bulkLFM[r2]),na.rm=T),max(c(pw$lfm[r1],p$bulkLFM[r2]),na.rm=T))
#   plot(pw$mwp[r1],pw$lfm[r1],xlim=xlims,ylim=ylims,pch=19,main=spSel[[i]][1],xlab='Midday water potential (MPa)',ylab='Life fuel moisture',cex=2)
#   points(p$WP_md_MPa[r2],p$bulkLFM[r2],col='red',pch=19,cex=2)
# }
# ## Combine our data and m for individual species
# spSel[[4]] <- c('QUEKEL','QUKE')
# 
# r1 <- which(pw$Species==spSel[[4]][1])
# r2 <- which(m$spp==spSel[[4]][2])
# xlims <- c(min(c(pw$mwp[r1],m$water_potential[r2]),na.rm=T),max(c(pw$mwp[r1],m$water_potential[r2]),na.rm=T))
# ylims <- c(min(c(pw$lfm[r1],m$lfm[r2]),na.rm=T),max(c(pw$lfm[r1],m$lfm[r2]),na.rm=T))
# plot(pw$mwp[r1],pw$lfm[r1],xlim=xlims,ylim=ylims,pch=19,main=spSel[[4]][1],cex=2)
# points(m$water_potential[r2],m$lfm[r2],col='darkgreen',cex=2,pch=19)
# 
# par(op)
# 
# ## barplots of species traits - by season
# pw$SpecSeason <- paste(pw$Species,pw$Sampling,sep='-')
# 
# (bulkLFM <- tapply(pw$lfm,pw$SpecSeason,mean,na.rm=T))
# 
# spMeans <- data.frame(Species=substr(names(bulkLFM),1,6),Season=substr(names(bulkLFM),8,12),BulkLFM=bulkLFM,LeafLFM=NA,StemLFM=NA,PredawnWP=NA,MiddayWP=NA,WPdiff=NA)
# rownames(spMeans) <- names(bulkLFM)
# spMeans$LeafLFM <- tapply(pw$Leaf.LFM,pw$SpecSeason,mean,na.rm=T)
# spMeans$StemLFM <- tapply(pw$Stem.LFM,pw$SpecSeason,mean,na.rm=T)
# spMeans$PredawnWP <- tapply(pw$Predawn.mean,pw$SpecSeason,mean,na.rm=T)
# spMeans$MiddayWP <- tapply(pw$mwp,pw$SpecSeason,mean,na.rm=T)
# spMeans$WPdiff <- tapply(pw$WPdiff,pw$SpecSeason,mean,na.rm=T)
# rownames(spMeans)
# 
# spMeans
# spMeans2 <- spMeans[order(spMeans$Season),]
# spOrder <- match(c('CEACUN','CEAPAR','ADEFAS','HETARB','QUEDUR','UMBCAL','ARBMEN','PSEMEN','QUEAGR','QUEKEL','QUEDOU','QUEGAR'),spMeans2$Species)
# spOrder <- c(spOrder,spOrder+12)
# (spMeans3 <- spMeans2[spOrder,])
# 
# barplot(spMeans3$MiddayWP[1:12],ylim=c(-6.5,0))
# barplot(spMeans3$MiddayWP[13:24])
# 
# barplot(spMeans$BulkLFM[1:12],ylim=c(0,1.8))
# barplot(spMeans$BulkLFM[13:24],ylim=c(0,1.8))
# abline(h=0.7,lty=2)
# 
# ## how much variation in fall due to WP
# cor(spMeans3$MiddayWP[13:24],spMeans$BulkLFM[13:24])
# 
# plot(MiddayWP~PredawnWP,data=spMeans[which(spMeans$Season=='Oct21'),],xlim=c(-5,0),ylim=c(-6.5,0))
# (fit <- lm(MiddayWP~PredawnWP,data=spMeans))
# abline(0,1,lty=2)
# abline(fit)
# 
# plot(BulkLFM~MiddayWP,data=spMeans[which(spMeans$Season=='Oct21'),],type='n',xlim=c(-7,-2))
# text(spMeans$MiddayWP[which((spMeans$Season=='Oct21'))],spMeans$BulkLFM[(spMeans$Season=='Oct21')],spMeans$Species[(spMeans$Season=='Oct21')])
# 
# spNm <- sort(unique(spMeans$Species))
# plot(BulkLFM~MiddayWP,data=spMeans,type='n',xlim=c(-7,-0.5))
# text(spMeans$MiddayWP[which((spMeans$Season=='Jun22'))],spMeans$BulkLFM[(spMeans$Season=='Jun22')],spMeans$Species[(spMeans$Season=='Jun22')],col='black',cex=0.75)
# i=1
# for (i in 1:length(spNm)) {
#   tmp <- spMeans[which(spMeans$Species==spNm[i]),]
#   lines(tmp$MiddayWP,tmp$BulkLFM)
# }
# text(spMeans$MiddayWP[which((spMeans$Season=='Oct21'))],spMeans$BulkLFM[(spMeans$Season=='Oct21')],spMeans$Species[(spMeans$Season=='Oct21')],col='red')
# abline(h=0.7,lty=2)
# 
# plot(BulkLFM~PredawnWP,data=spMeans[which(spMeans$Season=='Oct21'),],type='n',xlim=c(-5,-0.5),ylim=c(0.5,1.25))
# text(spMeans$PredawnWP[which((spMeans$Season=='Oct21'))],spMeans$BulkLFM[(spMeans$Season=='Oct21')],spMeans$Species[(spMeans$Season=='Oct21')])
# abline(h=0.7,lty=2)
# 
# spMeans <- spMeans[order(spMeans$BulkLFM),]
# 
# rownames(spMeans)
# #spMeans$sp2 <- c('Cc','Af','Qr','Qd','Cp','Qg','Qa','Qk','Ha','Uc','Am','Pm')
# #spMeans$sp6 <- c('Ceacun','Adefas','Quedur','Quedou','Ceapar','Quegar','Queagr','Quekel','Hetarb','Umbcal','Arbmen','Psemen','Bacpil')
# 
# op=par(mfrow=c(2,3),mar=c(4,4,2,1))
# barplot(spMeans$BulkLFM,main='Bulk LFM',names.arg = spMeans$sp2)
# barplot(spMeans$LeafLFM,main='Leaf LFM',names.arg = spMeans$sp2)
# barplot(spMeans$StemLFM,main='Stem LFM',names.arg = spMeans$sp2)
# barplot(spMeans$MiddayWP,main='Midday WP',names.arg = spMeans$sp2, ylim=c(-6.5,0))
# barplot(spMeans$PredawnWP,main='Predawn WP',names.arg = spMeans$sp2, ylim=c(-6.5,0))
# barplot(spMeans$WPdiff,main='WP differential',names.arg = spMeans$sp2)
# par(op)
# 
# pairs(spMeans[,3:8])
# plot(spMeans$MiddayWP,spMeans$BulkLFM,type='n')
# text(spMeans$MiddayWP,spMeans$BulkLFM,labels=spMeans$sp6)
# 
# 
# ### GGPLOT ANALYSES
# # Inverse analyses
# 
# ifit <- lm(ilfm~mwp,data=pw)
# abline(ifit)
# 
# gploti <- ggplot(pw) + 
#   geom_point(aes(x=mwp, y=ilfm,color=Species)) +
#   geom_abline(slope=ifit$coefficients[2],intercept=ifit$coefficients[1]) + 
#   labs(x='Midday mean water potential (MPa)',y='Inverse of live fuel moisture') 
# 
# print(gploti)
# print(gploti) + geom_hline(yintercept=1/0.7)
# gploti + facet_wrap(~Species,ncol=4)
# 
# # analog to PV curves
# plot(I(-1/(pw$mwp))~I(1-pw$lfm))
# #plot(I(-1/(pw$mwp))~I(1-pw$lfm))
# 
# 
# # not inverse analyses
# xbp <- which(pw$Species=='BACPIL')
# 
# #choose whether to use all points or without bacpil
# xx <- pw
# xx <- pw[-xbp,]
# 
# fit <- lm(lfm~mwp,data=xx)
# 
# gplot <- ggplot(xx) + 
#   geom_point(aes(x=mwp, y=lfm,color=Species),size=4) +
#   geom_abline(slope=fit$coefficients[2],intercept=fit$coefficients[1]) + 
#   labs(x='Midday mean water potential (MPa)',y='Live fuel moisture (%)') 
# 
# print(gplot)
# print(gplot) + geom_hline(yintercept=0.7)
# 
# gplot + facet_wrap(~Species,ncol=4)
# 
# plot(lfm~mwp,data=pw,pch=19)
# abline(fit)
# 
# 
