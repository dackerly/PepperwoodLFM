# combine data
# COMBINE DATA
rm(list=ls())
spc <- read.csv('data/SpCodes.csv')
head(spc)

lw <- read.csv('data/PWD_Oct2021+Jun2022_LFM_WP_Calcs.csv',as.is=T)
lw$SpCode6 <- spc$SpCode6[match(lw$Species,spc$Pepperwood.spp)]
table(lw$Species,lw$SpCode6)
head(lw)
summary(lw$Bulk.LFM)

p <- read.cBulk.LFMp <- read.csv('data/other_studies/Pivovaroff_WP_vs_LFM.csv',as.is=T)
names(p)
p$SpCode6 <- spc$SpCode6[match(p$Species,spc$SoCal.spp)]
table(p$Species,p$SpCode6)
head(p)
p$lfm <- p$LFM_./100
summary(p$lfm)

m <- read.csv('data/other_studies/sierra_lfm_mpa.csv')
names(m)
m$SpCode6 <- spc$SpCode6[match(m$Species,spc$SEKI.spp)]
table(m$Species,m$SpCode6)
head(m)
m$lfm <- m$lfm/100
summary(m$lfm)

# subset to same columns and make names the same
head(lw)
lwx <- lw[,c('SpCode6','Sampling','Midday.mean','Bulk.LFM')]
names(lwx) <- c('Species','date','mwp','lfm')
lwx$study <- 'Pepperwood'
head(lwx)

names(p)
px <- p[,c('SpCode6','Date','WP_md_MPa','lfm')]
names(px) <- c('Species','date','mwp','lfm')
px$study <- 'StuntRanch'
names(px)

names(m)
mx <- m[,c('SpCode6','date','water_potential','lfm')]
names(mx) <- c('Species','date','mwp','lfm')
mx$study <- 'SEKI'
head(mx)

lwa <- rbind(lwx,px,mx)
dim(lwa)
head(lwa)

# take complete cases only
lwa <- lwa[complete.cases(lwa[,c('mwp','lfm')]),]
dim(lwa)

# check data distribution
table(lwa$study)
table(lwa$study,lwa$Species)
table(lwa$study,lwa$date)

summary(lwa$lfm)
hist(lwa$lfm)

write.csv(lwa,'data/all-data-combined.csv')
