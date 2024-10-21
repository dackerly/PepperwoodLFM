# combine data
# COMBINE DATA
rm(list=ls())
spc <- read.csv('data/SpCodes.csv')
head(spc)

lw <- read.csv('data/PWD_Oct2021+Jun2022_LFM_WP_Calcs.csv',as.is=T) %>% 
  mutate(date = ymd(Date))
lw$SpCode6 <- spc$SpCode6[match(lw$Species,spc$Pepperwood.spp)]
table(lw$Species,lw$SpCode6)
head(lw)
summary(lw$Bulk.LFM)

p <- read.cBulk.LFMp <- read.csv('data/other_studies/Pivovaroff_WP_vs_LFM.csv',as.is=T) %>% 
  mutate(date = mdy(Date))
names(p)
p$SpCode6 <- spc$SpCode6[match(p$Species,spc$SoCal.spp)]
table(p$Species,p$SpCode6)
head(p)
p$lfm <- p$LFM_./100
summary(p$lfm)

m <- read.csv('data/other_studies/sierra_lfm_mpa.csv') %>% 
  mutate(date = ymd(date))
names(m)
m$SpCode6 <- spc$SpCode6[match(m$Species,spc$SEKI.spp)]
table(m$Species,m$SpCode6)
head(m)
m$lfm <- m$lfm/100
summary(m$lfm)

s <- read.csv('data/other_studies/sedgwick_22_lfm_mpa_df20230724.csv') %>% 
  mutate(date = ymd(date_lfm))
names(s)
s$SpCode6 <- spc$SpCode6[match(s$species,spc$Sedgwick.spp)]
table(s$species,s$SpCode6)
head(s)
s$lfm <- s$lfm_percent/100
s$mpa_mean <- -s$mpa_mean
summary(s$lfm)
summary(s$mpa_mean)

# subset to same columns and make names the same
head(lw)
#lwx <- lw[,c('SpCode6','Sampling','Midday.mean','Bulk.LFM')]
lwx <- lw[,c('SpCode6','date','Midday.mean','Bulk.LFM')]
names(lwx) <- c('Species','date','mwp','lfm')
lwx$study <- 'Pepperwood'
head(lwx)

names(p)
#px <- p[,c('SpCode6','Date','WP_md_MPa','lfm')]
px <- p[,c('SpCode6','date','WP_md_MPa','lfm')]
names(px) <- c('Species','date','mwp','lfm')
px$study <- 'StuntRanch'
names(px)

names(m)
mx <- m[,c('SpCode6','date','water_potential','lfm')]
names(mx) <- c('Species','date','mwp','lfm')
mx$study <- 'SEKI'
head(mx)

names(s)
#sx <- s[,c('SpCode6','date_lfm','mpa_mean','lfm')]
sx <- s[,c('SpCode6','date','mpa_mean','lfm')]
names(sx) <- c('Species','date','mwp','lfm')
sx$study <- 'Sedgwick'
head(sx)

lwa <- rbind(lwx,px,mx,sx)
dim(lwa)
head(lwa)

lwa$Sp.Site <- paste(lwa$Species,lwa$study,sep="_")

# take complete cases only
lwa <- lwa[complete.cases(lwa[,c('mwp','lfm')]),]
dim(lwa)
lwa <- lwa[-which(is.na(lwa$Species)),]
dim(lwa)

# check data distribution
table(lwa$study)
table(lwa$study,lwa$Species)
table(lwa$study,lwa$date)

summary(lwa$lfm)
hist(lwa$lfm)

write.csv(lwa,here('data', 'all-data-combined.csv'))
