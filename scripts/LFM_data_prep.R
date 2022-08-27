## preliminary analysis of 2021 LFM data
rm(list=ls())
source('scripts/Load.data.R')


lw <- read.csv(text=getURL('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/2021/LFM_Oct_2021/PWD_Oct2021_LFM_WP_Final.csv', followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")), na.strings=c("","NA"))
str(lw)
names(lw)[1] <- 'Site'
lw$Sampling <- 'Oct21'
lw <- lw[c('Site','Species','Sample.number','Sampling','Alternate.number','DBH','Height','Date','Predawn.mean','Midday.mean','Bulk.wet','Bulk.dry','Bulk.change','Leaf.wet','Leaf.dry','Leaf.change','Stem.wet','Stem.dry','Stem.change')]

# In Oct2021, we are calculating bulk LFM in two ways. For some plants, we separated leaves and stems in the field. For others, that was too time consuming, and we took one bulk sample, and don't have separate stem and leaf. This code calculates the bulk values for the first group, based on stem+leaf.

# calculate bulk data from separate samples
nobulk <- which(is.na(lw$Bulk.change))
lw$Bulk.wet[nobulk] <- lw$Leaf.wet[nobulk]+lw$Stem.wet[nobulk]
lw$Bulk.dry[nobulk] <- lw$Leaf.dry[nobulk]+lw$Stem.dry[nobulk]
lw$Bulk.change[nobulk] <- lw$Bulk.wet[nobulk]-lw$Bulk.dry[nobulk]

#check that all bulk change is accurate
summary(lw$Bulk.change-(lw$Bulk.wet-lw$Bulk.dry))
# ALL GOOD


lw2 <- read.csv(text=getURL('https://raw.githubusercontent.com/dackerly/PepperwoodVegPlots/master/2022/LFM_June_2022/PWD_JUN2022_LFM_Final.csv', followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")), na.strings=c("","NA"))
str(lw2)
names(lw2)[1] <- 'Site'
lw2$Sampling <- 'Jun22'
names(lw2)[which(names(lw2)=='Wet.wt')] <- 'Bulk.wet'
names(lw2)[which(names(lw2)=='Dry.wt')] <- 'Bulk.dry'
names(lw2)[which(names(lw2)=='Change')] <- 'Bulk.change'
lw2 <- lw2[,c('Site','Species','Sample.number','Sampling','Alternate.number','DBH','Height','Date','Predawn.mean','Midday.mean','Bulk.wet','Bulk.dry','Bulk.change')]

lw2$Leaf.wet <- NA
lw2$Leaf.dry <- NA
lw2$Leaf.change <- NA
lw2$Stem.wet <- NA
lw2$Stem.dry <- NA
lw2$Stem.change <- NA

# reverse sign of water potentials in June data
lw2$Predawn.mean <- (-1)*lw2$Predawn.mean
lw2$Midday.mean <- (-1)*lw2$Midday.mean

# check all columns aligned
all(names(lw)==names(lw2))

lw <- rbind(lw,lw2)
dim(lw)

# Calculate Live Fuel Moisture (LFM) - 
# LFM = water weight/dry weight
lw$Leaf.LFM <- lw$Leaf.change/lw$Leaf.dry
lw$Stem.LFM <- lw$Stem.change/lw$Stem.dry
lw$Bulk.LFM <- lw$Bulk.change/lw$Bulk.dry

#And calculate predawn-midday differential - this can
# provide an indication of how actively plants are 
# transpiring, and/or how high their stem conductance is
lw$WPdiff <- lw$Midday.mean-lw$Predawn.mean

# Now write results to new file for continuing analysis
write.csv(lw,'data/PWD_Oct2021+Jun2021_LFM_WP_Calcs.csv')

