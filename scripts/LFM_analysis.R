## preliminary analysis of 2021 LFM data
rm(list=ls())

lw <- read.csv('data/PWD_Oct2021_LFM_WP_Final.csv',as.is=T)
str(lw)

# calculate bulk data from separate samples
nobulk <- which(is.na(lw$Bulk.change))
lw$Bulk.wet[nobulk] <- lw$Leaf.wet[nobulk]+lw$Stem.wet[nobulk]
lw$Bulk.dry[nobulk] <- lw$Leaf.dry[nobulk]+lw$Stem.dry[nobulk]
lw$Bulk.change[nobulk] <- lw$Bulk.wet[nobulk]-lw$Bulk.dry[nobulk]

#check that all bulk change is accurate
#summary(lw$Bulk.change-(lw$Bulk.wet-lw$Bulk.dry))
# ALL GOOD

lw$Leaf.LFM <- lw$Leaf.change/lw$Leaf.dry
lw$Stem.LFM <- lw$Stem.change/lw$Stem.dry
lw$Bulk.LFM <- lw$Bulk.change/lw$Bulk.dry

plot(lw$Midday.mean,lw$Bulk.LFM)
plot(lw$Midday.mean,I(1/lw$Bulk.LFM),log='')
