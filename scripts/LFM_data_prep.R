## preliminary analysis of 2021 LFM data
rm(list=ls())

lw <- read.csv('data/PWD_Oct2021_LFM_WP_Final.csv',as.is=T)
str(lw)

# We are calculating bulk LFM in two ways. For some plants, we separated leaves and stems in the field. For others, that was too time consuming, and we took one bulk sample, and don't have separate stem and leaf. This code calculates the bulk values for the first group, based on stem+leaf.

# calculate bulk data from separate samples
nobulk <- which(is.na(lw$Bulk.change))
lw$Bulk.wet[nobulk] <- lw$Leaf.wet[nobulk]+lw$Stem.wet[nobulk]
lw$Bulk.dry[nobulk] <- lw$Leaf.dry[nobulk]+lw$Stem.dry[nobulk]
lw$Bulk.change[nobulk] <- lw$Bulk.wet[nobulk]-lw$Bulk.dry[nobulk]

#check that all bulk change is accurate
#summary(lw$Bulk.change-(lw$Bulk.wet-lw$Bulk.dry))
# ALL GOOD

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
write.csv(lw,'data/PWD_Oct2021_LFM_WP_Calcs.csv')
