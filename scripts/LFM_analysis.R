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
