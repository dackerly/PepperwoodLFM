# summarize try trait data
try <- read.csv('data/other_studies/try-trait-data.csv')
dim(try)
str(try)

tmn <- tapply(try$OrigValueStr,list(try$SpeciesName,try$OriglName),mean,na.rm=T)
str(tmn)
tmn

tsd <- tapply(try$OrigValueStr,list(try$SpeciesName,try$OriglName),sd,na.rm=T)
str(tsd)
tsd

tcv <- data.frame(tsd/tmn)
tcv

write.csv(tmn,'data/other_studies/try-means.csv')
write.csv(tcv,'data/other_studies/try-cvs.csv')

# check data when cv>0.5
for (i in 1:ncol(tcv)) print(c(i,which(abs(tcv[,i])>0.5)))
names(tcv)
tcv[12,]

table(try$SpeciesName)
table(try$OriglName)
try$OrigValueStr[which(try$SpeciesName=='Quercus berberidifolia' & try$OriglName=='P50 (MPa)')]
