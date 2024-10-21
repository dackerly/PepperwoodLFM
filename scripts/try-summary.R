# summarize try trait data
try <- read.csv(here('data', 'other_studies', 'try-trait-data.csv'))
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

write.csv(tmn,here('data', 'other_studies', 'try-means.csv'))
write.csv(tcv,here('data', 'other_studies', 'try-cvs.csv'))

# check data when cv>0.5
for (i in 1:ncol(tcv)) print(c(i,which(abs(tcv[,i])>0.5)))
names(tcv)
tcv[12,]

table(try$SpeciesName)
table(try$OriglName)
try$OrigValueStr[which(try$SpeciesName=='Quercus berberidifolia' & try$OriglName=='P50 (MPa)')]

###-----------------
# summarize try trait data for SEKI
tmn_seki <- read.csv(here('data', 'try_seki_traits_1.csv')) %>% 
  select(SpeciesName, OriglName, StdValue) %>% 
  filter(OriglName %in% c("P50", "P88", "LDMC",  "Pmin predawn (Mpa)", "Pmin midday (Mpa)")) %>% 
  group_by(SpeciesName, OriglName) %>% 
  mutate(tmn = mean(StdValue, na.rm = T)) %>% 
  select(-StdValue) %>% 
  distinct()

tsd_seki <- read.csv(here('data', 'try_seki_traits_1.csv')) %>% 
  select(SpeciesName, OriglName, StdValue) %>% 
  filter(OriglName %in% c("P50", "P88", "LDMC",  "Pmin predawn (Mpa)", "Pmin midday (Mpa)")) %>% 
  group_by(SpeciesName, OriglName) %>% 
  mutate(tsd = sd(StdValue, na.rm = T)) %>% 
  select(-StdValue) %>% 
  distinct()

write.csv(tmn_seki,here('data', 'other_studies', 'try-means-seki.csv'))
write.csv(tsd_seki,here('data', 'other_studies', 'try-cvs-seki.csv'))
###-----------------
# summarize try trait data for SEKI, attempt 2
tmn_seki <- read.csv(here('data', 'try_seki_traits_2.csv')) %>% 
  select(SpeciesName, OriglName, StdValue) %>% 
  filter(OriglName %in% c("P50", "P88", "LDMC",  "Pmin predawn (Mpa)", "Pmin midday (Mpa)")) %>% 
  filter(SpeciesName %in% c("Ceanothus megacarpus")) %>% 
  group_by(SpeciesName, OriglName) %>% 
  mutate(tmn = mean(StdValue, na.rm = T)) %>% 
  select(-StdValue) %>% 
  distinct()

tsd_seki <- read.csv(here('data', 'try_seki_traits_2.csv')) %>% 
  select(SpeciesName, OriglName, StdValue) %>% 
  #filter(OriglName %in% c("P50", "P88", "LDMC",  "Pmin predawn (Mpa)", "Pmin midday (Mpa)")) %>% 
  group_by(SpeciesName, OriglName) %>% 
  mutate(tsd = sd(StdValue, na.rm = T)) %>% 
  select(-StdValue) %>% 
  distinct()

###-----------------
# summarize try trait data for all, 20241014
try_all <- read.csv(here('data', 'try_all_20241014.csv')) %>% 
  select(SpeciesName, OriglName, StdValue) %>% 
  filter(OriglName %in% c("P50", "P88", "LDMC",  "Pmin predawn (Mpa)", "Pmin midday (Mpa)", 
                          "SLA (cm2 g-1)", "Water potential at 50% loss of conductivity Psi_50 (MPa)",
                          "P50 (MPa)", "P88 (MPa)", "SLA", "LMA (g/m2)"
                          )) %>% 
  mutate(trait_name = case_when(
    OriglName %in% c("P50", "Water potential at 50% loss of conductivity Psi_50 (MPa)", "P50 (MPa)") ~ "P50",
    OriglName %in% c("P88", "P88 (MPa)") ~ "P88",
    OriglName %in% c("SLA (cm2 g-1)", "SLA") ~ "SLA (cm2 g-1)",
    TRUE ~ OriglName
  )) %>% 
  select(-OriglName) %>% 
  distinct()
  
tmn_all <- try_all %>% 
  #filter(SpeciesName %in% c("Ceanothus megacarpus")) %>% 
  group_by(SpeciesName, trait_name) %>% 
  mutate(tmn = mean(StdValue, na.rm = T)) %>% 
  select(-StdValue) %>% 
  distinct()

tsd_all <- try_all %>% 
  group_by(SpeciesName, trait_name) %>% 
  mutate(tsd = sd(StdValue, na.rm = T)) %>% 
  select(-StdValue) %>% 
  distinct()

