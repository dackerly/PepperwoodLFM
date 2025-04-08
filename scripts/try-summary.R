# summarize try trait data
try <- read.csv(here('data', 'other_studies', 'try-trait-data.csv'))
dim(try)
str(try)

try_salvias <- try %>% 
  filter(AccSpeciesID %in% c(48133, 48136))

#look at P50 and psimin 
try_mmd<- try %>% 
  #filter(TraitID %in% c(719, ))
  filter(DataName %in% c("Minimum midday xylem pressure potential", 
                         "Xylem water potential at which 50% of conductivity is lost (P50)",
                         #"Mean P50 including only data from flushed curve",
                         "Mean P50 including all data", 
                         "Leaf osmotic potential at turgor loss")) %>% 
  mutate(DataName = case_when(
    DataName %in% c("Xylem water potential at which 50% of conductivity is lost (P50)",
                    "Mean P50 including only data from flushed curve",
                    "Mean P50 including all data") ~ "P50",
    TRUE ~ as.character(DataName)
  )) %>% 
  group_by(SpeciesName, DataName) %>% 
  summarise(mean = mean(StdValue)) %>% 
  pivot_wider(names_from = DataName, 
              values_from = mean)

try_mmd %>% 
  ggplot(aes(y = `Minimum midday xylem pressure potential`, 
             x = P50, 
             color = SpeciesName)) +
  geom_point() +
  geom_abline()

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

#------

#This is the data we actually use, which is a combination of the following: 
#1) David's trait data (here("data", "species-traits.csv)), not sure where it is made/written or written. Seems to be the data above? 
# 2) I (Indra) added in SEKI data and looked online to find other traits, which are now in the "species-traits-withseki.csv". Compiled in Excell. 
traits_df <- read_csv(here("data", "species-traits-withseki.csv")) %>% 
  clean_names() %>% 
  mutate(species = sp_code) %>% 
 # filter(sla < 20) %>% 
  mutate(psi_tlp = case_when(
    psi_tlp %in% c(NA) & species %in% c("QUEDOU") ~ -3.4,
    TRUE ~ as.numeric(psi_tlp)))

SpCodes <- read_csv(here("data", "SpCodes.csv"))

#Add rooting depth info from here:https://www.groundwaterresourcehub.org/where-we-work/california/plant-rooting-depth-database/
rd_df <- read_csv(here("data", "other_studies", "Plant_Rooting_Depth_Database_20210525_update_sheet2.csv")) %>% 
  clean_names() %>% 
  select(scientific_name, max_rooting_depth_m) %>%
  mutate(Genus_species = scientific_name) %>% 
  merge(., SpCodes,  by = c("Genus_species")) %>% 
  select(SpCode6, 3, 4) %>% 
  mutate(min_or_max = case_when(
    max_rooting_depth_m %in% c("> 2.45") ~ "min",
    max_rooting_depth_m %in% c("> 2.74") ~ "min",
    max_rooting_depth_m %in% c("> 1.52") ~ "min",
    max_rooting_depth_m %in% c("> 1.22") ~ "min",
    max_rooting_depth_m %in% c(">4") ~ "min",
    max_rooting_depth_m %in% c("> 0.61") ~ "min",
    TRUE ~ as.character("max")
  )) %>% 
  mutate(max_rooting_depth_m = case_when(
    max_rooting_depth_m %in% c("> 2.45") ~ 2.45,
    max_rooting_depth_m %in% c("> 2.74") ~ 2.74,
    max_rooting_depth_m %in% c("> 1.52") ~ 1.52,
    max_rooting_depth_m %in% c("> 1.22") ~ 1.22,
    max_rooting_depth_m %in% c(">4") ~ 4,
    max_rooting_depth_m %in% c("> 0.61") ~ 0.61,
    TRUE ~ as.numeric(max_rooting_depth_m)
  )) %>% 
  rename(sp_code = SpCode6) %>% 
  group_by(sp_code) %>% 
  mutate(max_rooting_depth_m = max(max_rooting_depth_m, na.rm = T)) %>% 
  mutate(max_rooting_depth_m = case_when(
    max_rooting_depth_m %in% c(-Inf) ~ NA, 
    TRUE ~ as.numeric(max_rooting_depth_m)
  )) %>% 
  select(-min_or_max) %>% 
  distinct()
  
  unique(rd_df$max_rooting_depth_m)
  
  traits_rd_df <- merge(rd_df, traits_df, by = c("sp_code"), all.y = T) %>% 
    select(-dr_max_rooting_depth) %>% #old attempt to manually find rooting deptha
    filter(!(sp_code %in% c("ABICON", "PINJEF", "CALDEC")))  
                    
write_csv(traits_rd_df, here("data", "traits_rd_20250327.csv"))



#### ----- 

#Older data, not sure this is used?
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


tmn_all_wide <- tmn_all %>% 
  pivot_wider(names_from = c(trait_name),
              values_from = c(tmn))

#-----

unique(try$SpeciesName)
unique(try_all$SpeciesName)

unique(try$SpeciesName)
unique(try_all$SpeciesName)

#-----

