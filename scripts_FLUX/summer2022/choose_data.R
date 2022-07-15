# CHOOSE DATA - July/August 2022
# this is the next step for thinking about sampling. what do we want to measure, how should we choose pieces? 
#   
# goals for sampling: 
# * carbon gas flux (LGR chamber measurements)
# * microbe community composition (sawdust, CTAB, then sequencing)
# * also potentially some kind of moisture content (?)
# 
# considerations for sampling: 
# * in my grant proposals, I wanted to key into 1-2 species and then sample across all 5 decay classes 
# * search time is difficult in the forestGEO plot going by quads and coordinates
# * search time can be reduced by pre-marking out samples with gps unit and/or flags
# * right now, using reciprocating saw to make little cubes is best for subsampling logs
# * still up for debate if multiple samples along the length of the log to better spatially represent decay
# * remaining Q's: should a single cube could be sufficient for flux, moisture content, and also what best methods can be used in drilling for microbes?

# what we are looking for in the dataframe:
# * just at recent samples from the 2021 survey - most recent information as to what decay class they represent
# * pilot sampling only looked at 'complete cases' - pieces that were recorded in 2014, 2017, 2021. I no longer believe this to be important

# libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# read in data 
new.df <- read.csv("/Users/abbeyyatsko/Desktop/repos/serc_deadwood/data_FORESTGEO/final_data_FORESTGEO/cleaned_survey_14-21.csv")

# from df.new: want PIECETAG, SPCODE, location information, BDS.2021 (to differentiate snag/log), and information for DC.2021 
dw_select <- new.df %>%
  select(c('STEMTAG', 'PIECETAG', "QUADNAME", "QX.x", "QY.x", 'SPCODE', 'DC.2021', 'BDS.2021'))

# exclude samples with no information for DC.2021
dw_select <-dw_select[!is.na(dw_select$DC.2021),]

# filter out all snags - this study only focuses on downed deadwood 
dw_select <-dw_select[!(dw_select$BDS.2021 == 'Snag'),]

# what kind of species are there?
unique(dw_select$SPCODE)

# what is the distribution of species counts? show pecies and frequency of dw pieces in 2021 survey 
dw_select %>% 
  drop_na(SPCODE) %>%
  ggplot(aes(x = SPCODE))+
  geom_bar(stat="count")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# potential candidates:
# tulip poplar (LITU) 
# oaks (QUFA, QUERC, QUAL)
# red maple (ACRU)
# american beech (FAGR)
# sweetgum (LIST2)

# QUESTION: what are dominant species in forestGEO plot? ask Jess. target these samples. 

# NOTE that FRPE has high hits, but this is probably because of the ash dieback that is pretty widespread in the marsh region of the plot 
ash <- subset(dw_select, SPCODE == "FRPE")

# there are mainly 1s and 2s, some 3s, a handful of 4s, and like a single 5s im pretty sure
ashplot <- ggplot(data = ash, aes(x = DC.2021)) + 
  geom_bar(position="stack", stat="count")

# BUILD SAMPLING DATA

# look at decay class distribution by species 
ggplot(data = dw_select, aes(x = DC.2021)) + 
  geom_bar(position="stack", stat="count")+ 
  facet_wrap(~SPCODE)+
  theme_light()

# species with the broadest distribution across 5 decay classes: 
# ACRU - red maple
# CATO6 - hickory
# COFL2 - flowering dogwood
# LIST2 - sweetgum
# LITU - tulip tree
# QUXX collectively - all of the oaks 

# write file including targeted deadwood samples based on species and decay classes 

# export .csv


