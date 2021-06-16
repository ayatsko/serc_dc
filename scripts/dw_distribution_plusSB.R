# deadwood script - look at distributions from 2014 survey, 2017 survey, and 2021 survey up 
# until spring break (~halfway through the spring survey)

### 1. ORGANIZE WORKSPACE----
# set up workspace and load in relevant data frames
# dw surveys from 2014 (?) and 2017 -> dw1 and dw2
library(ggplot2)
library(ggpubr)
library(dplyr)

setwd("/Users/abbeyyatsko/Desktop/repos/serc_deadwood/cleaned_data")
dw2014 <- read.csv("deadwood_2014_updated.csv", na.strings=c("","NA"))
dw2017 <- read.csv("deadwood_2017_updated.csv", na.strings=c("","NA"))
dead_2019 <- read.csv("dead_2019_updated.csv", na.strings=c("","NA"))
living_2019 <- read.csv("living_2019_updated.csv", na.strings=c("","NA"))

# note the initial length of the dataframes
# > 2014 has 1371 observations 
# > 2017 has 665 observations (approximately half of the preceding survey)
# > dead_2019 has 454 observations
# > living_2019 has 4662 observations

### 2. FILTER DATA----
# remove observations that do not have a tree tag ID
# function for complete cases (by survey)

View(dw2014)
completeFun_2014 <- function(dw2014, tag) {
  completeVec <- complete.cases(dw2014[, tag])
  return(dw2014[completeVec, ])
}
View(dw2017)
completeFun_2017 <- function(dw2017, STEMTAG) {
  completeVec <- complete.cases(dw2017[, STEMTAG])
  return(dw2017[completeVec, ])
}
View(dead_2019)
completeFun_2019_dead <- function(dead_2019, STEMTAG) {
  completeVec <- complete.cases(dead_2019[, STEMTAG])
  return(dead_2019[completeVec, ])
}
View(living_2019)
completeFun_2019_living <- function(living_2019, STEMTAG) {
  completeVec <- complete.cases(living_2019[, STEMTAG])
  return(living_2019[completeVec, ])
}

dw2014 <- dw2014[dw2014$TAG != "no tag", ]
dw2014 <- completeFun_2014(dw2014, "TAG")
dw2017 <- completeFun_2017(dw2017, "STEMTAG")
dead_2019 <- completeFun_2019_dead(dead_2019, "STEMTAG")
living_2019 <- completeFun_2019_living(living_2019, "STEMTAG")

# note the new length of the dataframes
# > 2014 has 810 observations 
# > 2017 has 642 observations (approximately half of the preceding survey)
# > dead_2019 has 454 observations
# > living_2019 has 4661 observations

# reduce data sets to include only tagged samples (previously filtered for)
# that ALSO have a recorded decay class
dw2014 <- completeFun_2014(dw2014, "DECAYCLASSORIG")
# there is a random observation for the 2014 that records a decay class 6 - remove
dw2014 <- dw2014[dw2014$DECAYCLASSORIG != "6", ] 
dw2017 <- completeFun_2017(dw2017, "DECAYCLASSORIG")
dead_2019 <- completeFun_2019_dead(dead_2019, "DECAYCLASS.2021")
living_2019 <- completeFun_2019_living(living_2019, "DECAYCLASS.2021")

# note the new length of the dataframes after being filtered for containing a decay class 
# > 2014 has 809 observations 
# > 2017 has 597 observations (approximately half of the preceding survey)
# > dead_2019 has 39 observations
# > living_2019 has 41 observations

### 3. IDENTIFY COMMON ELEMENTS ACROSS 2014, 2017, living&dead 2019 DATA ----
# goal: to see how decay class has progressed through time for a given DW piece
# (indicated by stem tag number) 

# therefore, we first need to identify which stem tags are shared between DW_2014, DW_2017.
# living_2019, and dead_2019

# first, make vectors for dw2014, dw2017, dead_2019, and living_2019 stem tags / identifiers 
# this will be easier to identify common elements using Reduce(intersect)

# > dw2014
dw2014_tagvec <- dplyr::pull(dw2014, TAG)
# > dw2017
dw2017_tagvec <- dplyr::pull(dw2017, STEMTAG)
# > dead_2019
dead_2019_tagvec <- dplyr::pull(dead_2019, STEMTAG)
# > living_2019 
living_2019_tagvec <- dplyr::pull(living_2019, STEMTAG)

# common elements between 2014 and 2017 surveys 
common_elements <- Reduce(intersect, list(dw2014_tagvec, dw2017_tagvec))
common_elements

# n = 148 stem tag IDs that can be traced from 2014 to 2017 with the means of making comparison 

# ok, so now we have-
#  > 1. the overlap in stem tags that are tracked from 2014 to 2017
#  > 2. records from living_2019 that would represent new pieces being added to the system
#  > 3. records from dead_2019 that would also represent new pieces being added 

# so the next steps are: 
#  > 1. filter out common elements (via stem tag) from 2014 and 2017 data to isolate 
#       what is tracked thru time 
dw2014_CE <- dw2014 %>% filter(TAG %in% common_elements)
# length: 252 (therefore some repetitions for different A or B pieces)

dw2017_CE <- dw2017 %>% filter(STEMTAG %in% common_elements)
# length: 198 (therefore some repetitions for different A or B pieces)
View(dw2014_CE)
View(dw2017_CE)

#  > 2. cut down dw2014_CE, dw2017_CE to include only some columns (those of interest)
# for dw2014_CE
dw2014_CE_chop <- subset(dw2014_CE, select = c("QUADNAME", "TAG", "SPCODE", "PIECE.2014", 
                                               "DECAYCLASSORIG", "DBH"))

# for dw2017_CE
dw2017_CE_chop <- subset(dw2017_CE, select = c("QUADNAME", "STEMTAG", "SPCODE","PIECE.2017",
                                               "PIECE.2021", "DECAYCLASS.2017","DECAYCLASSORIG", 
                                               "DECAYCLASS.2021", "DBH", "DBH.2017", "DBH.2021"))

#  > 3. also cut down living_2019, dead_2019 to include only some columns (those of interest)
living_2019_chop <- subset(living_2019, select = c("QUADNAME","STEMTAG", "SPCODE", "PIECE.2021",
                                                   "DECAYCLASS.2021","DBH.2021", "DBH.2019"))

dead_2019_chop <- subset(dead_2019, select = c("QUADNAME","STEMTAG", "SPCODE", "PIECE.2021",
                                                   "DECAYCLASS.2021","DBH.2021", "DBH"))

# NEXT STEP: figure out some way to combine these 4 dataframes and begin to call on 
# different variables at different time points in order to start viz for decay class transitions 



