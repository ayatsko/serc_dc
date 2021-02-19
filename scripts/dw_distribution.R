# deadwood script - initial look at distributions 

### 1. ORGANIZE WORKSPACE----
# set up workspace and load in relevant data frames
# dw surveys from 2014 (?) and 2017 -> dw1 and dw2
library(ggplot2)
library(ggpubr)
setwd("/Users/abbeyyatsko/Desktop/serc_deadwood/data")
dw2014 <- read.csv("DW_2014.csv", na.strings=c("","NA"))
dw2017 <- read.csv("DW_2017.csv", na.strings=c("","NA"))

# note the length of the two dataframes
# > 2014 has 1371 observations 
# > 2017 has 656 observations (approximately half of the preceding survey)

### 2. FILTER DATA----
# remove observations that do not have a tree tag ID
# function for complete cases (by survey)
completeFun_2014 <- function(dw2014, tag) {
  completeVec <- complete.cases(dw2014[, tag])
  return(dw2014[completeVec, ])
}
completeFun_2017 <- function(dw2017, STEMTAG) {
  completeVec <- complete.cases(dw2017[, STEMTAG])
  return(dw2017[completeVec, ])
}

dw2014 <- dw2014[dw2014$tag != "no tag", ]
dw2014 <- completeFun_2014(dw2014, "tag")
dw2017 <- completeFun_2017(dw2017, "STEMTAG")

# reduce data sets to include only tagged samples that have a recorded decay class
dw2014 <- completeFun_2014(dw2014, "decay")
# there is a random observation for the 2014 that records a decay class 6 - remove
dw2014 <- dw2014[dw2014$decay != "6", ] 
dw2017 <- completeFun_2017(dw2017, "DECAYCLASSORIG")

### 3. DECAY CLASS DISTRIBUTION COMPARISON 2014-2017 ----
# filtering order: are tagged --> have decay class info available 

# 2014 DW data 
DW_2014 <- ggplot(dw2014, aes(x=decay)) + 
  geom_histogram(binwidth=1, color = "black", fill="white")+
  theme_classic()+
  geom_vline(aes(xintercept=mean(decay)),
             color="blue", linetype="dashed", size=1)+
  ggtitle("2014 DW decay class distribution") +
  xlab("Count") + ylab("Decay Class")

# 2017 DW data 
# taking into consideration the 'converted' decay class values first (5 tier -> 3 tier)
DW_2017_3T <- ggplot(dw2017, aes(x=DECAYCLASSORIG)) + 
  geom_histogram(binwidth=1, color = "black", fill="white")+
  theme_classic()+
  geom_vline(aes(xintercept=mean(DECAYCLASSORIG)),
             color="blue", linetype="dashed", size=1)+
  ggtitle("2017 DW decay class distribution (3-tier)") +
  xlab("Count") + ylab("Decay Class")

# distribution of the original decay class values (5 tier)
DW_2017_5T <- ggplot(dw2017, aes(x=DECAYCLASS)) + 
  geom_histogram(binwidth=1, color = "black", fill="white")+
  theme_classic()+
  geom_vline(aes(xintercept=mean(DECAYCLASS)),
             color="blue", linetype="dashed", size=1)+
  ggtitle("2017 DW decay class distribution (5-tier)") +
  xlab("Count") + ylab("Decay Class")

# alligned together 
ggarrange(DW_2014,DW_2017_3T,DW_2017_5T,  ncol = 1, nrow = 3)

### 4. IDENTIFY COMMON ELEMENTS ACROSS 2014 and 2017 DATA ----
# goal: to see how decay class has progressed through time for a given DW piece
# (indicated by stem tag number) 

# therefore, we first need to identify which stem tags are shared between both DW_2014
# and DW_2017
common_elements <- Reduce(intersect, list(dw2014[, 1], dw2017[, 2]));
common_elements

# n = 148 stem tag IDs that can be traced from 2014 to 2017 with the means of making comparison 
# question: what do these distributions look like for 'common elements' each survey year?

# filter out common elements from 2014 and 2017 data 
dw2014_CE <- dw2014 %>% filter(tag %in% common_elements)
# length: 252 (therefore some repetitions for different A or B pieces)
dw2017_CE <- dw2017 %>% filter(STEMTAG %in% common_elements)
# length: 198 (therefore some repetitions for different A or B pieces)

sum(is.na(dw2014_CE$piece.id))
sum(is.na(dw2017_CE$PIECE))

# decay class distributions for common elements 
DW_2014_CE <- ggplot(dw2014_CE, aes(x=decay)) + 
  geom_histogram(binwidth=1, color = "black", fill="white")+
  theme_classic()+
  geom_vline(aes(xintercept=mean(decay)),
             color="blue", linetype="dashed", size=1)+
  ggtitle("2014 DW decay class distribution (common elements)") +
  xlab("Count") + ylab("Decay Class")

DW_2017_3T_CE <- ggplot(dw2017_CE, aes(x=DECAYCLASSORIG)) + 
  geom_histogram(binwidth=1, color = "black", fill="white")+
  theme_classic()+
  geom_vline(aes(xintercept=mean(DECAYCLASSORIG)),
             color="blue", linetype="dashed", size=1)+
  ggtitle("2017 DW decay class distribution (3-tier) (common elements)") +
  xlab("Count") + ylab("Decay Class")

DW_2017_5T_CE <- ggplot(dw2017_CE, aes(x=DECAYCLASS)) + 
  geom_histogram(binwidth=1, color = "black", fill="white")+
  theme_classic()+
  geom_vline(aes(xintercept=mean(DECAYCLASS)),
             color="blue", linetype="dashed", size=1)+
  ggtitle("2017 DW decay class distribution (5-tier) (common elements)") +
  xlab("Count") + ylab("Decay Class")

# compare distributions of decay class for full data set (left) and subsetted by common elements (right)
ggarrange(DW_2014,DW_2014_CE,DW_2017_3T,DW_2017_3T_CE,DW_2017_5T,DW_2017_5T_CE,  ncol = 2, nrow = 3)

### 5. PIECE TOGETHER THE PIECES ----
# make a column that merges stem tag ID with piece identifiers (letter)
dw2014_CE$tagpiece <- paste(dw2014_CE$tag,dw2014_CE$piece.id)
dw2017_CE$tagpiece <- paste(dw2017_CE$STEMTAG,dw2017_CE$PIECE)

# now compare common elements of 'tagpiece' between 2014 and 2017 data 
common_elements_piece <- Reduce(intersect, list(dw2014_CE[, 25], dw2017_CE[, 32]));
common_elements_piece

# filter out common elements from 2014 and 2017 data 
dw2014_CE_piece <- dw2014_CE %>% filter(tagpiece %in% common_elements_piece)
# length: 252 (therefore some repetitions for different A or B pieces)
dw2017_CE_piece <- dw2017_CE %>% filter(tagpiece %in% common_elements_piece)
# length: 198 (therefore some repetitions for different A or B pieces)






