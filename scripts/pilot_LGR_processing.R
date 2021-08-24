# script setup / processing workflow from Genevieve 
# LGR pilot data comes from 2 days of sampling: 08.03.21 (DAY 1) and 08.04.21 (DAY 2) 

# DAY 1 processing ---- 
# set year,month and date of files being processed
year='2021'
month='August'
date='2021-08-03'

# identify working directory for corresponding date
wd=paste('/Users/abbeyyatsko/Desktop/repos/serc_deadwood/pilot_LGR/2021-08-03',
         year,'/',month,'/',date,sep='')

# step 1 - format LGR raw data
# load data.table package
library(data.table)

# set working directory - this was already defined as the working directory above 
# note that this differs from the orignal script because there was a further folder called 'Raw data'
setwd("/Users/abbeyyatsko/Desktop/repos/serc_deadwood/pilot_LGR/2021-08-03")

# get names of all LGR files with GHG concentration
## data ('...f####.txt')
filenames=list.files(pattern='f0',full.names=T)

## Read in LGR datafiles to list
dat=lapply(filenames,read.csv,skip=1)

## Remove rows with NAs
# dat=lapply(dat,na.omit)
# NOTE this caused an error - remove step 

## Combine all files into single dataframe
dat.all=do.call(rbind,dat)

## Pull out date and time data
date_time=strptime(dat.all[,1],format='%m/%d/%Y %H:%M:%S')

## Add year,month,day,JD,hour,min,sec columns to dataframe
Year=as.numeric(format(date_time,'%Y'))
Month=as.numeric(format(date_time,'%m'))
Day=as.numeric(format(date_time,'%d'))
fDOY=as.numeric(julian(date_time,'2021-01-01'))  #Change for year
Hour=as.numeric(format(date_time,'%k'))
Min=as.numeric(format(date_time,'%M'))
Sec=as.numeric(format(date_time,'%S'))
dat.all=cbind(date_time,Year,Month,Day,fDOY,Hour,Min,Sec,dat.all[,-1])

## Save LGR data as data.table
dat.all=data.table(dat.all)

# preliminary plots
# for co2 
library(scales)
co2_d1 <- ggplot(dat.all, aes(date_time, X.CO2.d_ppm)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# for ch4 
ch4_d1 <- ggplot(dat.all, aes(date_time, X.CH4.d_ppm)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

library(ggarrange)
ggarrange(co2_d1, ch4_d1, nrow = 2)

# step 2 - format plot metadata

# Read in file with plot data
plots=read.csv('plot_metadata_day1.csv')
as.Date(plots$Date, '%m/%d/%y')
str(plots)
## Pull out date and time data
plot_date_time_start=strptime(paste(plots$Date,plots$Time_start),'%m/%d/%y %H:%M')

## Add 30 seconds to set start time to be within flux period 
plot_date_time_start=plot_date_time_start+30

## Add 5 min (300 sec) to flux start time to get flux end time
plot_date_time_end=plot_date_time_start+300

## Add fDOY columns for start and endtime to dataframe
fDOY_start=as.numeric(julian(plot_date_time_start,'2021-01-01'))  #Change for year
fDOY_end=as.numeric(julian(plot_date_time_end,'2021-01-01'))  #Change for year
plots=cbind(fDOY_start,fDOY_end,plots)

# step 3 - merge LGR files and plot metadata 

## Load sqldf package
library(sqldf)

## merge LGR/log and plot metadata files by time of flux (in fractional DOY)

## 'inner join' removes all rows that are not during chamber placement
## 'left join' keeps all data and adds NAs for data not during chamber placement
dat_merged <- sqldf("select * from dat.all
                 on (dat_all.fDOY between plots.fDOY_start and plots.fDOY_end)")

seqdf <- data.frame(thetime=seq(100,225,5),thevalue=factor(letters))
boundsdf <- data.frame(thestart=c(110,160,200),theend=c(130,180,220),groupID=c(555,666,777))

# run the same query using 'between...and' clause
testquery_2 <- sqldf("select seqdf.thetime, seqdf.thevalue, boundsdf.groupID 
from seqdf LEFT JOIN boundsdf ON (seqdf.thetime BETWEEN boundsdf.thestart AND boundsdf.theend)")

myquery_2 <- sqldf("select dat_all.fDOY, dat_all.ch4, plots.Time_start 
from dat_all LEFT JOIN plots ON (dat_all.fDOY BETWEEN plots.fDOY_start AND plots.fDOY_end)")
