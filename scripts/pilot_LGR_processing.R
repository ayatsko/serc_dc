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
