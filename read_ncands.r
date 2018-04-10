#######################################################################################
## Data processing for "Characteristics of the front-line child welfare workforce"
## Frank Edwards and Chris Wildeman, Children and Youth Services 2018
## All R scripts written by Frank Edwards
## contact: FEdwards@cornell.edu
## free for public use with citation
###################
## Execute to partition restricted-access NCANDS child file with needed variables
## To learn more about the restriced-access NCANDS, or public release NCANDS
## contact NDACAN@cornell.edu or visit ndacan.cornell.edu
#######################################################################################

rm(list=ls())
gc()
library(data.table)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

#####################################################################
# Read full ncands, first 1k rows for colTypes, variable selection
#####################################################################

infile<-"PATH TO TAB DELIMITED NCANDS CHILD FILE"



classes<-as.character(sapply(fread(file=infile,
                                   header=TRUE,
                                   sep="\t",
                                   nrows = 1000), class))
headers<-names(fread(file=infile, header=TRUE,
                     sep="\t",
                     nrows = 100))

keeps<-c("RptID",
  "SubYr", 
  "RptDt",
  "RptFIPS",
  "StaTerr",
  "wrkrid", 
  "suprvid")

keep.index<-which(headers%in%keeps)

dat.full<-fread(infile,
           col.names = headers[keep.index],
           colClasses = classes,
           select=keep.index, 
           data.table = FALSE)

dat.full<-dat.full%>%
  dplyr::rename(year=SubYr)

dat.full<-dat.full%>%
  filter(year>2002)

years<-unique(dat.full$year)

##### loop over all years, write out as individual files

for(i in 1:length(years)){
  
  dat<-dat.full%>%
    filter(year==years[i])
  
  write_csv(x=dat, path=paste("cw_cleaned_",years[i], ".csv",sep=""))
  gc()
}

#### source script to transform data into worker and state-level files

source("transform_data.r")

q(save="no")