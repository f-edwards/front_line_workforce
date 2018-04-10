#######################################################################################
## Data processing for "Characteristics of the front-line child welfare workforce"
## Frank Edwards and Chris Wildeman, Children and Youth Services 2018
## All R scripts written by Frank Edwards
## contact: FEdwards@cornell.edu
## free for public use with citation
###################
## Execute to process partitioned restricted-access NCANDS child file 
## To learn more about the restriced-access NCANDS, or public release NCANDS
## contact NDACAN@cornell.edu or visit ndacan.cornell.edu
#######################################################################################

rm(list=ls())
gc()
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(data.table)

setwd("PATH TO DATA")

files<-c("cw_cleaned_2015.csv",
         "cw_cleaned_2014.csv",
         "cw_cleaned_2013.csv",
         "cw_cleaned_2012.csv",
         "cw_cleaned_2011.csv",
         "cw_cleaned_2010.csv",
         "cw_cleaned_2009.csv",
         "cw_cleaned_2008.csv",
         "cw_cleaned_2007.csv",
         "cw_cleaned_2006.csv",
         "cw_cleaned_2005.csv",
         "cw_cleaned_2004.csv",
         "cw_cleaned_2003.csv")

dat<-fread(files[1])


### GENERATE REPORT-LEVEL DATA, KEEP ONLY FIRST CHILD ON REPORT

dat<-dat%>%
  distinct(RptID, .keep_all=TRUE)

dat$RptDt<-as.Date(dat$RptDt, 
                   "%Y-%m-%d")

##### loop over all files to speed-up data loading

for(i in 2:length(files)){
  dat.temp<-fread(files[i])
  dat.temp<-dat.temp%>%
    distinct(RptID, .keep_all=TRUE)
  dat.temp$RptDt<-as.Date(dat.temp$RptDt, 
                     "%Y-%m-%d")
  dat<-bind_rows(dat, dat.temp)
}

rm(dat.temp)
gc()

### make count of cases by state/year

case_count<-dat%>%
  mutate(SubYr = year(RptDt))%>%
  group_by(StaTerr, SubYr)%>%
  summarise(ncases = n())

write.csv(case_count, 
          "case_count.csv", 
          row.names = FALSE)

##########################################
## Handle missing data
##########################################

dat$suprvid[which(dat$suprvid=="")]<-NA
dat$wrkrid[which(dat$wrkrid=="")]<-NA


#########################################
## Identify bad states
## Any state-year with only one caseworker or
## NA count flagged as bad
## Flag used to filter IDs in output files
#########################################

state_total_workers<-dat%>%
  group_by(StaTerr, year)%>%
  summarise(caseworkers=n_distinct(wrkrid), supervisors=n_distinct(suprvid))

bad_wrkr<-state_total_workers%>%
  filter((caseworkers==1) | is.na(caseworkers))%>%
  mutate(bad_wrkr=TRUE)%>%
  select(StaTerr, year, bad_wrkr)

bad_supr<-state_total_workers%>%
  filter((supervisors==1) | is.na(supervisors))%>%
  mutate(bad_sup=TRUE)%>%
  select(StaTerr, year, bad_sup)

dat<-left_join(dat, bad_wrkr)%>%
  left_join(bad_supr)

#####################################################################
## Make Caseworker and supervisor level data
#####################################################################

CW<-dat%>%
  filter(!(is.na(wrkrid)))%>% 
  group_by(wrkrid, StaTerr)%>%
  summarise(first=min(RptDt),
            last=max(RptDt),
            ncases=n())%>%
  mutate(tenure=last-first,
         caseload = ncases/(as.numeric(tenure)/365))%>%
  ungroup()%>%
  mutate(type="Caseworker")

SUP<-dat%>%
  group_by(suprvid, StaTerr)%>%
  filter(!(is.na(suprvid)))%>%
  summarise(first=min(RptDt),
            last=max(RptDt),
            ncases=n())%>%
  mutate(tenure=last-first,
         caseload = ncases/(as.numeric(tenure)/365))%>% 
  ungroup()%>%
  mutate(type="Supervisor")

## FILTER BASED ON EXCLUSION CRITERIA, Short tenure, high caseload, low total cases

SUP<-SUP%>%
  left_join(dat%>%
              select(suprvid, StaTerr, bad_sup)%>%
              distinct())%>%
  mutate(bad_sup = ifelse((tenure<30)|(caseload>4000)|(ncases<5), 
                 TRUE, bad_sup),
         bad_sup = ifelse(is.na(bad_sup), FALSE, bad_sup), 
         bad_wrkr=FALSE)

CW<-CW%>%
  left_join(dat%>%
              select(wrkrid, StaTerr, bad_wrkr)%>%
              distinct())%>%
  mutate(bad_wrkr = ifelse((tenure<30)|(caseload>400)|(ncases<5),
                          TRUE, bad_wrkr),
         bad_wrkr=ifelse(is.na(bad_wrkr), FALSE, bad_wrkr),
         bad_sup=FALSE)

temp<-left_join(dat%>%
                 select(-bad_wrkr, -bad_sup), 
               CW%>%
                 select(wrkrid, 
                        StaTerr, 
                        bad_wrkr))
dat<-temp%>%
  left_join(SUP%>%
              select(suprvid,
                     StaTerr,
                     bad_sup))

##### Write worker-level file 

CWSUP<-bind_rows(CW%>%
                   rename(idno=wrkrid), 
                 SUP%>%
                   rename(idno=suprvid))

CWSUP<-as.data.frame(CWSUP)
CWSUP[is.na(CWSUP)]<-NA
CWSUP[CWSUP==Inf]<-NA


write_csv(CWSUP%>%
            filter(bad_sup==FALSE,
                   bad_wrkr==FALSE)%>%
            select(-bad_sup,
                   -bad_wrkr), 
          "CW_SUP_clean.csv")

#####################################
## Make turnover measure
## select state, select year 1 as base, count id_n+1%in%id_n, count(id_n+1), create new_ids, total_ids

states<-unique(dat$StaTerr)
years<-unique(dat$year)[order(unique(dat$year))]

turnover<-turnover_sup<-expand.grid(state=factor(states), year=factor(years))
turnover$old_wrkrs<-turnover$carryover_wrkrs<-NA
turnover_sup$old_wrkrs<-turnover_sup$carryover_wrkrs<-NA

#### wrkr

for(i in 1:length(states)){
  st<-states[i]
  temp_st<-dat%>%
    filter(StaTerr==st)%>%
    filter(!(is.na(wrkrid)),
           bad_wrkr==FALSE)

  base<-temp_st%>%
    filter(year==years[1])%>%
    select(wrkrid)%>%
    distinct()
  
  print(st)
  print("base")
  print(nrow(base))
  
  for(j in 2:length(years)){
    yr<-years[j]
    new<-temp_st%>%
      filter(year==yr)%>%
      select(wrkrid)%>%
      distinct()
    
    print(yr)
    print("new")
    print(nrow(new))
    
    wrkr_old_temp<-sum(base$wrkrid%in%new$wrkrid)
    wrkr_total_temp<-nrow(base)
    
    if(wrkr_total_temp!=0){
      output<-turnover%>%
        filter(state==st,
               year==yr)%>%
        mutate(old_wrkrs=wrkr_total_temp,
               carryover_wrkrs=wrkr_old_temp)
    } else{
      output<-turnover%>%
        filter(state==st,
               year==yr)%>%
        mutate(old_wrkrs=NA,
               carryover_wrkrs=NA)
    }
    
    index<-which((turnover$state==st)&
                   (turnover$year==yr))
    
    turnover[index,]<-output
    
    base<-new
  }
}

#### supr

for(i in 1:length(states)){
  st<-states[i]
  temp_st<-dat%>%
    filter(StaTerr==st)%>%
    filter(!(is.na(suprvid)),
           bad_sup==FALSE)
  
  base<-temp_st%>%
    filter(year==years[1])%>%
    select(suprvid)%>%
    distinct()
  
  print(st)
  print("base")
  print(nrow(base))
  
  for(j in 2:length(years)){
    yr<-years[j]
    new<-temp_st%>%
      filter(year==yr)%>%
      select(suprvid)%>%
      distinct()
    
    print(yr)
    print("new")
    print(nrow(new))
    
    wrkr_old_temp<-sum(base$suprvid%in%new$suprvid)
    wrkr_total_temp<-nrow(base)
    
    if(wrkr_total_temp!=0){
      output<-turnover_sup%>%
        filter(state==st,
               year==yr)%>%
        mutate(old_wrkrs=wrkr_total_temp,
               carryover_wrkrs=wrkr_old_temp)
    } else{
      output<-turnover_sup%>%
        filter(state==st,
               year==yr)%>%
        mutate(old_wrkrs=NA,
               carryover_wrkrs=NA)
    }
    
    index<-which((turnover_sup$state==st)&
                   (turnover_sup$year==yr))
    
    turnover_sup[index,]<-output
    
    base<-new
  }
}

turnover$type<-"Caseworker"
turnover_sup$type<-"Supervisor"
turnover[turnover==0]<-NA
turnover_sup[turnover_sup==0]<-NA # zeroes are not plausible

turnover_out<-bind_rows(turnover,
                        turnover_sup)

#### make lower bound on turnover with promotion assumption: lowerbound is all CW promoted to new SUPR
#### quit lower = new SUPR - exit CW
#### old_wrkrs is prior year total workers

#### upper bound for quits - 
####### upper.quits_{t0} = n_distinct(wrkrid_{t0}) - n_distinct(wrkrid_t0 %in% wrkrid_t1)
#### lower bound for quits, assumme all promoted to supr, total exits minus total new supervisors
####### lower.quits_{t0} = upper.quits_{t0} - n_distinct(!(suprvid_t1 %in% suprvid_t0))

cw_quit_bounds<-turnover%>% ###### THIS MAKES THE UPPER AND LOWER BOUNDS FOR CASEWORKER QUITS
  mutate(upper.quits = old_wrkrs - carryover_wrkrs)%>%
  select(state, year, upper.quits)%>%
  left_join(dat%>%  # calculate n_supr = n_distinct(suprvid_t1) for new_supr = n_supr - carryover_wrkrs
              group_by(StaTerr, year)%>%
              summarise(n_supr = n_distinct(suprvid))%>%
              rename(state=StaTerr)%>%
              mutate(year=factor(year))%>%
              ungroup()%>%
              left_join(turnover_sup%>%
                          select(state, year, carryover_wrkrs))%>%
              mutate(new_supr = n_supr - carryover_wrkrs)
  )%>%
  mutate(lower.quits = upper.quits - new_supr)%>%
  mutate(lower.quits = ifelse(lower.quits<0, 0, lower.quits))%>%
  select(state, year, upper.quits, lower.quits)%>%
  mutate(type="Caseworker")

turnover_out<-turnover_out%>%
  filter(year!="2003")

turnover_out<-turnover_out%>%
  mutate(turnover=1 - (carryover_wrkrs/old_wrkrs))%>%
  select(state, year, type, turnover)

### 0.9<turnover<1.0 isn't believable

turnover_out$turnover<-ifelse(turnover_out$turnover>0.9,
                              NA,
                              turnover_out$turnover)

######################
## Write turnover file
######################

turnover_out<-cw_quit_bounds%>%
  left_join(turnover)%>%
  mutate(lower.turnover=lower.quits/old_wrkrs, 
         upper.turnover=upper.quits/old_wrkrs)%>%
  select(state, year, type, lower.turnover)%>%
  right_join(turnover_out) ### upper.turnover is equal to turnover

write_csv(turnover_out, "turnover.csv")

######################
## write workforce file
######################

workforce<-dat%>%
  filter(bad_wrkr==FALSE)%>%
  group_by(StaTerr, year)%>%
  summarise(wrkrs=n_distinct(wrkrid))
  
workforce<-workforce%>%
  left_join(dat%>%
              filter(bad_sup==FALSE)%>%
              group_by(StaTerr, year)%>%
              summarise(suprs=n_distinct(suprvid)))%>%
  mutate(wrkrs = ifelse(wrkrs < 2, NA, wrkrs),
         suprs = ifelse(suprs < 2, NA, suprs))

write_csv(workforce, "workforce.csv")
