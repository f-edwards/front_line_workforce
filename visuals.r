#######################################################################################
## Data processing for "Characteristics of the front-line child welfare workforce"
## Frank Edwards and Chris Wildeman, Children and Youth Services 2018
## All R scripts written by Frank Edwards
## contact: FEdwards@cornell.edu
## free for public use with citation
###################
## Execute to produce visuals from processed NCANDS child file restricted data
## To learn more about the restriced-access NCANDS, or public release NCANDS
## contact NDACAN@cornell.edu or visit ndacan.cornell.edu
#######################################################################################

rm(list=ls()); gc()

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2); theme_set(theme_minimal())

setwd("S:/CR3178/Projects/caseworkers")
CWSUP<-read_csv("CW_SUP_clean.csv")
turnover<-read_csv("turnover.csv")

turnover_out<-left_join(turnover%>%
                          select(state, year, type, lower.turnover)%>%
                          spread(type, lower.turnover)%>%
                          rename(lower.turnover.caseworker = Caseworker, 
                                 lower.turnover.supervisor = Supervisor),
                        turnover%>%
                          select(state, year, type, turnover)%>%
                          spread(type, turnover)%>%
                          rename(upper.turnover.caseworker = Caseworker, 
                                 turnover.supervisor = Supervisor))%>%
  select(-lower.turnover.supervisor)%>%
  filter(!(is.na(upper.turnover.caseworker)))

write_csv(turnover_out, "turnover_state_year.csv")

workforce<-read_csv("workforce.csv")
### Child poverty data obtained from Census SAIPE API
### census.gov/programs-surveys/saipe/data/api.html
child_pov<-read_csv("SAIPE_Child_Pov_TS.csv") 
# to crosswalk state names to state abbreviations
data(state) 
crosswalk<-data.frame(StaTerr=as.character(state.abb), NAME=as.character(state.name), stringsAsFactors = FALSE)
crosswalk<-rbind(crosswalk, c("DC", "District of Columbia"))

child_pov<-left_join(child_pov, 
                     crosswalk)%>%
  rename(chpov=SAEPOV0_17_PT,
         year=time)%>%
  select(StaTerr, year, chpov)

##########################################################################
### OHIO HAS DATA THAT ONLY POPS UP FOR 07-10, and has unbelievable supervisor turnover
### Drop PR and OH and all missing and erroneous IDs
##########################################################################

turnover<-turnover%>%
  filter(state!="PR", state!="OH")

CWSUP<-CWSUP%>%
  filter(!(is.na(idno)))

CWSUP<-CWSUP%>% 
  filter(StaTerr!="PR", StaTerr!="OH")

### HI, LA, NC only report 1 supervisor

CWSUP<-CWSUP%>%
  filter(!(((StaTerr=="HI")&(type=="Supervisor"))|
           ((StaTerr=="LA")&(type=="Supervisor"))|
           ((StaTerr=="NC")&(type=="Supervisor"))))

CWSUP$caseload<-CWSUP$ncases/(CWSUP$tenure/365)

### Filtering on states excluded from other files by exclusion criteria

workforce<-workforce%>%
  rename(Caseworker = wrkrs,
         Supervisor = suprs)%>%
  gather(key=type, 
         value = workers, 
         Caseworker, Supervisor, 
         -StaTerr, -year)%>%
  filter(StaTerr!="OH",
         StaTerr!="PR",
         StaTerr!="PA",
         StaTerr!="VT",
         StaTerr!="NY")

### calculate, output capacity measure

capacity_out<-workforce%>%
  left_join(child_pov)%>%
  filter(year>2003)%>%
  spread(type, workers)%>%
  rename(caseworkers = Caseworker,
         supervisors = Supervisor)%>%
  mutate(caseworker_capacity = caseworkers / chpov * 1000,
         supervisor_capacity = supervisors / chpov * 1000)

case_count<-read.csv("case_count.csv", stringsAsFactors = FALSE)

case_count_out<-case_count%>%
  rename(year = SubYr,
         screened_in_reports=ncases)

write_csv(case_count_out, "state_screened_in_reports.csv")

capacity_out<-capacity_out%>%
  left_join(case_count_out)

write_csv(capacity_out, 
          "state_capacity.csv")

capacity<-workforce%>%
  left_join(child_pov)%>%
  filter(year>2003)%>%
  mutate(capacity = workers / chpov * 1000)

### alternate capacity measure, workers per screened-in reports

capacity_per_rpt_all<-capacity%>%
  filter(type == "Caseworker")%>%
  left_join(case_count%>%
              rename(year = SubYr))%>%
  mutate(capacity_rpt = workers / ncases * 100)

capacity_per_rpt<-capacity_per_rpt_all%>%
  group_by(StaTerr)%>%
  summarise(median_capacity = median (capacity_rpt, na.rm=TRUE))%>%
  arrange(median_capacity)%>%
  mutate(StaTerr = factor(StaTerr, levels = unique(StaTerr)))

### Appendix A
cor(capacity_per_rpt_all$capacity, capacity_per_rpt_all$capacity_rpt)

ggplot(capacity_per_rpt_all,
       aes(x=capacity,
           y=capacity_rpt))+
  geom_point()+
  ylab("Workers per 100 reports")+
  xlab("Workers per 1,000 children in poverty")+
  ggsave("capacity_rpt_pov.png")


capacity_per_wrkr_all<-capacity%>%
  filter(type=="Supervisor")%>%
  rename(supervisors = workers)%>%
  left_join(capacity%>%
              filter(type=="Caseworker")%>%
              select(-type, -capacity, -chpov))%>%
    mutate(capacity_wrkr = workers/supervisors)


capacity_per_wrkr<-capacity_per_wrkr_all%>%
  group_by(StaTerr)%>%
  summarise(median_capacity = median (capacity_wrkr, na.rm=TRUE))%>%
  filter(!(is.na(median_capacity)))%>%
  arrange(median_capacity)%>%
  mutate(StaTerr = factor(StaTerr, levels = unique(StaTerr)))

cor(na.omit(capacity_per_wrkr_all$capacity), na.omit(capacity_per_wrkr_all$capacity_wrkr))

ggplot(capacity_per_wrkr_all,
       aes(x=capacity,
           y=capacity_wrkr,
           color= StaTerr))+
  geom_point()+
  ylab("Workers per supervisor")+
  xlab("Supervisors per 1,000 children in poverty")+
  ggsave("capacity_wrkr_pov.png")


capacity_st<-capacity%>%
  group_by(StaTerr, type)%>%
  summarise(median_capacity = median(capacity, na.rm=TRUE))%>%
  arrange(type, median_capacity)

capacity_st$StaTerr<-factor(capacity_st$StaTerr,
                             levels=unique(capacity_st$StaTerr))

### for paper descriptives and visuals

capacity_quantiles<-capacity%>%
  group_by(type)%>%
  summarise(q_25=quantile(capacity, 0.25, na.rm=TRUE),
            q_50 = quantile(capacity, 0.5, na.rm=TRUE),
            q_75 = quantile(capacity, 0.75, na.rm=TRUE))

#############################################################
## Fig 1
#############################################################

ggplot(capacity_st,
       aes(x=StaTerr,
           y=median_capacity,
           color=type))+
  geom_point()+
  ylab("Median workers per 1,000 children in poverty")+
  xlab("")+
  coord_flip()+
  theme(axis.text=element_text(size=7))+
  labs(color="")+
  ggsave("Capacity_State.tiff", height=8, width=7)

#############################################################
## Exploratory capacity visuals
#############################################################

ggplot(capacity_per_rpt,
       aes(x=StaTerr,
           y=median_capacity))+
  geom_point()+
  ylab("Median workers per 100 screened-in maltreatment reports")+
  xlab("")+
  coord_flip()+
  theme(axis.text=element_text(size=7))+
  labs(color="")+
  ggsave("Capacity_per_rpt.tiff", height=8, width=7)


ggplot(capacity_per_wrkr,
       aes(x=StaTerr,
           y=median_capacity))+
  geom_point()+
  ylab("Median caseworkers per supervisor")+
  xlab("")+
  coord_flip()+
  theme(axis.text=element_text(size=7))+
  labs(color="")+
  ggsave("Capacity_per_wrkr.tiff", height=8, width=7)

#############################################################
## Fig 2
#############################################################

ggplot(capacity,
       aes(x=year, y = capacity, col = type))+
  geom_line()+
  facet_wrap(~StaTerr)+
  xlab("Year")+
  ylab("Workers per 1,000 children in poverty")+
  scale_x_continuous(breaks=2010)+
  theme(legend.title=element_blank())+
  ggsave("Capacity_TS.tiff", height=8, width=7)

capacity_cw1<-lm(capacity ~ 0 + I(year - 2004) + StaTerr,
                data=capacity%>%
                  filter(type=="Caseworker"))

capacity_s1<-lm(capacity ~ 0 + I(year - 2004) + StaTerr,
                data=capacity%>%
                  filter(type=="Supervisor"))

#############################################################
## Fig 3
#############################################################

CW_CL_quant<-CWSUP%>%
  group_by(type)%>%
  summarise(lower=quantile(caseload, 0.25),
            median=median(caseload),
  upper=quantile(caseload, 0.75),
  uppeR_90=quantile(caseload, 0.9),
  CV=sd(caseload)/mean(caseload))%>%
    ungroup()

ggplot(CWSUP%>%
         filter(caseload<800),
       aes(x=caseload))+
  geom_histogram(alpha = 0.8, fill=1, binwidth = 3)+
  xlab("Cases per year")+
  ylab("")+
  #coord_cartesian(xlim=c(0,1500))+
  facet_wrap(~type, ncol=1, scales="free")+
  theme(axis.text.y=element_blank())+
  ggsave("NatlCaseload.tiff", height=8, width=7)

#############################################################
## State caseload table and file
#############################################################

state_caseload_wrkr<-CWSUP%>%
  filter(type=="Caseworker")%>%
  group_by(StaTerr)%>%
  summarise(median_caseload=median(ncases/(tenure/365)), 
            upper_caseload=quantile(ncases/(tenure/365), 0.75),
            lower_caseload=quantile(ncases/(tenure/365), 0.25),
            mean=mean(ncases/(tenure/365)))%>%
  arrange(median_caseload)

state_caseload_wrkr$StaTerr<-factor(state_caseload_wrkr$StaTerr, 
                                    levels=state_caseload_wrkr$StaTerr)

#### State-level data files


state_caseload<-CWSUP%>%
  group_by(StaTerr, type)%>%
  summarise(median_caseload=median(ncases/(tenure/365)), 
            upper_caseload=quantile(ncases/(tenure/365), 0.75),
            lower_caseload=quantile(ncases/(tenure/365), 0.25),
            count=n())%>%
  arrange(type, median_caseload)

state_caseload_out<-CWSUP%>%
  group_by(StaTerr, type)%>%
  summarise(cases_median = median(ncases),
            cases_75thPct = quantile(ncases, 0.75),
            cases_25thPct = quantile(ncases, 0.25),
            cases_mean = mean(ncases),
            cases_sd = sd(ncases),
            tenure_median_yrs = median(tenure/365),
            tenure_75thPct = quantile(tenure/365, 0.75),
            tenure_25thPct = quantile(tenure/365, 0.25),
            tenure_mean = mean(tenure/365),
            tenure_sd = sd(tenure/365),
            caseload_median=median(ncases/(tenure/365)), 
            caseload_75thPct=quantile(ncases/(tenure/365), 0.75),
            caseload_25thPct=quantile(ncases/(tenure/365), 0.25),
            caseload_mean = mean(ncases/(tenure/365)),
            caseload_sd = sd(ncases/(tenure/365)))%>%
  arrange(type)

write_csv(state_caseload_out, "state_wrkr_data.csv")

state_caseload$StaTerr<-factor(state_caseload$StaTerr, 
                                    levels=unique(state_caseload$StaTerr))

#############################################################
## Fig 4
#############################################################

ggplot(state_caseload,
       aes(x=StaTerr, 
           y=median_caseload,
           col = type))+
  geom_point()+
  ylab("Median annual cases per worker")+
  xlab("")+
  coord_flip()+
  theme(legend.title=element_blank())+
  labs(fill="")+
  theme(axis.text=element_text(size=7))+
  ggsave("state_caseload.tiff", height=8, width=7)

#### TENURE
#############################################################
## Fig 5
#############################################################

ggplot(CWSUP, aes(x=tenure/365, fill=type))+ #consider dropping first case in 2015 to avoid inflation from new workers?
  geom_density(alpha=0.5)+
  xlab("Tenure in years")+
  labs(fill="")+
  ylab("")+
  ggsave("TenureNatl.tiff", height=8, width=7)

state_tenure<-CWSUP%>%
  group_by(StaTerr, type)%>%
  summarise(median_tenure=median(tenure/365))%>%
  arrange(type, median_tenure)

write_csv(state_tenure, "state_tenure.csv")

descriptives_tenure<-CWSUP%>%
  group_by(type)%>%
  summarise(median_tenure=median(tenure/365),
            tenure_75=quantile(tenure/365, 0.75),
            tenure_25=quantile(tenure/365, 0.25),
            cv=sd(tenure/365)/mean(tenure/365))

state_tenure$StaTerr<-factor(state_tenure$StaTerr,
                             levels=unique(state_tenure$StaTerr))

#############################################################
## Fig 6
#############################################################

ggplot(state_tenure,
       aes(x=StaTerr,
           y=median_tenure,
           color=type))+
  geom_point()+
  ylab("Median tenure in years")+
  xlab("")+
  coord_flip()+
  theme(axis.text=element_text(size=7))+
  labs(color="")+
  ggsave("Tenure_State.tiff", height=8, width=7)


#############################################################
## Fig 7
#############################################################

turnover_st<-turnover%>%
  filter(!(is.na(turnover)))%>%
  group_by(state, type)%>%
  summarise(median=median(turnover, na.rm=TRUE),
            median.lower=median(lower.turnover))%>%
  arrange(type, median)

turnover_st$state<-factor(turnover_st$state,
                          levels=unique(turnover_st$state))

ggplot(turnover_st,
       aes(x=state,
           y=median,
           color=type))+
  geom_point()+
  geom_linerange(aes(x=state, 
                    ymin=median.lower,
                    ymax=median,
                    color=type))+
  ylab("Median turnover")+
  xlab("")+
  coord_flip()+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_blank())+
  labs(fill="")+
  ggsave("Turnover_State.tiff", height=8, width=7)


medians<-turnover%>%
  group_by(type)%>%
  summarise(median.upper = median(turnover, na.rm=TRUE),
            median.lower = median(lower.turnover, na.rm=TRUE))

#############################################################
## Fig 8
#############################################################

ggplot(turnover%>%
         filter(!(is.na(turnover))),
       aes(y=turnover, x=year, col=type))+
  facet_wrap(~state)+
  geom_line()+
  geom_ribbon(aes(ymin=lower.turnover, ymax=turnover, fill=type),alpha=0.5, col=NA)+
  xlab("Year")+
  ylab("Turnover")+
  scale_x_continuous(breaks=2010)+
  scale_y_continuous(breaks=c(0, 0.3, 0.6))+
  theme(legend.title=element_blank())+
  ggsave("Turnover_TS.tiff", height=8, width=7)

#### national trends

turnover_cw1<-lm(turnover ~ I(year - 2004) + state,
                data=turnover%>%
                  filter(type=="Caseworker"))

turnover_s1<-lm(turnover ~ I(year - 2004) + state,
                data=turnover%>%
                  filter(type=="Supervisor"))
