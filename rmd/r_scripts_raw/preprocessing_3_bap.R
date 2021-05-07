#Preprocessing 3: BAP scores

#needs functions and preprocessing 1 to begin
#source("rmd/r_scripts_raw/preprocessing_1.R")
#source("rmd/r_scripts_raw/functions.R")

library(tidyverse)
#print("Please specify the year for BAP scores before continuing.IF you want all, please unhash line 21 and put year as 1973 (beginning of all data records).")

#year.input=2017
#put the desired year here if applicable

#Needs BAP scores from SBU (Preprocessing 1)

#merge with sites table to get PWL ID, group and order

metrics.short<-merge(metrics.short,sites,by.x="MSSIH_EVENT_SMAS_HISTORY_ID",by.y="SITE_HISTORY_ID")
#create year column
metrics.short$MSSIH_EVENT_SMAS_SAMPLE_DATE<-as.Date(metrics.short$MSSIH_EVENT_SMAS_SAMPLE_DATE,
                                                             "%m/%d/%Y")
metrics.short$year<-format(metrics.short$MSSIH_EVENT_SMAS_SAMPLE_DATE,"%Y")

sum.metrics<-metrics.short%>%
  filter(year>=year.input)%>%
  #filter(year>year.input) %>% 
  rename(BAP=MMDH_BIO_ASMT_PROFILE_SCORE)%>%
  group_by(MSSIH_EVENT_SMAS_HISTORY_ID, SITE_PWL_ID)%>%
  #group_by(SITE_PWL_ID)+
  summarise(mean=mean(BAP, na.rm = TRUE),
            N=n(),
            sd=sd(BAP, na.rm = TRUE))%>%
  mutate(se=sd/sqrt(N),
         ci=qt(1 - ((1 - 0.95) / 2), N - 1) * se)

sum.metrics2<-metrics.short%>%
  filter(year>=year.input)%>%
  #filter(year>year.input) %>% 
  rename(BAP=MMDH_BIO_ASMT_PROFILE_SCORE)%>%
  group_by(SITE_PWL_ID)%>%
  #group_by(SITE_PWL_ID)+
  summarise(mean=mean(BAP, na.rm = TRUE),
            N=n(),
            sd=sd(BAP, na.rm = TRUE))%>%
  mutate(se=sd/sqrt(N),
         ci=qt(1 - ((1 - 0.95) / 2), N - 1) * se)


#reorder based on grouping, so have to join the df's to get it back in
sum.metrics<-merge(sum.metrics, sites, by.x=c("MSSIH_EVENT_SMAS_HISTORY_ID","SITE_PWL_ID"),
                   by.y=c("SITE_HISTORY_ID","SITE_PWL_ID"))

sum.metrics$MSSIH_EVENT_SMAS_HISTORY_ID<-factor(sum.metrics$MSSIH_EVENT_SMAS_HISTORY_ID)

#change order to not a factor
sum.metrics$order<-as.numeric(sum.metrics$order)
#arrange by order
sum.metrics<-sum.metrics %>% 
  arrange(order)

#make the PWL ID into an ordered factor for the plot
sum.metrics$SITE_PWL_ID<-fct_reorder(sum.metrics$SITE_PWL_ID, sum.metrics$order)
#same for site, to get the order of either way of plotting correct
sum.metrics$MSSIH_EVENT_SMAS_HISTORY_ID<-fct_reorder(sum.metrics$MSSIH_EVENT_SMAS_HISTORY_ID,
                                                     sum.metrics$group)

#merge sum.metrics2 with sites to get color
#need short sites

pwl.short<-pwl %>% 
  select(SITE_PWL_ID,group) %>% 
  distinct()

sum.metrics2<-merge(sum.metrics2,pwl,by="SITE_PWL_ID")

sum.metrics2$order<-as.numeric(sum.metrics2$order)
sum.metrics2<-sum.metrics2 %>% 
  arrange(order)

sum.metrics2$SITE_PWL_ID<-fct_reorder(sum.metrics2$SITE_PWL_ID, sum.metrics2$order)
sum.metrics2$SITE_HISTORY_ID<-NULL
sum.metrics2$order<-NULL
sum.metrics2<-sum.metrics2 %>% 
  distinct()

#Create labels for the figure

l<-length(sum.metrics$MSSIH_EVENT_SMAS_HISTORY_ID)

pwl.l<-unique(sum.metrics$SITE_PWL_ID)
l.p<-length(pwl.l)

labels.df <- data.frame(
  label = c("Non", "Slight", "Moderate", "Severe"),
  x = max(l)+1,
  y = c(8.75, 6.25, 3.75, 1.25),
  stringsAsFactors = FALSE)
#same for by PWL
labels.df.pwl <- data.frame(
  label = c("Non", "Slight", "Moderate", "Severe"),
  x = max(l.p)+1,
  y = c(8.75, 6.25, 3.75, 1.25),
  stringsAsFactors = FALSE)


#get unique pwl id's that are below 5. 
bap5<-sum.metrics2 %>% 
  subset(mean<=5) %>% 
  select(SITE_PWL_ID)

bap.inc<-sum.metrics2 %>% 
  mutate(mean.plus=mean+ci,mean.mimnus=mean-ci) %>% 
  subset(mean.plus>=5 & mean.plus<=6|mean.mimnus>=4 & mean.mimnus<=5)

