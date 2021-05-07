#Preprocessing 6
#Microtox

#select and shorten the files
#merge the other 2
#tox.df<-merge(tox.sed.short,tox.wat.short,all = TRUE)

#TSR is the vibrio stuff, the TWR stuff is the ceriodaphnia 

tox.df<-tox.sed.short %>% 
  select(TSR_EVENT_SMAS_HISTORY_ID,TSR_EVENT_SMAS_SAMPLE_DATE,TSR_SEDIMENT_ASMT,
         TSR_SEDIMENT_ASMT,TSR_POREWATER_ASMT,TSR_POREWATER_RSLT,TSR_SEDIMENT_RSLT)

#make year column and  limit to what you want
tox.df$TSR_EVENT_SMAS_SAMPLE_DATE<-as.Date(tox.df$TSR_EVENT_SMAS_SAMPLE_DATE,"%m/%d/%Y")
tox.df$year<-format(tox.df$TSR_EVENT_SMAS_SAMPLE_DATE,"%Y")
tox.df<-tox.df %>% 
  subset(year>=year.input)

#merge with overall sites table to get the order

sites.short<-sites %>% 
  select(SITE_HISTORY_ID,group,order,SITE_WATER_QLTY_STANDARD,SITE_PWL_ID)

tox.df<-merge(tox.df,sites.short,by.x="TSR_EVENT_SMAS_HISTORY_ID",by.y="SITE_HISTORY_ID")

#select the columns to keep
tox.df<-tox.df %>% 
  arrange(group) %>% 
  select(!c(SITE_WATER_QLTY_STANDARD,SITE_PWL_ID,group,order))


tox.tbl<-tox.df %>% 
  rename("Station ID"=TSR_EVENT_SMAS_HISTORY_ID,
         "Sample Date"=TSR_EVENT_SMAS_SAMPLE_DATE,
         "Sediment \n Assessment"=TSR_SEDIMENT_ASMT,
         "Porewater \n Assessment"=TSR_POREWATER_ASMT,
         "Sediment \n EC50"=TSR_SEDIMENT_RSLT,
         "Porewater \n EC50"=TSR_POREWATER_RSLT)
