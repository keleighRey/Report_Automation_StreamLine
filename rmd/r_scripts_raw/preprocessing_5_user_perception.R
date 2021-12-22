#Preprocessing 5
#user perception data

#combine user perception with the sites table to get the order and pwl

userp.short<-merge(userp.short,pwl,by.x="UPFDH_EVENT_SMAS_HISTORY_ID",by.y = "SITE_HISTORY_ID")


userp.short$SITE_PWL_ID<-as.factor(userp.short$SITE_PWL_ID)


#userp.short$SITE_PWL_ID<-fct_reorder(userp.short$SITE_PWL_ID,userp.short$order)

#make year column and limit to data that is necessary
userp.short$UPFDH_EVENT_SMAS_SAMPLE_DATE<-as.Date(userp.short$UPFDH_EVENT_SMAS_SAMPLE_DATE,"%m/%d/%Y")
userp.short$year<-format(userp.short$UPFDH_EVENT_SMAS_SAMPLE_DATE,"%Y")

#filter to the year desired
userp.short<-userp.short %>% 
  subset(year>=year.input)

contact<-userp.short%>%
  filter(UPFDH_PRIMARY_CONTACT!="")%>%
  filter(UPFDH_SECONDARY_CONTACT!="")%>%
  mutate(Primary=as.numeric(as.factor(UPFDH_PRIMARY_CONTACT)),
         Secondary=as.numeric(as.factor(UPFDH_SECONDARY_CONTACT)))

contact<-contact %>% 
  select(UPFDH_EVENT_SMAS_HISTORY_ID, Primary, Secondary)%>%
  tidyr::pivot_longer(-UPFDH_EVENT_SMAS_HISTORY_ID, names_to = "Contact Type", values_to = "value")

contact<-contact %>% 
  rename(Site=UPFDH_EVENT_SMAS_HISTORY_ID)

#merge with PWL to get PWL
contact<-merge(contact,pwl,by.x="Site",by.y="SITE_HISTORY_ID")
contact<-contact %>% 
  rename(PWL=SITE_PWL_ID)

userp.short[userp.short == -9999] <- NA

Variables<-userp.short%>%
  group_by(UPFDH_EVENT_SMAS_HISTORY_ID)%>%
  summarize(`Water Clarity`=round(mean(as.numeric(UPFDH_WATER_CLARITY), na.rm = TRUE), digits=0),
            `Susp. Phyto.`= round(mean(as.numeric(UPFDH_SUSPENDED_PHYTOPLANKTON), na.rm = TRUE),digits=0),
            Periphyton= round(mean(as.numeric(UPFDH_PERIPHYTON), na.rm = TRUE), digits=0),
            Macro.= round(mean(as.numeric(UPFDH_MACROPHYTE), na.rm = TRUE), digits=0),
            Odor=round(mean(as.numeric(UPFDH_ODOR), na.rm=TRUE), digits=0),
            Trash= round(mean(as.numeric(UPFDH_TRASH), na.rm = TRUE), digits=0),
            `Discharge Pipes`= round(mean(as.numeric(UPFDH_DISCHARGE_PIPE), na.rm = TRUE), digits=0))

Variables<-Variables %>% 
  rename(Site=UPFDH_EVENT_SMAS_HISTORY_ID)

#replace any -999's with NA's
Variables[Variables < 0] <- NA

#merge with PWL to get PWL
Variables<-merge(Variables,pwl,by.x="Site",by.y="SITE_HISTORY_ID")
Variables<-Variables %>% 
  rename(PWL=SITE_PWL_ID) %>% 
  relocate(PWL,.before=Site)

Variables$group<-NULL
Variables$order<-NULL
  

#Dominant values for recreation
DominantP<-userp.short%>%
  filter(UPFDH_PRIMARY_VARIABLE!="")%>%
  group_by(UPFDH_EVENT_SMAS_HISTORY_ID, UPFDH_PRIMARY_VARIABLE)%>%
  count()%>%
  filter(n == max(n))%>%
  summarize(Primary=paste0(UPFDH_PRIMARY_VARIABLE, collapse = ", "))# %>% 
 # order_sites(sites_table = sites, sites_site_col = "SITE_HISTORY_ID", df_site_col = "UPFDH_EVENT_SMAS_HISTORY_ID")


DominantS<-userp.short%>%
  filter(UPFDH_SECONDARY_VARIABLE!="")%>%
  group_by(UPFDH_EVENT_SMAS_HISTORY_ID, UPFDH_SECONDARY_VARIABLE)%>%
  count()%>%
  group_by(UPFDH_EVENT_SMAS_HISTORY_ID)%>%
  filter(n == max(n))%>%
  group_by(UPFDH_EVENT_SMAS_HISTORY_ID)%>%
  summarize(Secondary=paste0(UPFDH_SECONDARY_VARIABLE, collapse=", "))

Dominant<-dplyr::left_join(DominantP, DominantS, by="UPFDH_EVENT_SMAS_HISTORY_ID")

Dominant$UPFDH_PRIMARY_VARIABLE<-NULL
Dominant<-Dominant %>% rename(Sites=UPFDH_EVENT_SMAS_HISTORY_ID)

Dominant<-merge(Dominant, pwl,by.x="Sites",by.y="SITE_HISTORY_ID")

Dominant<-Dominant %>% 
  rename(PWL=SITE_PWL_ID) %>% 
  relocate(PWL,.before=Sites)
Dominant$group<-NULL
Dominant$order<-NULL
Dominant$Primary<-gsub("_"," ",Dominant$Primary)
Dominant$Primary<-stringr::str_to_sentence(Dominant$Primary,locale="en")

#Dominant<-order_sites(Dominant,sites_table=sites, sites_site_col = "SITE_HISTORY_ID", df_site_col = "UPFDH_EVENT_SMAS_HISTORY_ID")

#get things for text
slight.imp<-contact %>% 
  select(PWL,`Contact Type`,value) %>%
  group_by(PWL,`Contact Type`) %>% 
  mutate(mean=mean(value)) %>% 
  filter(mean>=3) %>% 
  distinct()



