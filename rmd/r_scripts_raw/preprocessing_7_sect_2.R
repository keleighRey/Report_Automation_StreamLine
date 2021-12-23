#Preprocessing 7
#Section 2: Standards and Exceedances
library(stringr)
library(dplyr)

#let's redo the whole thing. take these out when it's incorporated into the .rmd
#source(here::here("rmd/r_scripts_raw/stayCALM_exceedances_updated.R"))
#source(here::here("rmd/r_scripts_raw/functions.R"))
#year.input=2019
#source(here::here("rmd/r_scripts_raw/preprocessing_1_raw.R"))
#year.input=2019
#source(here::here("rmd/r_scripts_raw/preprocessing_3_bap.R"))
#########################################################################################
##########################################################################################get pwl lines for map
#mapl<-read.csv(here::here("data","map_lines_use.csv"),stringsAsFactors = FALSE)

# #map function
# map.me<-function(df1,df2){
#   df1$SITE_LONGITUDE<-as.numeric(df1$SITE_LONGITUDE)
#   df1$SITE_LATITUDE<-as.numeric(df1$SITE_LATITUDE)
#   df2$SITE_LATITUDE<-as.numeric(df2$SITE_LATITUDE)
#   df2$SITE_LONGITUDE<-as.numeric(df2$SITE_LONGITUDE)
#   #bounding box
# library(ggmap)
# nybox<-make_bbox(df1,lon=SITE_LONGITUDE,lat=SITE_LATITUDE)
#  
# ny.map<-qmap(nybox, source = "osm",color="bw")+
#   #geom_path(data=subset(mapl,type=="trib"),aes(x = long, y = lat,group=group),color="steelblue1",alpha=0.5,,na.rm=TRUE)+
#   #geom_path(data=subset(mapl,type=="main"),aes(x = long, y = lat,group=group),color="slateblue3",size=0.9,,na.rm=TRUE)+
# theme(legend.title=element_blank(),legend.margin=margin(10,10,10,10),legend.key = element_rect(colour = NA, fill = NA),legend.background=element_blank())+
# geom_point(data=df1,aes(x=SITE_LONGITUDE,y=SITE_LATITUDE),
#                   color="yellow",size=4)+
#   geom_point(data=df2,aes(x=SITE_LONGITUDE,y=SITE_LATITUDE),color="red",size=4)+
#   geom_label_repel(data=df2,
#    label= df2$SITE_HISTORY_ID, 
#   aes(x=SITE_LONGITUDE,
#   y=SITE_LATITUDE),box.padding   = 0.35, point.padding = 0.5,
#                   segment.color = 'grey50',size=2)
#   
# print(ny.map)
#   
# }

#########################################################################################
#table of class and what's what
##merge units to get the good units
stay_calm_units<-read.csv(here::here("data/col_names/stayCALM_parameters.csv"))
stay_calm_units<-stay_calm_units %>% 
  select(CHEM_PARAMETER_UNIT,CHEM_PARAMETER_UNIT_NOSP) %>% 
  rename(units=CHEM_PARAMETER_UNIT_NOSP) %>% 
  distinct()
stay_calm_units$units<-tolower(stay_calm_units$units)

wqs_table<-wqs_violations$prep_data %>% 
  select(class,parameter,fraction,units,use,narrative) %>% 
  mutate(use=stringr::str_to_sentence(use,locale = "en")) %>% 
  rename(Class=class,
         Parameter=parameter,
         Fraction=fraction,
         Units=units,
         "Applicable Use"=use,
         "Standard Narrative"=narrative
         )

wqs_table$Class<-toupper(wqs_table$Class)
wqs_table<-wqs_table %>% 
  distinct()
wqs_table<-merge(wqs_table,stay_calm_units,by.x="Units",by.y="units")
wqs_table$Units<-NULL
wqs_table<-wqs_table %>% 
  rename(Units=CHEM_PARAMETER_UNIT)
wqs_table$Parameter<-stringr::str_to_title(wqs_table$Parameter)
wqs_table<-wqs_table %>% 
  relocate(Units,.before="Standard Narrative")



##############################################################################################
# #NNC
# #now we need numeric nutrient criteria
# chem.short<-chem.short %>% 
#   mutate(CHEM_PARAMETER_NAME=tolower(CHEM_PARAMETER_NAME))
# 
# nnc<-chem.short %>% 
#   filter(CHEM_PARAMETER_NAME=="phosphorus, total (as p)" & CHR_VALIDATOR_QUAL!="R")
# 
# nnc<-nnc %>% 
#   rename("DATE"=CHS_EVENT_SMAS_SAMPLE_DATE,
#          "SITE_ID"=CHS_EVENT_SMAS_HISTORY_ID)
# nnc$DATE<-as.Date(nnc$DATE,"%m/%d/%Y")
# nnc$year<-format(nnc$DATE,"%Y")
# 
# cla<-chem.short %>% 
#   filter(CHEM_PARAMETER_NAME=="chlorophyll a") %>% 
#   select(CHS_EVENT_SMAS_HISTORY_ID,CHR_RESULT_VALUE,CHS_EVENT_SMAS_SAMPLE_DATE,CHEM_PARAMETER_UNIT) %>% 
#   rename("chla"=CHR_RESULT_VALUE,
#          "DATE"=CHS_EVENT_SMAS_SAMPLE_DATE,
#          SITE_ID=CHS_EVENT_SMAS_HISTORY_ID,
#          cla_units=CHEM_PARAMETER_UNIT)
# cla$DATE<-as.Date(cla$DATE,"%m/%d/%Y")
# cla$year<-format(cla$DATE,"%Y")
# 
# #there are multiple cla records by date for some of the sites
# cla<-cla %>% 
#   group_by(SITE_ID,DATE,cla_units) %>% 
#   mutate(chla_mean=mean(chla))
# cla$chla<-NA
# cla<-cla %>% 
#   distinct()
# 
# #shorten the bap stuff
# bap.1<-metrics.short %>% 
#   select(MSSIH_EVENT_SMAS_HISTORY_ID,MSSIH_EVENT_SMAS_SAMPLE_DATE,MMDH_BIO_ASMT_PROFILE_SCORE) %>% 
#   rename(DATE=MSSIH_EVENT_SMAS_SAMPLE_DATE)
# bap.1$DATE<-as.Date(bap.1$DATE,"%m/%d/%Y")
# bap.1$year<-format(bap.1$DATE,"%Y")
# #dates don't match exactly, so maybe we'll do bap by year?
# 
# #make bap scores by year
# bap.1<-bap.1 %>% 
#   select(MSSIH_EVENT_SMAS_HISTORY_ID,year,MMDH_BIO_ASMT_PROFILE_SCORE) %>% 
#   group_by(MSSIH_EVENT_SMAS_HISTORY_ID,year) %>% 
#   mutate(bap.scr=mean(MMDH_BIO_ASMT_PROFILE_SCORE))
# 
# #remove the individual bap's from the df
# bap.1$MMDH_BIO_ASMT_PROFILE_SCORE<-NULL
# bap.1<-bap.1 %>% 
#   distinct() %>% 
#   rename(SITE_ID=MSSIH_EVENT_SMAS_HISTORY_ID)
# 
# #also need BAP for this one,first merge the nnc (phos) with the bap(by year)-some of these seem to be missing, they were listed as collected with LGSS low gradient, listed. so no bap's can be calculated
# nnc.df<-merge(nnc,bap.1,by=c("SITE_ID","year"),all.x = TRUE)
# 
# #then merge with CLA for chlorophyll
# 
# nnc.df<-merge(nnc.df,cla,by=c("SITE_ID","DATE"), all.x = TRUE)
# nnc.df<-nnc.df %>% 
#   rename(MMDH_BIO_ASMT_PROFILE_SCORE=bap.scr)
# 
# #need pwl id and wq standard
# 
# stds<-sites %>% 
#   select(SITE_HISTORY_ID,SITE_PWL_ID,ECO_REGION,SITE_WATER_QLTY_STANDARD) %>% 
#   rename(SITE_ID=SITE_HISTORY_ID)
# 
# nnc.df<-merge(nnc.df,stds,by="SITE_ID",all.x = TRUE)
# 
# nnc.df<-nnc.df %>% 
#   select(SITE_ID,DATE,CHEM_PARAMETER_NAME,CHR_RESULT_VALUE,CHEM_PARAMETER_UNIT,
#          SITE_PWL_ID,MMDH_BIO_ASMT_PROFILE_SCORE,chla,cla_units,ECO_REGION,
#          SITE_WATER_QLTY_STANDARD,CHEM_PARAMETER_FRACTION)
# 
# #now we have eco-region, bap, pwl id, chla
# #need type of violation
# nnc.df$CHEM_PARAMETER_NAME<-paste("phosphorus")
# nnc.df$CHEM_PARAMETER_FRACTION<-tolower(nnc.df$CHEM_PARAMETER_FRACTION)
# 
# #get the nnc standards in
# nnc.stds<-read.csv(here::here("data","standards","nnc.csv"),stringsAsFactors = FALSE)
# 
# #need to correct the standard listed
# nnc.df$SITE_WATER_QLTY_STANDARD<-tolower(nnc.df$SITE_WATER_QLTY_STANDARD)
# nnc.df<-nnc.df %>% 
#   mutate(classification=case_when(
#     SITE_WATER_QLTY_STANDARD=="aa(t)"~"aa",
#     SITE_WATER_QLTY_STANDARD=="aa(ts)"~"aa",
#     SITE_WATER_QLTY_STANDARD=="a(t)"~"a",
#     SITE_WATER_QLTY_STANDARD=="at(ts)"~"a",
#     SITE_WATER_QLTY_STANDARD=="b(t)"~"b",
#     SITE_WATER_QLTY_STANDARD=="b(ts)"~"b",
#     SITE_WATER_QLTY_STANDARD=="c(t)"~"c",
#     SITE_WATER_QLTY_STANDARD=="c(ts)"~"c",
#     TRUE~paste(SITE_WATER_QLTY_STANDARD)
#   ))
# 
# #merge with nnc standards
# nnc.final.df<-merge(nnc.df,nnc.stds,by=c("classification","ECO_REGION"))
# 
# #change to the correct units-phos is in mg/l so change to ug/l
# nnc.final.df$phos_ug<-nnc.final.df$CHR_RESULT_VALUE*1000
# 
# nnc.final.df<-nnc.final.df %>% 
#   mutate(exceed=case_when(
#     type %in% "aquatic_chronic"& phos_ug>guidance.value_ug & MMDH_BIO_ASMT_PROFILE_SCORE<5 ~1,
#     type %in% "health_water-supply" & phos_ug>guidance.value_ug & 
#       chla > chla_grtr_ug ~1,
#     TRUE ~ 0
#   ))
# 
# #add note for misssing bap
# nnc.final.df$exceed.note<-if_else(is.na(nnc.final.df$MMDH_BIO_ASMT_PROFILE_SCORE), paste("Missing BAP score."),paste(""))
# 
# #and summary for the nnc
# nnc_sum.df<-nnc.final.df %>% 
#   group_by(SITE_ID,CHEM_PARAMETER_NAME,type,CHEM_PARAMETER_UNIT) %>% 
#   summarize(record_count=n(),
#             mean=mean(CHR_RESULT_VALUE,na.rm =TRUE),
#             num_exceed=sum(exceed),
#             min=min(CHR_RESULT_VALUE,na.rm =TRUE),
#             max=max(CHR_RESULT_VALUE),
#             median=median(CHR_RESULT_VALUE,na.rm =TRUE))
# 
# nnc_sum.df<-nnc_sum.df %>% 
#   mutate_if(is.numeric, round, 3) %>% 
#   mutate(Type=
#            case_when(type %in% "aquatic_chronic"~ "Aquatic Chronic",
#                      type %in% "health_water-supply"~ "Health-Water Supply"),)
# nnc_sum.df$type<-NULL 
# nnc_sum.df<-nnc_sum.df %>% 
#   rename(Parameter=CHEM_PARAMETER_NAME,
#          Violation=num_exceed,
#          Min=min,
#          Max=max,
#          Median=median,
#          Mean=mean,
#          "Record Count"=record_count,
#          Units=CHEM_PARAMETER_UNIT)

######################################################################################
#Bap summary
#summary for the BAP
if(bap){
bap.df<-metrics.short %>% 
  select(MSSIH_EVENT_SMAS_HISTORY_ID,MSSIH_EVENT_SMAS_SAMPLE_DATE,MMDH_BIO_ASMT_PROFILE_SCORE,year) %>% 
  rename(DATE=MSSIH_EVENT_SMAS_SAMPLE_DATE) %>% 
  subset(year>=year.input) %>% 
  #filter(year=="2017"|year=="2018"|year=="2019") %>% 
  group_by(MSSIH_EVENT_SMAS_HISTORY_ID,DATE) %>% 
  summarize(BAP=mean(MMDH_BIO_ASMT_PROFILE_SCORE),
            "Standard Deviation"=sd(MMDH_BIO_ASMT_PROFILE_SCORE),
            "Standard Error"=(sd(MMDH_BIO_ASMT_PROFILE_SCORE)/sqrt(n())),
            "Replicate"=n())

bap.df<-bap.df%>% 
  mutate_if(is.numeric, round, 2)
}

#get flow stuff
#flow<-readxl::read_excel(here::here("data","20210607_S_Sample_Event_Info_field_all_fields.xlsx"))

flow<-field.short %>% 
  select(SEIH_EVENT_SMAS_HISTORY_ID,SEIH_EVENT_SMAS_SAMPLE_DATE,SEIH_TOTAL_DISCH) %>%
  mutate(SEIH_EVENT_SMAS_SAMPLE_DATE=as.Date(SEIH_EVENT_SMAS_SAMPLE_DATE,"%d/%m/%Y"),
         year=format(SEIH_EVENT_SMAS_SAMPLE_DATE,"%Y")) %>% 
  subset(SEIH_EVENT_SMAS_HISTORY_ID %in% sites.l) %>% 
  subset(year>=year.input) %>% 
  filter(SEIH_TOTAL_DISCH!="")
#####################################################################################
#chemistry; using the stayCALM
options(digits = 2)

chem_export_2<-chem_export %>%
  select(Site,Parameter,Fraction,Units,Date, Result) %>% 
  distinct() %>% 
  mutate(Result=as.numeric(Result)) %>% 
  group_by(Site,Parameter,Fraction,Units) %>% 
  summarise("Record Count"=n(),mean=mean(Result,na.rm = TRUE),median=median(Result,na.rm = TRUE),
            max=max(Result,na.rm = TRUE),min=min(Result,na.rm = TRUE)) %>% 
  distinct()

chem_export_2<-chem_export_2 %>% 
  mutate_if(is.numeric,round,3)

chem_export_2$mean<-as.character(chem_export_2$mean)
chem_export_2$median<-as.character(chem_export_2$median)
chem_export_2$max<-as.character(chem_export_2$max)
chem_export_2$min<-as.character(chem_export_2$min)

#rename
chem_export_2<-chem_export_2 %>% 
  select(Site,Parameter,Units,Fraction,`Record Count`,mean,median,max,min) %>% 
  rename(Mean=mean,
         Median=median,
         Max=max,
         Min=min)


field<-chem_export_2 %>% 
  mutate(Parameter=tolower(Parameter)) %>% 
  filter(Parameter=="dissolved oxygen saturation"|
         Parameter=="ph"|
         Parameter=="salinity"|
         Parameter=="chlorophyll a (probe)"|
         Parameter=="phycocyanin (probe)"|
         Parameter=="specific conductance"|
         Parameter=="dissolved oxygen"|
           Parameter=="temperature")

field.params<-unique(field$Parameter)#filter the field parameters out of the chemistry tables

chemistry_section2<-chem_export_2 %>% 
    mutate(Parameter=tolower(Parameter)) %>% 
  filter(!Parameter %in% field.params)


prepped<-wqs_violations$prep_data
