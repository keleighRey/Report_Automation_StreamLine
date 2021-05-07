#Preprocessing 7
#Section 2: Standards and Exceedances
library(stringr)
library(dplyr)

#let's redo the whole thing. take these out when it's incorporated into the .rmd
source(here::here("rmd/r_scripts_raw/stayCALM_exceedances.R"))
year.input=2019
source(here::here("rmd/r_scripts_raw/preprocessing_1_raw.R"))
year.input=2019
source(here::here("rmd/r_scripts_raw/preprocessing_3_bap.R"))


#########################################################################################
#table of class and what's what

wqs_table<-wqs.df %>% 
  select(class,parameter,fraction,units,narrative) %>% 
  rename("Waterbody Class"=class,
         Parameter=parameter,
         Fraction=fraction,
         Units=units,
         "Standard Narrative"=narrative
         )
wqs_table$`Waterbody Class`<-toupper(wqs_table$`Waterbody Class`)


##############################################################################################
#NNC
#now we need numeric nutrient criteria
chem.short<-chem.short %>% 
  mutate(CHEM_PARAMETER_NAME=tolower(CHEM_PARAMETER_NAME))

nnc<-chem.short %>% 
  filter(CHEM_PARAMETER_NAME=="phosphorus, total (as p)" & CHR_VALIDATOR_QUAL!="R")

nnc<-nnc %>% 
  rename("DATE"=CHS_EVENT_SMAS_SAMPLE_DATE,
         "SITE_ID"=CHS_EVENT_SMAS_HISTORY_ID)
nnc$DATE<-as.Date(nnc$DATE,"%m/%d/%Y")
nnc$year<-format(nnc$DATE,"%Y")

cla<-chem.short %>% 
  filter(CHEM_PARAMETER_NAME=="chlorophyll a") %>% 
  select(CHS_EVENT_SMAS_HISTORY_ID,CHR_RESULT_VALUE,CHS_EVENT_SMAS_SAMPLE_DATE,CHEM_PARAMETER_UNIT) %>% 
  rename("chla"=CHR_RESULT_VALUE,
         "DATE"=CHS_EVENT_SMAS_SAMPLE_DATE,
         SITE_ID=CHS_EVENT_SMAS_HISTORY_ID,
         cla_units=CHEM_PARAMETER_UNIT)
cla$DATE<-as.Date(cla$DATE,"%m/%d/%Y")
cla$year<-format(cla$DATE,"%Y")

#there are multiple cla records by date for some of the sites
cla<-cla %>% 
  group_by(SITE_ID,DATE,cla_units) %>% 
  mutate(chla_mean=mean(chla))
cla$chla<-NA
cla<-cla %>% 
  distinct()

#shorten the bap stuff
bap.1<-metrics.short %>% 
  select(MSSIH_EVENT_SMAS_HISTORY_ID,MSSIH_EVENT_SMAS_SAMPLE_DATE,MMDH_BIO_ASMT_PROFILE_SCORE) %>% 
  rename(DATE=MSSIH_EVENT_SMAS_SAMPLE_DATE)
bap.1$DATE<-as.Date(bap.1$DATE,"%m/%d/%Y")
bap.1$year<-format(bap.1$DATE,"%Y")
#dates don't match exactly, so maybe we'll do bap by year?

#make bap scores by year
bap.1<-bap.1 %>% 
  select(MSSIH_EVENT_SMAS_HISTORY_ID,year,MMDH_BIO_ASMT_PROFILE_SCORE) %>% 
  group_by(MSSIH_EVENT_SMAS_HISTORY_ID,year) %>% 
  mutate(bap.scr=mean(MMDH_BIO_ASMT_PROFILE_SCORE))

#remove the individual bap's from the df
bap.1$MMDH_BIO_ASMT_PROFILE_SCORE<-NULL
bap.1<-bap.1 %>% 
  distinct() %>% 
  rename(SITE_ID=MSSIH_EVENT_SMAS_HISTORY_ID)

#also need BAP for this one,first merge the nnc (phos) with the bap(by year)-some of these seem to be missing, they were listed as collected with LGSS low gradient, listed. so no bap's can be calculated
nnc.df<-merge(nnc,bap.1,by=c("SITE_ID","year"),all.x = TRUE)

#then merge with CLA for chlorophyll

nnc.df<-merge(nnc.df,cla,by=c("SITE_ID","DATE"), all.x = TRUE)
nnc.df<-nnc.df %>% 
  rename(MMDH_BIO_ASMT_PROFILE_SCORE=bap.scr)

#need pwl id and wq standard

stds<-sites %>% 
  select(SITE_HISTORY_ID,SITE_PWL_ID,ECO_REGION,SITE_WATER_QLTY_STANDARD) %>% 
  rename(SITE_ID=SITE_HISTORY_ID)

nnc.df<-merge(nnc.df,stds,by="SITE_ID",all.x = TRUE)

nnc.df<-nnc.df %>% 
  select(SITE_ID,DATE,CHEM_PARAMETER_NAME,CHR_RESULT_VALUE,CHEM_PARAMETER_UNIT,
         SITE_PWL_ID,MMDH_BIO_ASMT_PROFILE_SCORE,chla,cla_units,ECO_REGION,
         SITE_WATER_QLTY_STANDARD,CHEM_PARAMETER_FRACTION)

#now we have eco-region, bap, pwl id, chla
#need type of violation
nnc.df$CHEM_PARAMETER_NAME<-paste("phosphorus")
nnc.df$CHEM_PARAMETER_FRACTION<-tolower(nnc.df$CHEM_PARAMETER_FRACTION)

#get the nnc standards in
nnc.stds<-read.csv(here::here("data","standards","nnc.csv"),stringsAsFactors = FALSE)

#need to correct the standard listed
nnc.df$SITE_WATER_QLTY_STANDARD<-tolower(nnc.df$SITE_WATER_QLTY_STANDARD)
nnc.df<-nnc.df %>% 
  mutate(classification=case_when(
    SITE_WATER_QLTY_STANDARD=="aa(t)"~"aa",
    SITE_WATER_QLTY_STANDARD=="aa(ts)"~"aa",
    SITE_WATER_QLTY_STANDARD=="a(t)"~"a",
    SITE_WATER_QLTY_STANDARD=="at(ts)"~"a",
    SITE_WATER_QLTY_STANDARD=="b(t)"~"b",
    SITE_WATER_QLTY_STANDARD=="b(ts)"~"b",
    SITE_WATER_QLTY_STANDARD=="c(t)"~"c",
    SITE_WATER_QLTY_STANDARD=="c(ts)"~"c",
    TRUE~paste(SITE_WATER_QLTY_STANDARD)
  ))

#merge with nnc standards
nnc.final.df<-merge(nnc.df,nnc.stds,by=c("classification","ECO_REGION"))

#change to the correct units-phos is in mg/l so change to ug/l
nnc.final.df$phos_ug<-nnc.final.df$CHR_RESULT_VALUE*1000

nnc.final.df<-nnc.final.df %>% 
  mutate(exceed=case_when(
    type %in% "aquatic_chronic"& phos_ug>guidance.value_ug & MMDH_BIO_ASMT_PROFILE_SCORE<5 ~1,
    type %in% "health_water-supply" & phos_ug>guidance.value_ug & 
      chla > chla_grtr_ug ~1,
    TRUE ~ 0
  ))

#add note for misssing bap
nnc.final.df$exceed.note<-if_else(is.na(nnc.final.df$MMDH_BIO_ASMT_PROFILE_SCORE), paste("Missing BAP score."),paste(""))

#and summary for the nnc
nnc_sum.df<-nnc.final.df %>% 
  group_by(SITE_ID,CHEM_PARAMETER_NAME,type,CHEM_PARAMETER_UNIT) %>% 
  summarize(record_count=n(),
            mean=mean(CHR_RESULT_VALUE,na.rm =TRUE),
            num_exceed=sum(exceed),
            min=min(CHR_RESULT_VALUE,na.rm =TRUE),
            max=max(CHR_RESULT_VALUE),
            median=median(CHR_RESULT_VALUE,na.rm =TRUE))

nnc_sum.df<-nnc_sum.df %>% 
  mutate_if(is.numeric, round, 3) %>% 
  mutate(Type=
           case_when(type %in% "aquatic_chronic"~ "Aquatic Chronic",
                     type %in% "health_water-supply"~ "Health-Water Supply"),)
nnc_sum.df$type<-NULL 
nnc_sum.df<-nnc_sum.df %>% 
  rename(Parameter=CHEM_PARAMETER_NAME,
         Violation=num_exceed,
         Min=min,
         Max=max,
         Median=median,
         Mean=mean,
         "Record Count"=record_count,
         Units=CHEM_PARAMETER_UNIT)

######################################################################################
#Bap summary
#summary for the BAP
bap.df<-metrics.short %>% 
  select(MSSIH_EVENT_SMAS_HISTORY_ID,MSSIH_EVENT_SMAS_SAMPLE_DATE,MMDH_BIO_ASMT_PROFILE_SCORE,year) %>% 
  rename(DATE=MSSIH_EVENT_SMAS_SAMPLE_DATE) %>% 
  subset(year>=year.input) %>% 
  #filter(year=="2017"|year=="2018"|year=="2019") %>% 
  group_by(MSSIH_EVENT_SMAS_HISTORY_ID,DATE) %>% 
  summarize(BAP=mean(MMDH_BIO_ASMT_PROFILE_SCORE),"Standard Deviation"=sd(MMDH_BIO_ASMT_PROFILE_SCORE),n=n())

bap.df<-bap.df%>% 
  mutate_if(is.numeric, round, 2)

#######################################################################################
#Exceedance table
#create summary columns for exceedances in all chemistry (including field)
#create year column
temp4$year<-format(temp4$date,"%Y")

temp4<-temp4 %>% 
  filter(year>=2017)

#group by year/date and then use the TRUE/FALSE column to tally the exceedances
temp.5<-temp4 %>% 
  group_by(site_id,year,parameter) %>% 
  mutate(exceed=case_when(
    WQS_attain_combined=="FALSE"~1,
    TRUE~0
  )) %>% 
  summarise(Exceedances=sum(exceed)) %>%
  filter(Exceedances!=0) %>% 
  rename(Site=site_id,
         Year=year,
         Parameter=parameter)

temp.5$Parameter<-gsub("_"," ",temp.5$Parameter)
temp.5$Parameter<-str_to_title(temp.5$Parameter)

all_exceedances.df<-selected.df%>%
  filter(attaining_wqs==FALSE)%>%
  unite(Standard, c("direction", "threshold"), sep = "")%>%
  arrange(site_id, year)%>%
  select(Site=site_id, Year=year, Parameter=parameter, Units=units, Standard, Result=result)

all_exceedances.df<-all_exceedances.df %>% 
  mutate(Parameter=case_when(
    Parameter=="dissolved_oxygen"~"Dissolved Oxygen",
    Parameter=="total_dissolved_solids"~"Total Dissolved Solids",
    Parameter=="iron"~"Iron",
    Parameter=="ph"~"pH"
  ))
#####################################################################################
#create summary tables for teh rest of the chemistry

insitu_sum.df<-in.situ.short %>% 
  group_by(ISWC_EVENT_SMAS_HISTORY_ID,CHEM_PARAMETER_NAME,CHEM_PARAMETER_UNIT) %>% 
  summarise("Record Count"=n(),Mean=mean(ISWC_RESULT,na.rm = TRUE),
            Median=median(ISWC_RESULT,na.rm = TRUE),
            SD=sd(ISWC_RESULT,na.rm = TRUE),
            Max=max(ISWC_RESULT,na.rm = TRUE),
            Min=min(ISWC_RESULT,na.rm = TRUE)) %>% 
  rename(Units=CHEM_PARAMETER_UNIT,
         SITE_ID=ISWC_EVENT_SMAS_HISTORY_ID) %>% 
  mutate(Parameter=tolower(CHEM_PARAMETER_NAME)) %>% 
  mutate(Parameter=str_to_title(CHEM_PARAMETER_NAME)) %>% 
  relocate(Parameter,.before="SITE_ID")
  
insitu_sum.df<-insitu_sum.df %>% mutate_if(is.numeric, round, 3)
insitu_sum.df$CHEM_PARAMETER_NAME<-NULL


chem_sum.df<-chem.short %>% group_by(CHS_EVENT_SMAS_HISTORY_ID,CHEM_PARAMETER_NAME,CHEM_PARAMETER_UNIT) %>% 
  summarise("Record Count"=n(),Mean=mean(CHR_RESULT_VALUE,na.rm = TRUE),
            Median=median(CHR_RESULT_VALUE,na.rm = TRUE),
            SD=sd(CHR_RESULT_VALUE,na.rm = TRUE),
            Max=max(CHR_RESULT_VALUE,na.rm=TRUE),
            Min=min(CHR_RESULT_VALUE,na.rm=TRUE))

chem_sum.df<-chem_sum.df %>% mutate_if(is.numeric, round, 3)
#chem_sum.df$chemical_name<-str_to_lower(chem_sum.df$chemical_name)
#chem_sum.df$chemical_name<-str_to_title(chem_sum.df$chemical_name)

chem_sum.df<-chem_sum.df %>% 
  rename(Parameter=CHEM_PARAMETER_NAME,
         Units=CHEM_PARAMETER_UNIT,
         SITE_ID=CHS_EVENT_SMAS_HISTORY_ID) %>% 
  mutate(Parameter=str_to_title(Parameter))


chem_sum.df<-chem_sum.df %>% 
  relocate(Parameter,.before=SITE_ID)

#get flow stuff
flow<-read.csv(here::here("data","20201019_S_SAMPLE_EVENT_INFO_all_fields.csv"),stringsAsFactors = FALSE)

flow<-flow %>% 
  select(SEIH_EVENT_SMAS_HISTORY_ID,SEIH_EVENT_SMAS_SAMPLE_DATE,SEIH_DISCH_COLLECT,SEIH_TOTAL_DISCH) %>%
  mutate(SEIH_EVENT_SMAS_SAMPLE_DATE=as.Date(SEIH_EVENT_SMAS_SAMPLE_DATE,"%d/%m/%Y"),
         year=format(SEIH_EVENT_SMAS_SAMPLE_DATE,"%Y")) %>% 
  subset(SEIH_EVENT_SMAS_HISTORY_ID %in% sites.l) %>% 
  subset(year>=year.input) %>% 
  filter(SEIH_TOTAL_DISCH!="")
  