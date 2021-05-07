#Preprocessing 2: Chemistry
#11/4/2020

#this will use the shortened df's to prepare them for the .rmd files

#make sure to go into the file that it made and assign group and order, and then read it back in here
file.path=file.path(here::here(),
                    "data")

#sites<-read.csv(paste(file.path,"/sites_to_assign_order.csv",sep = ""),stringsAsFactors = FALSE)

#chemistry names
chem.names<-read.csv(paste(file.path,"/chem_names.csv",sep = ""),stringsAsFactors = FALSE)

###############################################################################################
#Chemistry preprocessing
#Chemistry figures for each analyte
library(stringr)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(pander)
library(dplyr)
library(forcats)


#change the non-detects (blanks) to the reporting limit
#chem.short<-chem.short %>% 
 # mutate(CHR_RESULT_VALUE=case_when(CHR_VALIDATOR_QUAL=="U"~as.numeric(CHR_REPORTING_DETECT_LIMIT)),
  #       TRUE~as.numeric(CHR_RESULT_VALUE))


#merge chem.short and sites to get the order, PWL ID and group
chem_all<-merge(chem.short,sites,by.y="SITE_HISTORY_ID",by.x="CHS_EVENT_SMAS_HISTORY_ID")
chem_all$CHEM_PARAMETER_NAME<-toupper(chem_all$CHEM_PARAMETER_NAME)
chem_all<-chem_all %>% 
  arrange(order)

chem_all$SITE_PWL_ID<-as.factor(chem_all$SITE_PWL_ID)
#take out the Rejected samples
chem_all<-chem_all %>% 
  filter(chem_all$CHR_VALIDATOR_QUAL!="R")


#get the ones that need to be transformed alone
chem.trans<-subset(chem_all,chem_all$CHEM_PARAMETER_UNIT_NOSP=="mg/L"|chem_all$CHEM_PARAMETER_UNIT_NOSP=="ug/L")
chem.trans<-chem.trans %>% 
  arrange(order)

chem.trans$SITE_PWL_ID<-factor(chem.trans$SITE_PWL_ID,ordered = TRUE)

#filter the ones that don't need their axis transformed
chem.no.trans<-chem_all %>% 
filter(chem_all$CHEM_PARAMETER_UNIT_NOSP != "mg/L"& chem_all$CHEM_PARAMETER_UNIT_NOSP != "ug/L") %>% 
 arrange(order)

chem.no.trans$SITE_PWL_ID<-factor(chem.no.trans$SITE_PWL_ID,ordered = TRUE)

###################################################################################
#filter statewide dataset to the chemicals measured here
params.l<-unique(chem.short$CHEM_PARAMETER_NAME)

sbu.chem.statewide<-sbu.chem.statewide %>% 
  filter(CHEM_PARAMETER_NAME %in% params.l)

sbu.chem.statewide<-sbu.chem.statewide %>% 
  mutate_if(is.numeric, round, 3) 

sbu.insitu.statewide<-sbu.insitu.statewide %>% 
  mutate_if(is.numeric, round, 3) 

####################################################################################################
#merge the sites table with the insitu for plotting by group
if (nrow(in.situ.short>0)){
  in.situ.short<-merge(in.situ.short,sites,by.x="ISWC_EVENT_SMAS_HISTORY_ID",by.y="SITE_HISTORY_ID")
}else{print("There is no insitu data available, please change your query")}

in.situ.short<-in.situ.short %>% 
arrange(order)

in.situ.short$SITE_PWL_ID<-factor(in.situ.short$SITE_PWL_ID,ordered = TRUE)


#run the exceedance script

#create the stars for the figure
stars<-temp5 %>% 
  select(PWL_segment,Violations,Parameter) %>% 
  group_by(PWL_segment,Parameter) %>% 
  summarise(Violations=sum(Violations)) %>% 
  mutate(chemical_name=paste(Parameter))

stars$PWL_segment<-as.factor(stars$PWL_segment)

sbu.insitu.statewide<-sbu.insitu.statewide %>% 
  mutate(chemical_name=case_when(
    CHEM_PARAMETER_NAME=="DISSOLVED_OXYGEN"~"Dissolved Oxygen",
    CHEM_PARAMETER_NAME=="TEMPERATURE"~"Temperature",
    CHEM_PARAMETER_NAME=="CONDUCTANCE"~"Conductance",
    CHEM_PARAMETER_NAME=="PCT_SATURATION"~ "Percent Saturation",
    CHEM_PARAMETER_NAME=="SALINITY"~"Salinity",
    CHEM_PARAMETER_NAME=="PH"~"ph",
    TRUE~paste(CHEM_PARAMETER_NAME)
  ))


#confidence intervals
#insitu.statewide<-insitu.statewide %>% 
#mutate(error=(qnorm(0.975)*sd/sqrt(n)),ci95=mean+error,ci.neg95=mean-error)

#chnage names to match the insitu files
stars<-stars %>% 
  mutate(chemical_name=case_when(
    Parameter=="dissolved_oxygen"~"Dissolved Oxygen",
    Parameter=="ph"~"ph",
    Parameter=="total_dissolved_solids"~"Total Dissolved Solids",
    Parameter=="iron"~"iron",
    Parameter=="nitrite"~"Nitrogen, Nitrite",
    Parameter=="phosphorus"~"Phosphorus, Total (As P)"
  ))

stars.insitu<-stars

#change names to match the chemistry files

in.situ.short$CHEM_PARAMETER_NAME<-tolower(in.situ.short$CHEM_PARAMETER_NAME)
in.situ.short$CHEM_PARAMETER_NAME<-str_to_title(in.situ.short$CHEM_PARAMETER_NAME)
chem.trans$CHEM_PARAMETER_NAME<-tolower(chem.trans$CHEM_PARAMETER_NAME)
chem.trans$CHEM_PARAMETER_NAME<-str_to_title(chem.trans$CHEM_PARAMETER_NAME)
chem.no.trans$CHEM_PARAMETER_NAME<-tolower(chem.no.trans$CHEM_PARAMETER_NAME)
chem.no.trans$CHEM_PARAMETER_NAME<-str_to_title(chem.no.trans$CHEM_PARAMETER_NAME)

#make a summary table for dates included in the analysis######################################

dates_chem<-chem.short %>% 
  select(CHS_EVENT_SMAS_HISTORY_ID,CHS_EVENT_SMAS_SAMPLE_DATE) %>%
  group_by(CHS_EVENT_SMAS_HISTORY_ID,CHS_EVENT_SMAS_SAMPLE_DATE) %>% 
  mutate(Chemistry=paste("X")) %>% 
  distinct() %>% 
  rename(sample_id=CHS_EVENT_SMAS_HISTORY_ID,
         date=CHS_EVENT_SMAS_SAMPLE_DATE)
dates_chem$date<-as.Date(dates_chem$date,"%m/%d/%Y")

dates_bap<-metrics.short %>% 
  select(MSSIH_EVENT_SMAS_HISTORY_ID,MSSIH_EVENT_SMAS_SAMPLE_DATE) %>% 
  group_by(MSSIH_EVENT_SMAS_HISTORY_ID,MSSIH_EVENT_SMAS_SAMPLE_DATE) %>% 
  rename(sample_id=MSSIH_EVENT_SMAS_HISTORY_ID,
         date=MSSIH_EVENT_SMAS_SAMPLE_DATE) %>% 
  mutate(Macroinvertebrates=paste("X")) %>% 
  distinct()
dates_bap$date<-as.Date(dates_bap$date,"%m/%d/%Y")
dates_bap$year<-format(dates_bap$date,"%Y")
dates_bap<-dates_bap %>% 
  subset(year>=year.input)

dates_bap$year<-NULL

dates_all<-merge(dates_chem,dates_bap,by=c("sample_id","date"),all = TRUE)
dates_all<-dates_all %>% 
  rename(Site=sample_id,
         Date=date)

