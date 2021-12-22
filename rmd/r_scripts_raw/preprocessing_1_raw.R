#Preprocessing for STREAMS report
#11/4/2020
#Preprocessing 1 will limit SBU files to just the data that you want. 

#start with a list of sites-just site ID for those that you want
library(dplyr)

file.path=file.path(here::here(),
                      "data/")

sites<-read.csv(here::here("data",sites_file_name),stringsAsFactors = FALSE)
#run the formulas
#source("rmd/r_scripts_raw/functions.R")

#read in the SBU tables
#
#put the list here name of the file-lapply(read.csv(filepath(csv in the iteration, store into the SBU object)))
SBU.sites<-read.csv(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Sites_ITS/Master_S_Site_v2_created_2021_12_07.csv",sep=""),
                    fileEncoding="UTF-8-BOM")
#SBU.sites<-readxl::read_excel(here::here("data/sites_master_for_fl.xlsx"))
SBU.sites<-SBU.sites %>%
  rename(SITE_WATER_QLTY_STANDARD=SITE_WQ_STANDARD)

SBU.chem<-read.csv(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Chemistry_ITS/MASTER_S_CHEM_HISTORY_RESULT_2021-05-03.csv",sep = ""),stringsAsFactors = FALSE)
#SBU.chem<-read.csv(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Chemistry_ITS/dodge_RESULT.csv",sep = ""),stringsAsFactors = FALSE)


SBU.tox.sediment<-read.csv(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Toxicity_ITS/MASTER_S_TOXICITY_SEDIMENT_RESULT.csv",sep = ""),stringsAsFactors = FALSE)

SBU.tox.water<-read.csv(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Toxicity_ITS/MASTER_S_TOXICITY_WATER_RESULT.csv",sep = ""),stringsAsFactors = FALSE)

SBU.userp<-readxl::read_excel(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_SBU_Field_ITS/20210608_S_User_Perception_Field_Data_all_fields.xlsx",sep = ""))
#SBU.userp<-readxl::read_excel(here::here("data/Dodge_Creek_Field/c20210924_Dodge_Creek_2021_User_Perception.xlsx"))


SBU.habitat<-readxl::read_excel(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_SBU_Field_ITS/20210608_S_Habitat_field_all_fields.xlsx",sep = ""))
#SBU.habitat<-readxl::read_excel(here::here("data/Dodge_Creek_Field/c20210924_Dodge_Creek_2021_Habitat_Survey.xlsx"))


SBU.metrics<-read.csv(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Macro_ITS/metrics.with.all.fields.csv",sep=""),stringsAsFactors=FALSE)

SBU.in.situ.chem<-read.csv(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_SBU_Field_ITS/Master_S_IN_SITU_WATER_CHEM_v4_Internal_Format_created_2021_12_07.csv",sep=""))
#dodge<-read.csv(here::here("data/Dodge_Creek_Field/c20210924_Dodge_Creek_2021_In_situ.csv"))
#SBU.in.situ.chem<-rbind(SBU.in.situ.chem,dodge)

SBU.chem.sample<-read.csv(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Chemistry_ITS/MASTER_S_CHEM_HISTORY_SAMPLE_2021-05-03.csv",sep=""),stringsAsFactors = FALSE)
#SBU.chem.sample<-read.csv(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Chemistry_ITS/dodge_sample.csv",sep=""),stringsAsFactors = FALSE)

#SBU.chem.good<-read.csv(paste(file.path,"/sbu_chem_2001_2019_provis.csv",sep = ""))
#SBU.field<-readxl::read_excel("C:/Users/kareynol/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_SBU_Field_ITS/20201019_S_SAMPLE_EVENT_INFO_all_fields.xlsx")

SBU.field<-readxl::read_excel(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_SBU_Field_ITS/20210607_S_Sample_Event_Info_field_all_fields.xlsx",sep=""))
#SBU.field<-readxl::read_excel(here::here("data/Dodge_Creek_Field/c20210924_Dodge_Creek_2021_Sample_Event_Info_Survey.xlsx"))

#read in the analyte table

analyte<-read.csv(paste(file.path,"/qaqc_analyte_table.csv",sep=""),stringsAsFactors = FALSE)

sbu.chem.all<-merge(SBU.chem,SBU.chem.sample,by.x=c("CHR_SYS_SAMPLE_CDE","CHR_SAMPLE_DEL_GRP"),
                    by.y = c("CHS_SYS_SAMPLE_CDE","CHS_SAMPLE_DEL_GRP"))
rm(SBU.chem,SBU.chem.sample)

#filter out the Equipement blanks and dupes
sbu.chem.all<-sbu.chem.all %>% 
  subset(CHS_DEC_SAMPLE_TYPE_CDE=="N") %>% 
  subset(CHS_SAMPLE_SOURCE=="Field") %>% 
  subset(!(CHR_RESULT_TYPE_CDE %in% "SUR"))


#read in the pcode table
pcode<-read.csv(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Chemistry_ITS/MASTER_S_CHEM_PARAMETER_2021-05-03.csv",sep=""),
                stringsAsFactors = FALSE)

#merge with pcode tables to get the parameter names
sbu.chem.all<-merge(sbu.chem.all,pcode,by.x="CHR_PCODE",by.y="CHEM_PARAMETER_PCODE") %>% 
    filter(!(CHR_PCODE %in% c(110, 136, 139, 143, 145)))

#change both to numeric
pcode$pcode.num<-as.numeric(pcode$CHEM_PARAMETER_PCODE)
SBU.in.situ.chem$pcode.num<-as.numeric(SBU.in.situ.chem$ISWC_CHEM_PARAMETER_PCODE_VALID)

#merge pcode and insitu
SBU.in.situ.chem<-merge(SBU.in.situ.chem,pcode,by="pcode.num",all.x = TRUE)
SBU.in.situ.chem$ISWC_RESULT<-as.numeric(SBU.in.situ.chem$ISWC_RESULT)

#write.csv(sbu.chem.all,"data/SBU_chem_all_fields.csv",row.names = FALSE)

#create statewide metrics#################################################################
#adding statewide stuff 
#limit to just eh columns we need, this is just 2018-2019
#sbu.chem.statewide<-sbu.chem.all %>% 
 # select(CHR_CHEMICAL_NAME,CHR_RESULT_VALUE,CHR_RESULT_UNIT,CHR_VALIDATOR_QUAL)


#take out the ones that were flagged as rejected
#sbu.chem.statewide<-sbu.chem.statewide %>% 
 # filter(CHR_VALIDATOR_QUAL!="R")

#summarize for statwide averages- CHEMISTRY
#sbu.chem.statewide<-sbu.chem.statewide %>%
 # group_by(CHR_CHEMICAL_NAME) %>% 
  #summarise_at(vars(CHR_RESULT_VALUE),
   #            funs(mean(.,na.rm = TRUE),
    #                sd(.,na.rm = TRUE),n=n(),
     #               q95=quantile(CHR_RESULT_VALUE,.95,na.rm = TRUE),
      #              q75=quantile(CHR_RESULT_VALUE,.75,na.rm = TRUE),
       #             q25=quantile(CHR_RESULT_VALUE,.25,na.rm = TRUE)))

#do it for the provisional data fromt eh qa/qc, this is back to 2001
sbu.chem.statewide<-sbu.chem.all%>% 
  subset(CHR_VALIDATOR_QUAL!="R") %>% 
  group_by(CHEM_PARAMETER_NAME) %>% 
  summarise_at(vars(CHR_RESULT_VALUE),
               funs(mean(.,na.rm = TRUE),
                    sd(.,na.rm = TRUE),n=n(),
                    q95=quantile(CHR_RESULT_VALUE,.95,na.rm = TRUE),
                    q75=quantile(CHR_RESULT_VALUE,.75,na.rm = TRUE),
                    q25=quantile(CHR_RESULT_VALUE,.25,na.rm = TRUE)))

#summarize for statwide averages- INSITU


sbu.insitu.statewide<-SBU.in.situ.chem %>%
  filter(ISWC_RESULT!=-9999) %>% 
  group_by(CHEM_PARAMETER_NAME) %>% 
  summarise_at(vars(ISWC_RESULT),
               funs(mean(.,na.rm = TRUE),
                    sd(.,na.rm = TRUE),n=n(),
                    q95=quantile(ISWC_RESULT,.95,na.rm = TRUE),
                    q75=quantile(ISWC_RESULT,.75,na.rm = TRUE),
                    q25=quantile(ISWC_RESULT,.25,na.rm = TRUE)))

##################################################################################################
#create the short tables with only the sites that are necessary.

sites.l<-unique(sites$SH_SITE_ID)

sites.short<-filter.to.sites(SBU.sites,quo(SITE_HISTORY_ID))
#merge with original sites file for order and group

sites.short<-merge(sites.short,sites,by.x="SITE_HISTORY_ID",by.y="SH_SITE_ID")

#chemistry
#fl external monitoring
chem.short<-filter.to.sites(sbu.chem.all,quo(CHS_EVENT_SMAS_HISTORY_ID))

# #######fr Finger Lakes monitoring_______________________________________________________--
# ext<-read.csv(here::here("data/Final_Advanced_MP_External_102221.csv"))
# chem.short<-plyr::rbind.fill(chem.short, ext)
# #_________________________________________________________________________________________--

chem.short<-chem.short %>% 
  subset(CHR_VALIDATOR_QUAL!="R")#remove the rejected samples from the df

#flag the event samples, first by subsetting the field information 
field.short<-filter.to.sites(SBU.field,quo(SEIH_EVENT_SMAS_HISTORY_ID))

field.short<-field.short %>% 
  select(SEIH_EVENT_SMAS_HISTORY_ID,SEIH_EVENT_SMAS_SAMPLE_DATE,
         SEIH_EVENT_BASELINE,SEIH_EVENT_EXTENT,SEIH_TOTAL_DISCH) %>% 
  group_by(SEIH_EVENT_SMAS_HISTORY_ID,SEIH_EVENT_SMAS_SAMPLE_DATE) %>% 
  mutate(average_flow=mean(SEIH_TOTAL_DISCH,na.rm = TRUE),sd=sd(SEIH_TOTAL_DISCH,na.rm = TRUE)) %>% 
  mutate(high_flow_flag=case_when(
    SEIH_TOTAL_DISCH > sd*2~"high",
    SEIH_EVENT_EXTENT=="Severe"~"high",
    TRUE~""
  ))

#and then by grabbing just what you need
flow.flags<-field.short %>% 
  select(SEIH_EVENT_SMAS_HISTORY_ID,SEIH_EVENT_SMAS_SAMPLE_DATE,high_flow_flag) %>% 
  distinct()
#we'll merge this with chemistry to flag the high flow 
#correct the dates
chem.short$CHS_EVENT_SMAS_SAMPLE_DATE<-as.Date(chem.short$CHS_EVENT_SMAS_SAMPLE_DATE,"%m/%d/%Y")
flow.flags$SEIH_EVENT_SMAS_SAMPLE_DATE<-as.Date(flow.flags$SEIH_EVENT_SMAS_SAMPLE_DATE,"%m/%d/%Y")

chem.short$high_flow_flag<-NULL

chem.short<-merge(chem.short,flow.flags,
                  by.x=c("CHS_EVENT_SMAS_HISTORY_ID","CHS_EVENT_SMAS_SAMPLE_DATE"),
                  by.y=c("SEIH_EVENT_SMAS_HISTORY_ID","SEIH_EVENT_SMAS_SAMPLE_DATE"),
                  all.x = TRUE)
#merge with the insitu too
SBU.in.situ.chem<-merge(SBU.in.situ.chem,flow.flags,
                  by.x=c("ISWC_EVENT_SMAS_HISTORY_ID","ISWC_EVENT_SMAS_SAMPLE_DATE"),
                  by.y=c("SEIH_EVENT_SMAS_HISTORY_ID","SEIH_EVENT_SMAS_SAMPLE_DATE"),
                  all.x = TRUE)
#clean up the high flow flags

chem.short<-chem.short %>% 
  mutate(high_flow_flag=case_when(is.na(high_flow_flag)~"",
                                  high_flow_flag=="NA"~"",
                                  TRUE~high_flow_flag))

SBU.in.situ.chem<-SBU.in.situ.chem %>% 
  mutate(high_flow_flag=case_when(is.na(high_flow_flag)~"",
                                  high_flow_flag=="NA"~"",
                                  TRUE~high_flow_flag))
#chem.short<-chem.short %>% 
  #filter(CHR_RESULT_VALUE!=""|!is.na(CHR_RESULT_VALUE))
#3/31/21 leave these in here bc they are non-detects

habitat.short<-filter.to.sites(SBU.habitat,quo(HFDH_EVENT_SMAS_HISTORY_ID))
userp.short<-filter.to.sites(SBU.userp,quo(UPFDH_EVENT_SMAS_HISTORY_ID))
tox.sed.short<-filter.to.sites(SBU.tox.sediment,quo(EVENT_SMAS_ID))
tox.wat.short<-filter.to.sites(SBU.tox.water,quo(EVENT_SMAS_ID))
metrics.short<-filter.to.sites(SBU.metrics,quo(MSSIH_EVENT_SMAS_HISTORY_ID))
in.situ.short<-filter.to.sites(SBU.in.situ.chem,quo(ISWC_EVENT_SMAS_HISTORY_ID))



qa.table<-filter.to.sites(sbu.chem.all,quo(CHS_EVENT_SMAS_HISTORY_ID))

#clean up
rm(SBU.habitat,SBU.in.situ.chem,SBU.metrics,SBU.sites,SBU.tox.sediment,SBU.tox.water,SBU.userp,
   sbu.chem.all)

sites<-sites.short

#make the df for pwl addition back in
pwl<-sites %>% 
  select(SITE_HISTORY_ID,SITE_PWL_ID,order,group)


#read in the intro file
intro<-read.csv(here::here("data/intro.csv"),stringsAsFactors = FALSE)

###################################################################################
#create year columns for chemistry data sets
chem.short$CHS_EVENT_SMAS_SAMPLE_DATE<-as.Date(chem.short$CHS_EVENT_SMAS_SAMPLE_DATE,"%m/%d/%Y")

chem.short$year<-format(chem.short$CHS_EVENT_SMAS_SAMPLE_DATE,"%Y")
#limit to the years we want
chem.short<-chem.short %>% 
  subset(year>=year.input) %>% 
  subset(CHR_VALIDATOR_QUAL!="R")

in.situ.short$ISWC_EVENT_SMAS_SAMPLE_DATE<-as.Date(in.situ.short$ISWC_EVENT_SMAS_SAMPLE_DATE,"%m/%d/%Y")
in.situ.short$year<-format(in.situ.short$ISWC_EVENT_SMAS_SAMPLE_DATE,"%Y")

in.situ.short<-in.situ.short %>% 
  filter(year>=year.input)

#analyte table
chemistry.analytes<-chem.short %>% 
  select(CHEM_PARAMETER_NAME,CHEM_PARAMETER_FRACTION) %>% 
  distinct()

#merge with in_situ too
in.situ.short_analyte<-in.situ.short %>% 
select(CHEM_PARAMETER_NAME,CHEM_PARAMETER_FRACTION) %>% 
  distinct()

analyte.all<-rbind(chemistry.analytes,in.situ.short_analyte)
analyte.all$CHEM_PARAMETER_NAME<-toupper(analyte.all$CHEM_PARAMETER_NAME)


#merge w/analyte table to get what was actually sampled
analyte.short<-merge(analyte,analyte.all, by=c("CHEM_PARAMETER_NAME",
                                                      "CHEM_PARAMETER_FRACTION"))

analyte.short<-analyte.short %>% 
  select(!CHEM_PARAMETER_NAME) %>% 
  select(!CHEM_PARAMETER_FRACTION) %>% 
  arrange(Analytical.Lab)

#create table for the analyte list/precision method etc
analyte_final<-analyte.short %>% 
  rename("Analytical \nLab"=Analytical.Lab,
         "Method"=Standard.Method,
         "Calibration: \n Initial"=Cal.initial,
         "Calibration: \n Ongoing"=cal.ongoing,
         "Calibration: \n Blanks"=cal.blanks,
         "Detection \n Limit"=Method.Detection.Limit,
         "Reporting \n Limit"=Reporting.Limit)

#make the results have 1/2 the MDL for analysis
chem.short<-chem.short %>% 
  mutate(CHR_RESULT_VALUE=case_when(CHR_VALIDATOR_QUAL=="U"~as.numeric(CHR_METHOD_DETECT_LIMIT*0.5),
         TRUE~as.numeric(CHR_RESULT_VALUE)))
###################################################################################
#summarize sites table
##Table 1
#renames column in the sites table to match
library(dplyr)

table1<-sites %>% 
select(SITE_HISTORY_ID,group,SITE_NAME,SITE_PWL_ID,SITE_WATER_QLTY_STANDARD,SITE_DESC,SITE_LATITUDE,SITE_LONGITUDE,order) %>% 
  distinct() %>% 
  arrange(order)

table1<-table1 %>% rename("Location ID"=SITE_HISTORY_ID,
                          "Group"=group,
                  "Stream"=SITE_NAME,
                  "WI/PWL ID"=SITE_PWL_ID,
                  "Waterbody Classification"=SITE_WATER_QLTY_STANDARD,
                  "Description"=SITE_DESC,
                  "Latitude"=SITE_LATITUDE,
                  "Longitude"=SITE_LONGITUDE)

table1$Description<-toupper(table1$Description)
table1$Latitude<-as.numeric(table1$Latitude)
table1$Latitude<-round(table1$Latitude,digits = 5)
table1$Longitude<-as.numeric(table1$Longitude)
table1$Longitude<-round(table1$Longitude,digits = 5)
table1<-table1 %>% 
  mutate(Latitude=as.character(Latitude),Longitude=as.character(Longitude))
table1$Description<-stringr::str_to_sentence(table1$Description,locale = "en")

#remove order column for table disply
table1$order<-NULL

table1<-table1 %>% 
  rename("Waterbody \n Classification"=`Waterbody Classification`)

                     