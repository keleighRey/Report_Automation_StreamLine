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
#put the list here name of the file-lapply(read.csv(filepath(csv in the iteration, store into the SBU object)))
SBU.sites<-readxl::read_excel(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Sites_ITS/20201117_S_SITE.xlsx",sep=""))

SBU.chem<-read.csv(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Chemistry_ITS/MASTER_S_CHEM_HISTORY_RESULT_2021-05-03.csv",sep = ""),stringsAsFactors = FALSE)

SBU.tox.sediment<-read.csv(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Toxicity_ITS/20201014_S_TOXICITY_SEDIMENT_RESULT.csv",sep = ""),stringsAsFactors = FALSE)

SBU.tox.water<-read.csv(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Toxicity_ITS/20201014_S_TOXICITY_WATER_RESULT.csv",sep = ""),stringsAsFactors = FALSE)

SBU.userp<-readxl::read_excel(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_SBU_Field_ITS/20201020_S_USER_PRECEP_FIELD_DATA_HIST_all_fields.xlsx",sep = ""))

SBU.habitat<-readxl::read_excel(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_SBU_Field_ITS/20201020_S_HABITAT_FIELD_DATA_HISTORY_all_fields.xlsx",sep = ""))

SBU.metrics<-read.csv(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Macro_ITS/metrics.with.all.fields.csv",sep=""),stringsAsFactors=FALSE)

SBU.in.situ.chem<-readxl::read_excel(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_SBU_Field_ITS/20201030_S_IN_SITU_WATER_CHEM_all_fields.xlsx",sep=""))

SBU.chem.sample<-read.csv(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Chemistry_ITS/MASTER_S_CHEM_HISTORY_SAMPLE_2021-05-03.csv",sep=""),stringsAsFactors = FALSE)
#SBU.chem.good<-read.csv(paste(file.path,"/sbu_chem_2001_2019_provis.csv",sep = ""))
#SBU.field<-readxl::read_excel("C:/Users/kareynol/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_SBU_Field_ITS/20201019_S_SAMPLE_EVENT_INFO_all_fields.xlsx")

SBU.field<-readxl::read_excel(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_SBU_Field_ITS/20201019_S_SAMPLE_EVENT_INFO_all_fields.xlsx",sep=""))
#read in the analyte table

analyte<-read.csv(paste(file.path,"/qaqc_analyte_table.csv",sep=""),stringsAsFactors = FALSE)

sbu.chem.all<-merge(SBU.chem,SBU.chem.sample,by.x=c("CHR_SYS_SAMPLE_CDE","CHR_SAMPLE_DEL_GRP"),
                    by.y = c("CHS_SYS_SAMPLE_CDE","CHS_SAMPLE_DEL_GRP"))
rm(SBU.chem,SBU.chem.sample)

#filter out the Equipement blanks and dupes
sbu.chem.all<-sbu.chem.all %>% 
  subset(CHS_DEC_SAMPLE_TYPE_CDE=="N") %>% 
  subset(CHS_SAMPLE_SOURCE=="Field")

#read in the pcode table
pcode<-read.csv(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Chemistry_ITS/MASTER_S_CHEM_PARAMETER_2021-05-03.csv",sep=""),
                stringsAsFactors = FALSE)

#merge with pcode tables to get the parameter names
sbu.chem.all<-merge(sbu.chem.all,pcode,by.x="CHR_PCODE",by.y="CHEM_PARAMETER_PCODE")

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
chem.short<-filter.to.sites(sbu.chem.all,quo(CHS_EVENT_SMAS_HISTORY_ID))
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
    SEIH_EVENT_EXTENT=="Severe"~"high"
  ))

#and then by grabbing just what you need
flow.flags<-field.short %>% 
  select(SEIH_EVENT_SMAS_HISTORY_ID,SEIH_EVENT_SMAS_SAMPLE_DATE,high_flow_flag)
#we'll merge this with chemistry to flag the high flow 
#correct the dates
chem.short$CHS_EVENT_SMAS_SAMPLE_DATE<-as.Date(chem.short$CHS_EVENT_SMAS_SAMPLE_DATE,"%m/%d/%Y")
flow.flags$SEIH_EVENT_SMAS_SAMPLE_DATE<-as.Date(flow.flags$SEIH_EVENT_SMAS_SAMPLE_DATE,"%m/%d/%Y")

chem.short<-merge(chem.short,flow.flags,
                  by.x=c("CHS_EVENT_SMAS_HISTORY_ID","CHS_EVENT_SMAS_SAMPLE_DATE"),
                  by.y=c("SEIH_EVENT_SMAS_HISTORY_ID","SEIH_EVENT_SMAS_SAMPLE_DATE"),
                  all.x = TRUE)

#chem.short<-chem.short %>% 
  #filter(CHR_RESULT_VALUE!=""|!is.na(CHR_RESULT_VALUE))
#3/31/21 leave these in here bc they are non-detects

habitat.short<-filter.to.sites(SBU.habitat,quo(HFDH_EVENT_SMAS_HISTORY_ID))
userp.short<-filter.to.sites(SBU.userp,quo(UPFDH_EVENT_SMAS_HISTORY_ID))
tox.sed.short<-filter.to.sites(SBU.tox.sediment,quo(TSR_EVENT_SMAS_HISTORY_ID))
tox.wat.short<-filter.to.sites(SBU.tox.water,quo(TWR_EVENT_SMAS_HISTORY_ID))
metrics.short<-filter.to.sites(SBU.metrics,quo(MSSIH_EVENT_SMAS_HISTORY_ID))
in.situ.short<-filter.to.sites(SBU.in.situ.chem,quo(ISWC_EVENT_SMAS_HISTORY_ID))


qa.table<-filter.to.sites(sbu.chem.all,quo(CHS_EVENT_SMAS_HISTORY_ID))

#clean up
rm(SBU.habitat,SBU.in.situ.chem,SBU.metrics,SBU.sites,SBU.tox.sediment,SBU.tox.water,SBU.userp,
   sbu.chem.all)

#output sites file to get the order and group

#sites.short<-sites.short %>% 
 # mutate(order="",group="")

#write.csv(sites.short,here::here("data/sites_to_assign_order.csv"),row.names = FALSE)
#(print("Make sure you go in and assign order and grouping, if necessary. Please see file sites_to_assign_order.csv and rename it."))


#re-read in the corrected sites file
#sites<-read.csv(here::here("data",updated_sites_file_name),stringsAsFactors = FALSE)
sites<-sites.short

sites$order<-as.factor(sites$order)

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

#merge w/analyte table to get what was actually sampled
analyte.short<-merge(analyte,chemistry.analytes, by="CHEM_PARAMETER_NAME")

analyte.short<-analyte.short %>% 
  select(!CHEM_PARAMETER_NAME) %>% 
  select(!CHEM_PARAMETER_FRACTION)

#create table for the analyte list/precision method etc
tl.1<-analyte.short %>% 
  rename("Analytical \nLab"=Analytical.Lab,
         "Method"=Standard.Method,
         "Calibration: \n Initial"=Cal.initial,
         "Calibration: \n Ongoing"=cal.ongoing,
         "Calibration: \n Blanks"=cal.blanks,
         "Detection \n Limit"=Method.Detection.Limit,
         "Reporting \n Limit"=Reporting.Limit)



                     