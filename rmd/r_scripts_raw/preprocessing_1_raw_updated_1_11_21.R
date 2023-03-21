#Preprocessing for STREAMS report
#11/4/2020
#updated 1/11/21 for reading in master files instead of changing names
#Preprocessing 1 will limit SBU files to just the data that you want. 

#start with a list of sites-just site ID for those that you want
library(dplyr)

file.path=file.path(here::here(),
                      "data/")

sites<-read.csv(here::here("data",sites_file_name),stringsAsFactors = FALSE)
#run the formulas
#source("rmd/r_scripts_raw/functions.R")

#read in the SBU tables
##############################################################################
SBU.tox.sediment<-read.csv(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Toxicity_ITS/MASTER_S_TOXICITY_SEDIMENT_RESULT.csv",sep = ""),stringsAsFactors = FALSE)

SBU.tox.water<-read.csv(paste("C:/Users/",user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Toxicity_ITS/MASTER_S_TOXICITY_WATER_RESULT.csv",sep = ""),stringsAsFactors = FALSE)

analyte<-read.csv(paste(file.path,"/qaqc_analyte_table.csv",sep=""),stringsAsFactors = FALSE)
#################################################################################
#read in SBU data that is the "master" files
library(dplyr)
library(readxl)

# read in sites and initialize sites list to include all sites 
#read in data that have the "master" tag
db_path<-paste("C:/Users/",params$user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization",sep = "")

sites_path <- file.path(
  db_path,
  "Cleaned Files",
  "Final_Sites_ITS"
)
# # Get the file paths for the filenames with the prefix "MASTER" and
# # extension CSV.
sites_csv_list <- list.files(
  path = sites_path,
  pattern = "Master(.+?)csv",
  full.names = TRUE
)
# Identify the appropriate name for each file path.
sites_csv_names <- case_when(
  grepl("Master_S_Site", sites_csv_list) ~ "SBU.sites",
  TRUE ~ "ERROR"
)
# Assign easy to reference names to filepaths.
names(sites_csv_list) <- sites_csv_names
# Reading in macro data -------------------------------------------------
## Loop through CSV list, import data, store in a list.
sites_raw_list <- lapply(sites_csv_list, function(file_i) {
  # Import data
  read.csv(
    file_i,
    na.strings = c("", "NA"),
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM"
  )})
# SBU.sites<-read.csv("C:/Users/kareynol/New York State Office of Information Technology Services/SMAS - Streams Data Modernization/Cleaned Files/Final_Sites_ITS/Master_S_Site_v2_created_2021_12_07.csv",
#                     fileEncoding = "UTF-8-BOM")

SBU.sites<-sites_raw_list$SBU.sites

SBU.sites<-SBU.sites %>%
  rename(SITE_WATER_QLTY_STANDARD=SITE_WQ_STANDARD)

# #__________________________________________________________________________
# ##FOR FINGER LAKES DATA
# SBU.sites<-readxl::read_excel(here::here("data/sites_master_for_fl.xlsx"))
# #__________________________________________________________________________


##########################################################
#Chem
chem_path <- file.path(
  db_path,
  "Cleaned Files",
  "Final_Chemistry_ITS"
)
# Get the file paths for the filenames with the prefix "MASTER" and
# extension CSV.
chem_csv_list <- list.files(
  path = chem_path,
  pattern = "MASTER(.+?)csv",
  full.names = TRUE
)
# Identify the appropriate name for each file path.
chem_csv_names <- case_when(
  grepl("RESULT", chem_csv_list) ~ "result",
  grepl("SAMPLE", chem_csv_list) ~ "sample",
  grepl("PARAMETER", chem_csv_list) ~ "pcode",
  
  TRUE ~ "ERROR"
)
# Assign easy to reference names to filepaths.
names(chem_csv_list) <- chem_csv_names
# Reading in chem data -------------------------------------------------
## Loop through CSV list, import data, store in a list.
chem_raw_list <- lapply(chem_csv_list, function(file_i) {
  # Import data
  read.csv(
    file_i,
    na.strings = c("", "NA"),
    stringsAsFactors = FALSE
  )})
# Join chem Data ----------------------------------------------------------

chem.all<-merge(chem_raw_list$result,chem_raw_list$sample,
                by.x=c("CHR_SYS_SAMPLE_CDE","CHR_SAMPLE_DEL_GRP"),
                by.y=c("CHS_SYS_SAMPLE_CDE","CHS_SAMPLE_DEL_GRP"))

chem.all<-chem.all %>% 
  subset(CHS_DEC_SAMPLE_TYPE_CDE=="N") %>% 
  subset(CHS_SAMPLE_SOURCE=="Field") %>% 
  subset(!(CHR_RESULT_TYPE_CDE %in% "SUR")) %>% 
  subset(CHR_RESULT_TYPE_CDE %in% "TRG")



#change both to numeric
chem_raw_list$pcode$pcode.num<-as.numeric(chem_raw_list$pcode$CHEM_PARAMETER_PCODE)
pcode<-chem_raw_list$pcode

#merge pcode and chemistry
sbu.chem.all<-merge(chem.all,chem_raw_list$pcode,by.x="CHR_PCODE",by.y="pcode.num",all.x = TRUE) %>% 
  #filter out lab pH, lab temperature, and lab specific conductance
  filter(!(CHR_PCODE %in% c(110, 136, 139, 143, 145)))

#clean up\
rm(chem.all)
###################################################################################
#field data
field_path <- file.path(
  db_path,
  "Cleaned Files",
  "Final_SBU_Field_ITS"
)
# Get the file paths for the filenames with the prefix "MASTER" and
# extension CSV.
field_csv_list <- list.files(
  path = field_path,
  pattern = "(.+?)csv",
  full.names = TRUE
)
# Identify the appropriate name for each file path.
field_csv_names <- case_when(
  grepl("User_Perception", field_csv_list) ~ "userp",
  grepl("Habitat", field_csv_list) ~ "habitat",
  grepl("IN_SITU", field_csv_list) ~ "insitu",
  grepl("Sample_Event", field_csv_list) ~ "sample_info",
  TRUE ~ "ERROR"
)
# Assign easy to reference names to filepaths.
names(field_csv_list) <- field_csv_names
# Reading in macro data -------------------------------------------------
## Loop through CSV list, import data, store in a list.
field_raw_list <- lapply(field_csv_list, function(file_i) {
  # Import data
  read.csv(
    file_i,
    na.strings = c("", "NA"),
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8-BOM")})

#merge insitu and pcode
field_raw_list$insitu$pcode.num<-as.numeric(field_raw_list$insitu$ISWC_CHEM_PARAMETER_PCODE_VALID)

#merge pcode and insitu
field_raw_list$insitu<-merge(field_raw_list$insitu,chem_raw_list$pcode,by="pcode.num",all.x = TRUE)

SBU.in.situ.chem<-field_raw_list$insitu
SBU.habitat<-field_raw_list$habitat
SBU.userp<-field_raw_list$userp
SBU.field<-field_raw_list$sample_info
################################################################################

db_path<-paste("C:/Users/",params$user,"/New York State Office of Information Technology Services/SMAS - Streams Data Modernization",sep = "")

macro_path <- file.path(
  db_path,
  "Cleaned Files",
  "Final_Macro_ITS"
)
# Get the file paths for the filenames with the prefix "MASTER" and
# extension CSV.
macro_csv_list <- list.files(
  path = macro_path,
  pattern = "MASTER(.+?)csv",
  full.names = TRUE
)
# Identify the appropriate name for each file path.
macro_csv_names <- case_when(
  grepl("METRICS", macro_csv_list) ~ "metrics",
  grepl("SPECIES_SAMP_INF", macro_csv_list) ~ "bug_method",
  grepl("SPECIES_DATA_HISTORY",macro_csv_list)~"raw_bugs",
  TRUE ~ "ERROR"
)
# Assign easy to reference names to filepaths.
names(macro_csv_list) <- macro_csv_names
# Reading in macro data -------------------------------------------------
## Loop through CSV list, import data, store in a list.
macro_raw_list <- lapply(macro_csv_list, function(file_i) {
  # Import data
  read.csv(
    file_i,
    na.strings = c("", "NA"),
    stringsAsFactors = FALSE
  )})

# Join Macro Data ----------------------------------------------------------
SBU.metrics <- merge(
  x = macro_raw_list$metrics,
  y = macro_raw_list$bug_method,
  by.x = "MMDH_LINKED_ID_VALIDATOR",
  by.y="MSSIH_LINKED_ID_VALIDATOR"
)

############################################################################
#clean up
rm(field_raw_list,macro_csv_list,macro_raw_list)
###############################################################
#merge insitu

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

# # For finger lakes report -------------------------------------------------
# chem.short$month<-format(as.Date(chem.short$CHS_EVENT_SMAS_SAMPLE_DATE,"%m/%d/%Y"),"%m")
# 
# chem.short<-chem.short %>% 
#   filter(month<=10)
# # For finger lakes report -------------------------------------------------


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
# # For finger lakes report -------------------------------------------------
# field.short$month<-format(as.Date(field.short$SEIH_EVENT_SMAS_SAMPLE_DATE,"%m/%d/%Y"),"%m")
# 
# field.short<-field.short %>% 
#   filter(month<=10)
# # For finger lakes report -------------------------------------------------


#and then by grabbing just what you need
flow.flags<-field.short %>% 
  select(SEIH_EVENT_SMAS_HISTORY_ID,SEIH_EVENT_SMAS_SAMPLE_DATE,high_flow_flag) %>% 
  distinct()

# # #######fr Finger Lakes monitoring_______________________________________________________--
# flow.flags.2<-ext %>%
#   select(CHS_EVENT_SMAS_HISTORY_ID,CHS_EVENT_SMAS_SAMPLE_DATE,high_flow_flag) %>%
#   rename(SEIH_EVENT_SMAS_HISTORY_ID=CHS_EVENT_SMAS_HISTORY_ID,
#          SEIH_EVENT_SMAS_SAMPLE_DATE=CHS_EVENT_SMAS_SAMPLE_DATE)
# 
# flow.flags<-rbind(flow.flags,flow.flags.2)
# flow.flags<-flow.flags %>%
#   distinct()
# # #######fr Finger Lakes monitoring_______________________________________________________--

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

# For finger lakes report -------------------------------------------------
in.situ.short$month<-format(as.Date(in.situ.short$ISWC_EVENT_SMAS_SAMPLE_DATE,"%m/%d/%Y"),"%m")

in.situ.short<-in.situ.short %>% 
  filter(month<=10)
# For finger lakes report -------------------------------------------------



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
analyte.all$CHEM_PARAMETER_FRACTION<-case_when(
  is.na(analyte.all$CHEM_PARAMETER_FRACTION)~"",
  TRUE~analyte.all$CHEM_PARAMETER_FRACTION)


#merge w/analyte table to get what was actually sampled
analyte.short<-merge(analyte,analyte.all, by=c("CHEM_PARAMETER_NAME",
                                                      "CHEM_PARAMETER_FRACTION"))

analyte.short<-analyte.short %>% 
  select(!CHEM_PARAMETER_NAME) %>% 
  select(!CHEM_PARAMETER_FRACTION) %>% 
  arrange(Analytical.Lab)

#create table for the analyte list/precision method etc
analyte_final<-analyte.short %>% 
  rename("Analytical Lab"=Analytical.Lab,
         "Method"=Standard.Method,
         "Calibration: Initial"=Cal.initial,
         "Calibration: Ongoing"=cal.ongoing,
         "Calibration: Blanks"=cal.blanks,
         "Detection Limit"=Method.Detection.Limit,
         "Reporting Limit"=Reporting.Limit)

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
