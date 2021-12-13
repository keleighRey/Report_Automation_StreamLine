#stayCALM_2
#updated 2/23/2021 for using regular chemistry
#updated 10/1/2021 for new scripties

#run the preprocessing 1 file that limits chemistry and insitu data to what we want
source(here::here("rmd","r_scripts_raw/functions.R"))
source(here::here("rmd","r_scripts_raw/preprocessing_1_raw.R"))

#this one does a lot so we can clean up what we don't need
rm(analyte,habitat.short,metrics.short,
   sbu.chem.statewide,sbu.insitu.statewide,tox.sed.short,tox.wat.short,userp.short)

#first read in the pwl to get the segment id in both files
pwl<-sites.short %>% 
  select(SITE_HISTORY_ID,SITE_PWL_ID)

#chem.short<-chem.short %>% 
 # mutate(SITE_PWL_ID=case_when(is.na(SITE_PWL_ID)~paste(PWL_FL),
  #                             TRUE~paste(SITE_PWL_ID)))

#make the results have 1/2 the MDL for analysis
chem.short<-chem.short %>% 
  mutate(CHR_RESULT_VALUE=case_when(CHR_VALIDATOR_QUAL=="U"~as.numeric(CHR_METHOD_DETECT_LIMIT*0.5),
         TRUE~as.numeric(CHR_RESULT_VALUE))) %>% 
  subset(CHR_VALIDATOR_QUAL!="R")
#merge them both to get the PWL ids
chem.short<-merge(chem.short,pwl, by.x="CHS_EVENT_SMAS_HISTORY_ID",by.y="SITE_HISTORY_ID")

in.situ.short<-merge(in.situ.short,pwl, by.x="ISWC_EVENT_SMAS_HISTORY_ID",by.y="SITE_HISTORY_ID")

#read in the colnames change 
colnames_ref<-read.csv(here::here("data/col_names/stayCALM_colnames.csv"),stringsAsFactors = FALSE)


#function to change colnames for chemistry
colnames_smas_chem<-function(df){
  chem.good<-df
  col.from<-names(chem.good)
  col.vec.short<-colnames_ref %>% 
    subset(chem_db_col %in% col.from)
  
  names(chem.good)[match(col.vec.short[,"chem_db_col"],names(chem.good))]=col.vec.short[,"stayCALM_col"] #match the column names
  
  #subset the new names from what you just did and then remove the rest
  forstayCALM.colvec<-unique(col.vec.short$stayCALM_col)
  
  chem.good<-chem.good[,forstayCALM.colvec]
  
  #create empty data frame for the stayCALM package to merge with the final to have all of the columns you need
  
  #first create the list for all stay CALM colnames
  sc_all_cols<-unique(colnames_ref$stayCALM_col)
  df1 <- data.frame(matrix(vector(),ncol=max(length(sc_all_cols))))#creates an empty data frame
  colnames(df1) <-c(sc_all_cols) #renames the columns
  
  chem.good<-merge(chem.good,df1,all.x=TRUE)
  .GlobalEnv$chem.good<- chem.good #make available
  
}

colnames_smas_chem(chem.short)

#function to change colnames for insitu
colnames_smas_insitu<-function(df){
  insitu.good<-df
  col.from<-names(insitu.good)
  col.vec.short<-colnames_ref %>% 
    subset(insitu_db_col %in% col.from)
  
  names(insitu.good)[match(col.vec.short[,"insitu_db_col"],names(insitu.good))]=col.vec.short[,"stayCALM_col"] #match the column names
  
  #subset the new names from what you just did and then remove the rest
  forstayCALM.colvec<-unique(col.vec.short$stayCALM_col)
  
  insitu.good<-insitu.good[,forstayCALM.colvec]
  
  #create empty data frame for the stayCALM package to merge with the final to have all of the columns you need
  
  #first create the list for all stay CALM colnames
  sc_all_cols<-unique(colnames_ref$stayCALM_col)
  df1 <- data.frame(matrix(vector(),ncol=max(length(sc_all_cols))))#creates an empty data frame
  colnames(df1) <-c(sc_all_cols) #renames the columns
  
  insitu.good<-merge(insitu.good,df1,all.x=TRUE)
  .GlobalEnv$insitu.good<- insitu.good #make available
  
}

colnames_smas_insitu(in.situ.short)
#change parameter names to match for the stayCALM package to run

#bind the two into one df
#first fix the date for insitu.good
insitu.good$date<-as.Date(insitu.good$date,"%m/%d/%Y")

all.chemistry<-rbind(chem.good,insitu.good)

all.chemistry$fraction<-tolower(all.chemistry$fraction)
all.chemistry$units<-tolower(all.chemistry$units)



#correct the parameter names for stayCALM

#read in the parameter correction file
param.names<-read.csv(paste(file.path,"/col_names/stayCALM_parameters.csv",sep = ""),stringsAsFactors = FALSE)

#fix the DO fraction
all.chemistry<-all.chemistry %>% 
  mutate(fraction=case_when(parameter=="DISSOLVED OXYGEN"& units=="mg/l"~"dissolved",
                            parameter=="PH" & units=="ph units"~"total",
                            TRUE~paste(fraction)))


param.names.short<-param.names %>% 
  select(CHEM_PARAMETER_NAME,stayCALM_parameter,stayCALM_units,stayCALM_fraction) %>% 
  distinct()

all.chemistry<-merge(all.chemistry,param.names.short,
                     by.x=c("parameter","fraction","units"),
                     by.y=c("CHEM_PARAMETER_NAME","stayCALM_fraction","stayCALM_units"),
                     all.x = TRUE)


#now clean up the columns by pasting the old parameter names to any that are blank (that means they aren't part of the stayCALM package)

all.chemistry<-all.chemistry %>% 
  mutate(stayCALM_parameter=case_when(stayCALM_parameter==""~paste(parameter),
                                      is.na(stayCALM_parameter)~paste(parameter),
                                      TRUE~paste(stayCALM_parameter)))

#make them all lowercase
all.chemistry$stayCALM_parameter<-tolower(all.chemistry$stayCALM_parameter)

#subset to keep the columns we need
all.chemistry<-all.chemistry %>% 
  select(!c(parameter)) %>% 
  rename(parameter=stayCALM_parameter)


#need to make new sample_ID columns since some are formated differently
all.chemistry$date<-as.Date(all.chemistry$date,"%m/%d/%Y")
all.chemistry$date_text<-format(all.chemistry$date,"%Y%m%d")

all.chemistry$sample_id<-paste(all.chemistry$site_id,all.chemistry$date_text,sep="_")
all.chemistry$date_text<-NULL

raw_df<-all.chemistry
raw_df$replicate<-""
raw_df$depth<-as.numeric(raw_df$depth)
raw_df$info_type<-as.character(raw_df$info_type)

#for data package
raw_df_export<-raw_df
#take out phosphorus bc it doesn't apply to us
raw_df<-raw_df %>% 
  filter(parameter!="phosphorus") %>% 
  mutate(result_type="not-provided") %>% 
  mutate(method_speciation=case_when(parameter=="ammonia"~"as N",
                                     parameter=="nitrate"~"as N",
                                     parameter=="nitrite"~"as N",
                                     parameter=="nitrate_nitrite"~"as N"))


# Extract the package root with base R functions.
# This directory will provide relative paths between machines.
root.dir <- gsub("(stayCALM)(.*$)", "\\1", getwd())

library(stayCALM)
# This argument is supplied to the Rmarkdown code chunk options.
export.logical <- TRUE

wqs <- stayCALM::nysdec_wqs %>%
  dplyr::filter(
    !(
      parameter %in% "phosphorus" & type %in% c("aquatic_chronic",
                                                "health_water-source")
    ),
    !grepl("bap", parameter),
    !parameter %in% "chlorophyll_a"
  )

#trying out the new script
wqs_violations <- stayCALM::wqs_violations(
  .data = raw_df,
  .period = stayCALM::from_today("10 years"),
  .targeted_assessment = TRUE,
  .wipwl_df = stayCALM::wipwl_df,
  .wqs_df = wqs,
  .tmdl_df = stayCALM::tmdl_df
)


temp5<-wqs_violations[["violation_data"]]


temp5$year<-format(temp5$date,"%Y")
temp5<-temp5 %>% 
  group_by(seg_id,year,parameter,units) %>% 
  mutate(exceed=case_when(
    attaining_wqs=="no"~1,
    TRUE~0
  )) %>% 
  summarise(Violations=sum(exceed)) %>%
  filter(Violations!=0) %>% 
  rename(PWL_segment=seg_id,
         Year=year,
         Parameter=parameter)

temp6<-wqs_violations[["violation_data"]]%>%
  distinct() %>% 
  group_by(site_id, parameter, fraction,units, date,result)%>%
    mutate(exceed=case_when(
    attaining_wqs=="no"~1,
    TRUE~0
  )) %>% 
  summarise(Violations=sum(exceed))

