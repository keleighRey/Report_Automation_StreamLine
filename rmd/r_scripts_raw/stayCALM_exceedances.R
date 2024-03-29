#stayCALM 
#updated 2/23/2021 for using regular chemistry

#run the preprocessing 1 file that limits chemistry and insitu data to what we want
source(here::here("rmd","r_scripts_raw/functions.R"))
source(here::here("rmd","r_scripts_raw/preprocessing_1_raw.R"))

#this one does a lot so we can clean up what we don't need
rm(analyte,habitat.short,metrics.short,
   sbu.chem.statewide,sbu.insitu.statewide,tox.sed.short,tox.wat.short,userp.short)

#first read in the pwl to get the segment id in both files
pwl<-sites.short %>% 
  select(SITE_HISTORY_ID,SITE_PWL_ID)
#add the FL ones
ext.pwl<-ext %>% 
  select()

#merge with chem and insitu to get the pwl id into those files
chem.short<-merge(chem.short,pwl, by.x="CHS_EVENT_SMAS_HISTORY_ID",by.y="SITE_HISTORY_ID")


#make the results have 1/2 the MDL for analysis
chem.short<-chem.short %>% 
  mutate(CHR_RESULT_VALUE=case_when(CHR_VALIDATOR_QUAL=="U"~as.numeric(CHR_METHOD_DETECT_LIMIT*0.5),
         TRUE~as.numeric(CHR_RESULT_VALUE)))

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

# Extract the package root with base R functions.
# This directory will provide relative paths between machines.
library(stayCALM)
root.dir <- gsub("(stayCALM)(.*$)", "\\1", getwd())

wqs.df <- stayCALM::nysdec_wqs
data("wipwl.df")

wqs_wipwl.df <- wipwl.df %>%
  # Subset columns
  select(seg_id, class, spatial_extent, water_type) %>% 
  # Ensure all rows are unique representations of the data.
  distinct() %>%
  # Join the DF with the WQS by the specified columns.
  left_join(wqs.df,
            by = c("class",
                   "spatial_extent",
                   "water_type"))

chem_extract.df <- all.chemistry %>% 
  filter(parameter %in% c("hardness", "ph", "temperature")) %>% 
  tidyr::pivot_wider(
    id_cols = sample_id,
    names_from = "parameter",
    values_from = c("value", "units"),
    names_sep = "_",
    values_fn = list(value = mean,
                     units = unique,
                     na.rm = TRUE)
  ) %>% 
  right_join(all.chemistry, by = "sample_id") %>% 
  distinct()

#fix the units/results
wqs.short<-wqs_wipwl.df %>% 
  select(seg_id,parameter,fraction,units) %>% 
  rename(units_threshold=units)

chem_extract.df<-merge(chem_extract.df,wqs.short,by=c("seg_id","parameter","fraction"),all.x = TRUE)

chem_extract.df<-chem_extract.df %>% 
  distinct()

chem_extract.df$value<-as.numeric(chem_extract.df$value)
#fix units that don't match

#take out any blanks-not sure we need to do this?
chem_extract.df<-chem_extract.df %>% 
  filter(value!="")

chem_extract.df$value<-as.numeric(chem_extract.df$value)

#correct them based on the units in the stds, and convert corrected value column to numeric
chem_extract.df$corrected_value<-ifelse(chem_extract.df$units=="mg/l" & chem_extract.df$units_threshold=="ug/l",
                                        chem_extract.df$value*1000,paste(chem_extract.df$value))
chem_extract.df$corrected_value<-ifelse(is.na(chem_extract.df$corrected_value),
                                        chem_extract.df$value,chem_extract.df$corrected_value)
chem_extract.df$corrected_value<-as.numeric(chem_extract.df$corrected_value)

chem_extract.df_2<-chem_extract.df %>% 
  rename(units_parameter=units,
         units=units_threshold)

#have to have it merge by units_threshold to get the match
chem_export <- merge(x = chem_extract.df_2, 
                 y = wqs_wipwl.df,
                 by = c("seg_id", "parameter",
                        "fraction","units"),
                 all.x = TRUE)
chem_export_thresh<-thresh_determination(chem_export)

#chem_export$units<-NULL

chem_extract.df$value<-NULL
chem_extract.df$units<-NULL
chem_extract.df<-chem_extract.df %>% 
  rename(value=corrected_value,
         units=units_threshold)

chem.df <- merge(x = chem_extract.df, 
                 y = wqs_wipwl.df,
                 by = c("seg_id", "parameter",
                        "fraction", "units"))

chem.df <- thresh_determination(chem.df)

chem.df$date<-as.Date(chem.df$date,"%m/%d/%Y")
chem.df$year<-format(chem.df$date,"%Y")
chem.df$month<-format(chem.df$date,"%m")

chem.df$assessment_id <- group_id(.data = chem.df,
                                  .keep = c("seg_id",
                                            "parameter",
                                            "fraction"),
                                  .numeric = TRUE)

chem.df$within_period <- assessment_period(.date_vec = chem.df$date,
                                           .n_years_ago = 10)

prepped.df <- prep_values(.data = chem.df,
                          .block_col = "block",
                          .value_col = "value",
                          .statistic_col = "statistic",
                          .new_value_col = "result",
                          .min_n_col = "min_n")

selected.df <-
  subset(
    prepped.df,
    select = c(
      "assessment_id",
      "seg_id",
      "site_id",
      "water_type",
      "type",
      "use",
      "standard_type",
      "group",
      "block",
      "statistic",
      "parameter",
      "fraction",
      "units",
      "result",
      "date",
      "year",
      "within_period",
      "direction",
      "threshold",
      "summarize_rows",
      "summarize_rows_operator",
      "wqs_75p_threshold",
      "data_provider"
    )
  )

selected.df$attaining_wqs <- attaining(selected.df$result,
                                       selected.df$direction,
                                       selected.df$threshold)

selected.df$attaining_75 <- attaining(selected.df$result,
                                      selected.df$direction,
                                      selected.df$wqs_75p_threshold)

temp4<-selected.df%>%
  distinct() %>% 
  group_by(site_id, parameter, fraction, date)%>%
  summarize(n=n(),
            combined=sum(attaining_wqs),
            WQS_attain_combined= ifelse(combined==n, yes=TRUE, no=FALSE),
            num_standards=(n-combined))

temp5<-selected.df %>% 
  distinct() %>% 
  group_by(seg_id,parameter,fraction,units,date) %>% 
  summarize(n=n(),
            combined=sum(attaining_wqs),
            WQS_attain_combined= ifelse(combined==n, yes=TRUE, no=FALSE),
            num_standards=(n-combined))

temp5$year<-format(temp5$date,"%Y")
temp5<-temp5 %>% 
  group_by(seg_id,year,parameter) %>% 
  mutate(exceed=case_when(
    WQS_attain_combined=="FALSE"~1,
    TRUE~0
  )) %>% 
  summarise(Violations=sum(exceed)) %>%
  filter(Violations!=0) %>% 
  rename(PWL_segment=seg_id,
         Year=year,
         Parameter=parameter)

temp6<-selected.df%>%
  distinct() %>% 
  group_by(site_id, parameter, fraction, date,result)%>%
  summarize(n=n(),
            combined=sum(attaining_wqs),
            WQS_attain_combined= ifelse(combined==n, yes=TRUE, no=FALSE),
            num_standards=(n-combined))

#write.csv(temp5,"outputs/fl_exceedances.csv",row.names = FALSE)
#From here you should be able to left join pwls or raw data back on or filter to do summaries to create tables etc. 

#clean up
#rm(list=ls()[! ls() %in% c("temp4","selected.df","wqs_wipwl.df","temp5")])

