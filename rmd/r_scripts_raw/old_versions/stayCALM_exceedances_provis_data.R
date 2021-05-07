#stayCALM 

# Load a collection of tidyverse packages into the environment.
library(tidyverse)
# Load the stayCALM package into the environment.
library(stayCALM)

#year
year.input=2019

#load teh data needed
sites<-read.csv(file.path(here::here(),
                      "data","sites_to_assign_order.csv"),stringsAsFactors = FALSE)
raw.chem<-read.csv(file.path(here::here(),
                             "data",
                             "sbu_chem_2001_2019_provis.csv"))

sites.l<-unique(sites$SITE_HISTORY_ID)
raw<-raw.chem %>% 
  subset(site_id %in% sites.l) %>% 
  filter(validator_qualifiers!="R")

raw$sample_date<-as.Date(raw$sample_date,"%m/%d/%Y")
raw$year<-format(raw$sample_date,"%Y")

raw<-raw %>% 
  filter(year>=year.input)

#insitu data
SBU.in.situ.chem<-read.csv(file.path(here::here(),
                                     "data",
                                     "20201030_S_IN_SITU_WATER_CHEM_all_fields.csv"),stringsAsFactors=FALSE)
insitu<-SBU.in.situ.chem %>% 
  subset(ISWC_EVENT_SMAS_HISTORY_ID %in% sites.l)

insitu$ISWC_EVENT_SMAS_SAMPLE_DATE<-as.Date(insitu$ISWC_EVENT_SMAS_SAMPLE_DATE,"%m/%d/%Y")
insitu$year<-format(insitu$ISWC_EVENT_SMAS_SAMPLE_DATE,"%Y")

insitu<-insitu %>% 
  filter(year>=year.input)

#clean up
rm(raw.chem,SBU.in.situ.chem)

# Extract the package root with base R functions.
# This directory will provide relative paths between machines.
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

# These data sets have been added to the stayCALM package as .Rda files.
# This data will eventually be housed in an authoritative database.
data(smas.df) 
#data(lmas.df)

# Append the SMAS and LMAS data by row.
org.df <- rbind(smas.df, lmas.df)

# # Keep only the dates within the 10-year assessment period.
# # StayCALM will flag these dates as outside of the assessment period, but
# # this can inflate the number of un-assessed waters reported. It is easier,
# # to filter out the older data here if it is not of interest.
# day_zero <- stayCALM::date_subtraction(.date = Sys.Date(),
#                                        .subtract = "10 years")
# org.df <- org.df[org.df$date >= day_zero, ]

#limit to just the data set we want for wallkill
cols.keep<-names(org.df)
#remove org.df
rm(org.df)

#need to have the insitu data in there too and rename columns to match the 

raw.1<-raw%>% 
  rename(date=sample_date,
         parameter=chemical_name,
         value=result_value,
         units=result_unit)

#create good sample id (since the sys sample code has -W on them)
raw.1$sample_id<-paste(raw.1$site_id,format(raw.1$date,"%Y%m%d"),sep = "_")

x1<-raw.1[,names(raw.1) %in% cols.keep] 
x1$depth<-"NA"
x1$info_type<-"NA"
x1$data_provider<-"NA"

#need seg_ID
pwl<-sites %>% 
  select(SITE_PWL_ID,SITE_HISTORY_ID) %>% 
  rename(seg_id=SITE_PWL_ID,
         site_id=SITE_HISTORY_ID)

x1<-merge(x1,pwl,by="site_id",all.x = TRUE)
x1$fraction<-tolower(x1$fraction)


insitu$PARAMETER_NAME<-tolower(insitu$CHEM_PARAMETER_NAME)

insitu$PARAMETER_NAME<-gsub(" ","_",insitu$PARAMETER_NAME)

insitu<-insitu %>% #need and data provider and quant limit, need pwl id, info_type
  rename(site_id=ISWC_EVENT_SMAS_HISTORY_ID,
         parameter=PARAMETER_NAME,
         date=ISWC_EVENT_SMAS_SAMPLE_DATE,
         value=ISWC_RESULT,
         units=CHEM_PARAMETER_UNIT,
         validator_qualifiers=ISWC_INSITU_QUAL,
         interpreted_qualifiers=ISWC_INSITU_QUAL,
         sample_id=EVENT_SMAS_ID,
         fraction=CHEM_PARAMETER_FRACTION
         
  ) %>% 
  mutate(fraction=case_when(
    parameter %in% "ph"~"total",
    parameter %in% "dissolved_oxygen"~"dissolved",
    TRUE~"total")) %>% 
  mutate(data_provider="NA",quantitation_limit="NA",info_type="NA")


x2<-insitu[,names(insitu) %in% cols.keep] 
#x2 needs PWL ID
#need other missing columns
x2$data_provider<-""
x2$depth<-""
x2$validator_qualifiers<-x2$interpreted_qualifiers

x2<-merge(x2,pwl,by="site_id")


x3<-rbind(x1,x2)

#fix the chemicals to match wqs file
x3$parameter<-tolower(x3$parameter)

x3<-x3 %>% 
  mutate(parameter=case_when(
    parameter %in% "nitrogen, ammonia (as n)"~ "ammonia",
    parameter %in% "chloride (as cl)"~"chloride",
    parameter %in% "total dissolved solids (residue, filterable)" ~ "total_dissolved_solids",
    parameter %in% "nitrogen, nitrate (as n)"  ~ "nitrate",
    parameter %in% "nitrogen, nitrite" ~"nitrite",
    parameter %in%  "nitrate+nitrite as nitrogen" ~"nitrate_nitrite",
    parameter %in% "sulfate (as so4)"  ~ "sulfate",
    parameter %in% "hardness (as caco3)"     ~ "hardness",
    parameter %in% "fecal coliform" ~ "fecal_coliforms",
    parameter %in% "coliform"~"total_coliform",
    parameter %in% "phosphorus, total (as p)" ~"phosphorus",
    TRUE ~ paste(parameter)
    
  )) %>% 
  mutate(units=case_when(
    parameter %in% "ph"~"ph_units",
    TRUE~paste(units)
  ))

org.df<-x3
org.df$units<-tolower(org.df$units)
org.df$date<-as.Date(org.df$date,"%m/%d/%Y")
org.df$year<-format(org.df$date,"%Y")

org.df<-org.df %>% 
  subset(year>=year.input)

chem_extract.df <- org.df %>% 
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
  right_join(org.df, by = "sample_id")

#fix the units/results
wqs.short<-wqs_wipwl.df %>% 
  select(seg_id,parameter,fraction,units) %>% 
  rename(units_threshold=units)

chem_extract.df<-merge(chem_extract.df,wqs.short,by=c("seg_id","parameter","fraction"),all.x = TRUE)

chem_extract.df$value<-as.numeric(chem_extract.df$value)
#fix units that don't match

#take out any blanks
chem_extract.df<-chem_extract.df %>% 
  filter(value!="")

chem_extract.df$value<-as.numeric(chem_extract.df$value)


#correct them based on the units in the stds, and convert corrected value column to numeric
chem_extract.df$corrected_value<-ifelse(chem_extract.df$units=="mg/l" & chem_extract.df$units_threshold=="ug/l",
                                        chem_extract.df$value*1000,paste(chem_extract.df$value))
chem_extract.df$corrected_value<-ifelse(is.na(chem_extract.df$corrected_value),
                                        chem_extract.df$value,chem_extract.df$corrected_value)
chem_extract.df$corrected_value<-as.numeric(chem_extract.df$corrected_value)

#have to have it merge by units_threshold to get the match
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
  group_by(site_id, parameter, fraction, date)%>%
  summarize(n=n(),
            combined=sum(attaining_wqs),
            WQS_attain_combined= ifelse(combined==n, yes=TRUE, no=FALSE),
            num_standards=(n-combined))

temp5<-selected.df %>% 
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
  summarise(Exceedances=sum(exceed)) %>%
  filter(Exceedances!=0) %>% 
  rename(PWL_segment=seg_id,
         Year=year,
         Parameter=parameter)

#write.csv(temp5,"outputs/fl_exceedances.csv",row.names = FALSE)
#From here you should be able to left join pwls or raw data back on or filter to do summaries to create tables etc. 

#clean up
rm(list=ls()[! ls() %in% c("temp4","selected.df","wqs_wipwl.df","temp5")])

