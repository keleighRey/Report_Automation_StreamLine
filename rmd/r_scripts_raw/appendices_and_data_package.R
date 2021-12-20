#APPENDIX IV QA/QC table and data package
#Keleigh Reynolds
#3/22/21

#this is to prep the rejected chemistry data for the qa/qc table
#options(scipen=99)
#have to run the first three

#source(here::here("rmd/r_scripts_raw","stayCALM_exceedances_updated.R"))
#source(here::here("rmd/r_scripts_raw","functions.R"))
#source(here::here("rmd/r_scripts_raw","preprocessing_1_raw.R"))
#source(here::here("rmd/r_scripts_raw","preprocessing_2_chemistry.R"))


rej<-qa.table %>% 
  filter(CHR_VALIDATOR_QUAL=="R") %>% 
  select(CHS_EVENT_SMAS_HISTORY_ID,CHS_EVENT_SMAS_SAMPLE_DATE,
         CHEM_PARAMETER_NAME,CHEM_PARAMETER_UNIT,CHEM_PARAMETER_FRACTION,CHR_RESULT_VALUE,
         CHR_VALIDATION_DATE,CHR_VALIDATOR_QUAL,CHR_VALIDATOR_QUAL_EXPLN) %>% 
  rename(Site=CHS_EVENT_SMAS_HISTORY_ID,
         Date=CHS_EVENT_SMAS_SAMPLE_DATE,
         Parameter=CHEM_PARAMETER_NAME,
         Units=CHEM_PARAMETER_UNIT,
         Fraction=CHEM_PARAMETER_FRACTION,
         Result=CHR_RESULT_VALUE,
         Validated=CHR_VALIDATION_DATE,
         Validator=CHR_VALIDATOR_QUAL,
         Explanation=CHR_VALIDATOR_QUAL_EXPLN) %>% 
  mutate(Parameter=stringr::str_to_title(Parameter)) %>% 
  mutate(Parameter=case_when(Parameter=="Ph"~"pH",
                             TRUE~paste(Parameter))) %>% 
  mutate(Fraction=tolower(Fraction)) %>% 
  mutate(Date=as.Date(Date,"%m/%d/%Y"),Validated=as.Date(Validated,"%m/%d/%Y"))
############################################################################################
#data package

#use new files from the stayCALM package, updated 10/4/2021
chem_export<-raw_df_export %>% 
  select(seg_id,site_id,date,parameter,value,units,fraction,validator_qualifiers,
         interpreted_qualifiers)

#need class for the rest-use the sites table for this instead,
# bc the prepped data only looks at the ones that have standards
class<-sites.short %>%  
  select(SITE_PWL_ID,SITE_WATER_QLTY_STANDARD) %>% 
  rename(seg_id=SITE_PWL_ID,class=SITE_WATER_QLTY_STANDARD) %>% 
  distinct()

#merge the raw data by class to get something else to merge it with
chem_export<-merge(chem_export,class,by="seg_id")
chem_export<-chem_export %>% 
  distinct()
#merge with the "not found" params so we can rbind the new (changed to correct units) raw chemistr
#not_prepped<-unique(wqs_violations$param_not_found)

#chem_export_1<-merge(chem_export,not_prepped,by=c("parameter","fraction","units"))
#now we have the data that didn't go through stayCALM here, have to merge it with 
#the prepped data to get the right stuff

#prepped<-wqs_violations$prep_data %>% 
 # select("seg_id","site_id","date","parameter","value","units", "fraction",
  #       "validator_qualifiers","interpreted_qualifiers","class","water_type", "narrative")

#chem_export_1$narrative<-""
#now bind these together!
#chem_export<-rbind(chem_export_1,prepped)
#
#have to change ammonia stuff to get it to merge here, since units are converted for the
#stayCALM package
chem_export<-chem_export %>% 
  mutate(value=case_when(parameter=="ammonia"~value*1000,
                          TRUE~value)) %>% 
  mutate(units=case_when(parameter=="ammonia"~paste("ug/l"),
                         TRUE~paste(units))) %>% 
  mutate(result=round(value,digits = 1))#this is to match the value on the stayCALM values
#they round to 1 sig digit

viol_small<-wqs_violations$violation_data %>% 
  select(seg_id,site_id,date,parameter, result,fraction, 
         units,use,statistic,threshold,attaining_wqs,sample_count) %>% 
  tidyr::separate_rows(site_id,sep=";")

viol_small<-viol_small %>% 
  mutate(units=case_when(parameter=="ph"~paste("ph units"),
                         TRUE~paste(units)))

chem_export<-merge(chem_export,viol_small,by=c("seg_id","site_id","date","parameter",
                                               "fraction","units","result"),all.x=TRUE)
#fix the pH repeats, get them alone, get the range of values and then bind it back
chem_export.ph<-chem_export %>% 
  filter(parameter=="ph") %>% 
  arrange(threshold) %>% 
  mutate(threshold=paste(unique(threshold), collapse = "-")) %>% 
  mutate(threshold=gsub("-NA","",threshold)) %>% 
  distinct()

chem_export<-chem_export %>% 
  filter(parameter!="ph")

chem_export<-rbind(chem_export,chem_export.ph)

chem_export<-chem_export %>% 
  distinct()

#need narrative for the threshold
narrative<-wqs_violations$prep_data %>%
  select("site_id","date","parameter","units", "fraction","narrative")
  
narrative<-narrative %>% 
  mutate(units=case_when(units=="ph_units"~paste("ph units"),
                         TRUE~paste(units))) %>% 
  distinct()

chem_export<-merge(chem_export,narrative, 
                   by=c("site_id","date","parameter","units", "fraction"),
                   all.x=TRUE)
chem_export<-chem_export %>% 
  distinct()
#now make the "no violation" language

chem_export<-chem_export %>% 
  mutate(Excursion=case_when(attaining_wqs=="yes"~"No Excursion",
                             attaining_wqs=="no"~"Excursion",
                             TRUE~"No applicable standard")) %>% 
  select(!c(attaining_wqs,sample_count,statistic))

#get rid of the weird flag stuff
chem_export<-chem_export %>% 
  mutate(validator_qualifiers=case_when(validator_qualifiers=="n_a"~"",
                                        validator_qualifiers=="N/A"~"",
                                        TRUE~paste(toupper(validator_qualifiers)))) %>% 
  mutate(interpreted_qualifiers=case_when(interpreted_qualifiers=="N/A"~"",
                                          TRUE~paste(interpreted_qualifiers)))

#get the unit converstion
unit_conv<-pcode %>% 
  select(CHEM_PARAMETER_UNIT,CHEM_PARAMETER_UNIT_NOSP) %>% 
  distinct() %>%
  mutate(CHEM_PARAMETER_UNIT_NOSP=tolower(CHEM_PARAMETER_UNIT_NOSP))

chem_export<-merge(chem_export,unit_conv,by.x="units",by.y="CHEM_PARAMETER_UNIT_NOSP")
chem_export<-chem_export %>% 
  distinct()

#clean up the units columns
chem_export$units<-NULL
chem_export<-chem_export %>% 
  rename(units=CHEM_PARAMETER_UNIT) %>% 
  relocate(units,.before="fraction") %>% 
  relocate(seg_id,.before="site_id")

#make it pretty
chem_export<-chem_export %>% 
  rename('PWL segment'=seg_id,
         Site="site_id",
         Date="date",
         Result="value",
         Fraction="fraction",
         Units="units",
         "Data Flags"= validator_qualifiers,
         "Data Flags-Meaning"=interpreted_qualifiers,
         Class="class",
         Parameter=parameter,
         Use=use,
         Narrative=narrative,
         Threshold=threshold)
chem_export$Result<-as.character(chem_export$Result)#removes the trailing 0's to get the
#original sig figs

chem_export$Class<-toupper(chem_export$Class)

chem_export$Parameter<-stringr::str_to_title(chem_export$Parameter)
chem_export$Use<-stringr::str_to_title(chem_export$Use)


chem_export<-chem_export %>% 
  mutate(Parameter=case_when(Parameter=="Ph"~"ph",
                             Parameter=="Nitrate_nitrite"~"Nitrite (as N)",
                             Parameter=="Dissolved_oxygen"~"Dissolved Oxygen",
                             TRUE~paste(Parameter)))

#violation summary###################################
violations<-wqs_violations[["exceedance_summary"]]
violations<-merge(violations,class,by="seg_id")

violations<-violations %>% 
  rename("PWL segment"=seg_id,
         Class=class,
         Use=use)
  
violations<-violations %>% 
  group_by(`PWL segment`,Class,Use) %>% 
  summarise(Excursions=sum(n_exceedances))

#make it pretty
violations<-violations %>% 
  mutate(Class=toupper(Class),Use=stringr::str_to_title(Use))

#get the meta_data
meta<-read.csv(here::here("data/col_names/meta_data_package_names.csv"))

# Create a blank workbook
library(openxlsx)
OUT <- createWorkbook()

# Add some sheets to the workbook

addWorksheet(OUT,"meta_data")
addWorksheet(OUT,"Chemistry_Results_Raw")
addWorksheet(OUT,"Excursion_Summary")

# Write the data to the sheets

writeData(OUT,sheet="meta_data",x=meta)
writeData(OUT,sheet="Chemistry_Results_Raw",x=chem_export)
writeData(OUT,sheet = "Excursion_Summary", x=violations)

# Reorder worksheets
#worksheetOrder(OUT) <- c(2,1,)

data_package_name=sites_file_name

data_package_name<-gsub(".csv","",data_package_name)

if(params$data_package){

# Export the file
file.name=paste("outputs/",data_package_name,"_attachment_I.xlsx",sep = "")

saveWorkbook(OUT, file.name)
}
  
