#APPENDIX IV QA/QC table and data package
#Keleigh Reynolds
#3/22/21

#this is to prep the rejected chemistry data for the qa/qc table
options(scipen=99)


rej<-qa.table %>% 
  filter(CHR_VALIDATOR_QUAL!="A") %>% 
  select(CHS_EVENT_SMAS_HISTORY_ID,CHS_EVENT_SMAS_SAMPLE_DATE,
         CHEM_PARAMETER_NAME,CHEM_PARAMETER_UNIT,CHEM_PARAMETER_FRACTION,CHR_RESULT_VALUE,
         CHR_VALIDATION_DATE,CHR_VALIDATOR_QUAL,CHR_VALIDATOR_QUAL_EXPLN) %>% 
  rename(Site=CHS_EVENT_SMAS_HISTORY_ID,
         Date=CHS_EVENT_SMAS_SAMPLE_DATE,
         Paramter=CHEM_PARAMETER_NAME,
         Units=CHEM_PARAMETER_UNIT,
         Fraction=CHEM_PARAMETER_FRACTION,
         Result=CHR_RESULT_VALUE,
         Validated=CHR_VALIDATION_DATE,
         Validator=CHR_VALIDATOR_QUAL,
         Explanation=CHR_VALIDATOR_QUAL_EXPLN)
############################################################################################
#data package

chem.export<-chem_export %>% 
  select(!c(info_type,data_provider,depth,value_hardness,value_ph,value_temperature,
            units_hardness,units_ph,units_temperature)) %>% 
  mutate_if(is.numeric, round,digits=2)


#Combine with Violations, use the temp4 from the stayCALM package

viol.export<-selected.df %>% 
  select(seg_id,site_id,date,parameter,result,units,fraction,year,attaining_wqs,) %>% 
  rename(value=result)

chem.export<-merge(chem.export,viol.export,by=c("seg_id","site_id","date","parameter",
                                                "fraction","value","units"),all.x = TRUE) %>% 
  distinct()

#need class for the rest
class<-wqs_wipwl.df %>% 
  select(seg_id,class,water_type) %>% 
  distinct()

chem.export<-chem.export %>% 
  select(!c(sample_id,class,water_type,summarize_rows,summarize_rows_operator,wqs_75p_threshold,
            wqs_75p_threshold_formula))

chem.export<-merge(chem.export,class,by="seg_id",all.x = TRUE)

chem.export<-chem.export %>% 
  rename( "PWL segment"=seg_id,
          Site=site_id,
          Date=date,
          Parameter=parameter,
          Fraction=fraction,
          Result=value,                
          Units=units,
          "Data Flags-Meaning"=interpreted_qualifiers,
          "Method Quantitation Limit"=quantitation_limit,
          "Data Flags"=validator_qualifiers,
          "Threshold Units"=threshold_units,
          "Standard Type"=type,
          Use=use,
          "WQ Standard"=narrative,
          Year=year,
          Violation=attaining_wqs,        
          "Water Class"=class,
          "Water Type"=water_type)

chem.export<-chem.export %>% 
  mutate(Violation=case_when(Violation=="TRUE"~"No Violation",
                             Violation=="FALSE"~"Violation"))


#violation summary
violations<-chem.export %>% 
  filter(Violation=="Violation")
  

# Create a blank workbook
library(openxlsx)
OUT <- createWorkbook()

# Add some sheets to the workbook

addWorksheet(OUT,"Chemistry_Results_Raw")
addWorksheet(OUT,"Violation_Summary")

# Write the data to the sheets

writeData(OUT,sheet="Chemistry_Results_Raw",x=chem.export)
writeData(OUT,sheet = "Violation_Summary", x=violations)

# Reorder worksheets
#worksheetOrder(OUT) <- c(2,1,)

data_package_name=sites_file_name

data_package_name<-gsub(".csv","",data_package_name)

# Export the file
file.name=paste("outputs/",data_package_name,"_data_package.xlsx",sep = "")

saveWorkbook(OUT, file.name)
  
