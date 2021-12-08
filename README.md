# Report_Automation_StreamLine
This is a streamlined version of the Report Automation script, that connects via a synced drive to the Teams folder and SMAS data. This was developed with Andrea Conine and Zach Smith, and will be update to connect to the larger DB when that is available.

## To run the script, make a copy of the parent_12_7_2021.RMD, and name it with your project.

### 1.Copy your data into the "data" folder.  

There are 2 files you need to create, one is the sites file, and one is the intro file.

 -Use the template for your sites file located in teh "template" folder. 
 -You will assign group and order here. This will make your figures and tables flow in the order you decide.  
 -You will also need to create an intro.csv file. this is where you can input initial or custom narrative block at the beginning of the report. This csv must have one column named "into" with your intro below it in one cell.  
 -Finally, you will need a .png of your map made in GIS for the report. Plop that file into the data/map folder.   
### 2. Input the other parameters into the params on the YAML or using the "knit with parameters" shiny.

**Param list:**
params:  
    **tox: FALSE**  #this is either TRUE or FALSE, will run the tox section of hte report if TRUE.  
    **study_name: "Halfway Creek"** #your desired study name. This is continued throughout the text, so keep in mind how it will sound in a sentence.  
    **year_input: 2017** #the year that you want the scripts to start grabbing data from the datamod tables from.  
    **intro_file_name: "halfway_intro.csv"** #the name of the file you created your intro in (.csv)  
    **sites_file_name: "halfway_sites.csv"** #name of the sites file, see the template file in the templates folder. (.csv)  
    **grouping_for_chem: site** #this changes the axis of the chemistry plots, options are : "site"or "PWL"  
    **grouping_for_bap: site** #same as above, options are "site","PWL" or "both". both will create 2 figures  
    map_zoom: 12 #discontinued, will not change anything  
    **user: karey** #your username, you will need this to access the datamod tables  
    **limit_to_nuts: TRUE** #if TRUE, this will limit the chemistry figures to just the nutrient suite and the in-situ figures.  
    **data_package: FALSE**  #if TRUE this will create a .xlsx file in the outputs folder that has raw chemistry and a summary of excursions by PWL.  
    **lit_cited_file: "lit_cited.docx"** #this is an editable file for lit_cited, if you want to change it I would rename and reflect that name here.  
    **map_file: "birch_map.png"** # the .png of a map you'd like included in the report.  
     

### 3. Knit the report.

