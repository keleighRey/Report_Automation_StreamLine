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

### 3. Knit the report.

