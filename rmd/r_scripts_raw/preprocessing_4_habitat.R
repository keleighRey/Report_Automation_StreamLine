#Preprocessing 4 
# this script will adjust the physical characteristics of the study sites (habitat and user perception)

#needs functions and preprocessing 1 to begin
#source("rmd/r_scripts_raw/preprocessing_1.R")
#source("rmd/r_scripts_raw/functions.R")


#select relevant columns
habitat.short<-habitat.short %>% 
  select(HFDH_EVENT_SMAS_HISTORY_ID,HFDH_EVENT_SMAS_SAMPLE_DATE
         ,HFDH_GRADIENT,HFDH_EPIFAUNAL_COVER,HFDH_EMBEDDEDNESS_POOLING,
         HFDH_VELOCITY_DEPTH_REGIME,
        HFDH_SEDIMENT_DEPOSITION,HFDH_FLOW_STATUS,
        HFDH_CHANNEL_ALTERATION, HFDH_RIFFLE_BEND_FREQUENCY,
        HFDH_LEFT_BANK_STABILITY,HFDH_RIGHT_BANK_STABILITY,
        HFDH_LEFT_BANK_VEG,HFDH_RIGHT_BANK_VEG,
        HFDH_LEFT_BANK_VEG_ZONE,HFDH_RIGHT_BANK_VEG_ZONE
        ) 
#create year column


habitat.short$HFDH_EVENT_SMAS_SAMPLE_DATE<-as.Date(habitat.short$HFDH_EVENT_SMAS_SAMPLE_DATE,"%m/%d/%Y")

habitat.short<-habitat.short %>% 
  mutate(year=format(HFDH_EVENT_SMAS_SAMPLE_DATE,"%Y"))

#limit to time period specified

habitat.short<-habitat.short %>% 
  filter(year >= year.input)

habitat.short<-merge(habitat.short,sites,by.y="SITE_HISTORY_ID",by.x="HFDH_EVENT_SMAS_HISTORY_ID")

habitat.short<-habitat.short %>% 
  arrange(order)

habitat.short$SITE_PWL_ID<-factor(habitat.short$SITE_PWL_ID, ordered = TRUE)

#average all aspects by site
habitat.df<-habitat.short%>%
  group_by(HFDH_EVENT_SMAS_HISTORY_ID, HFDH_GRADIENT)%>%
  summarize(`Epi.\n Cover`=mean(HFDH_EPIFAUNAL_COVER),
            `Embed. \n Pool.`=mean(HFDH_EMBEDDEDNESS_POOLING),
            `Vel/Dep. \n Reg.`=mean(HFDH_VELOCITY_DEPTH_REGIME),
            `Sed. \n Dep.`=mean(HFDH_SEDIMENT_DEPOSITION),
            `Flow \n Status`=mean(HFDH_FLOW_STATUS),
            `Chan. \n Alt`=mean(HFDH_CHANNEL_ALTERATION),
            `Rif. \n Freq`=mean(HFDH_RIFFLE_BEND_FREQUENCY),
            `L.B. \n Stability`=mean(HFDH_LEFT_BANK_STABILITY),
            `R.B. \n Stability`=mean(HFDH_RIGHT_BANK_STABILITY),
            `L.B. \n Veg`=mean(HFDH_LEFT_BANK_VEG),
            `R.B.\n Veg`=mean(HFDH_RIGHT_BANK_VEG),
            `L.B. \n Veg Zone`=mean(HFDH_LEFT_BANK_VEG_ZONE),
            `R.B. \n Veg Zone`=mean(HFDH_RIGHT_BANK_VEG_ZONE))

habitat.df[habitat.df=="-9999"]<-NA



#################################################################################################
# Add HMA model values to the table, add columns to see whether the model or stream score is lower, calculate HMA, add column to interpret HMA score
habit.hma<-habitat.df%>%
  mutate(EpiCoverM=case_when(
    HFDH_GRADIENT=="High" ~ 17,
    TRUE ~ 14),
    EmbedM=case_when(
      HFDH_GRADIENT=="High" ~ 17,
      TRUE ~ 13),
    VelDepM=case_when(
      HFDH_GRADIENT=="High" ~ 19,
      TRUE~10),
    SedDepM=case_when(
      HFDH_GRADIENT=="High" ~ 18,
      TRUE ~ 14),
    FlowM=case_when(
      HFDH_GRADIENT=="High" ~19,
      TRUE ~17),
    ChanM=case_when(
      HFDH_GRADIENT=="High"~ 18,
      TRUE ~ 17),
    RiffleM=case_when(
      HFDH_GRADIENT=="High"~ 19,
      TRUE~ 14),
    StabilityM=18,
    VegM=case_when(
      HFDH_GRADIENT=="High" ~ 18,
      TRUE ~ 17),
    VegZoneM=case_when(
      HFDH_GRADIENT=="High" ~ 18,
      TRUE ~ 15))

habit.hma<-habit.hma%>%
  mutate(EpiCover.Low=case_when(
    `Epi.
 Cover`<=EpiCoverM~`Epi.
 Cover`,
 TRUE~EpiCoverM),
    Embed.Low=case_when(
      `Embed. 
 Pool.`<=EmbedM ~ `Embed. 
 Pool.`,
      TRUE~EmbedM),
    VelDep.Low=case_when(
      `Vel/Dep. 
 Reg.`<=VelDepM ~ `Vel/Dep. 
 Reg.`,
      TRUE~ VelDepM),
    SedDep.Low=case_when(
      `Sed. 
 Dep.`<=SedDepM ~ `Sed. 
 Dep.`,
      TRUE~ SedDepM),
    Flow.Low=case_when(
      `Flow 
 Status`<=FlowM ~ `Flow 
 Status`,
      TRUE ~ FlowM),
    Chan.Low=case_when(
      `Chan. 
 Alt`<=ChanM ~ `Chan. 
 Alt`,
      TRUE~ ChanM),
    Riffle.Low=case_when(
      `Rif. 
 Freq`<=RiffleM ~ `Rif. 
 Freq`,
      TRUE ~ RiffleM),
    Stability.Low=case_when(
      (`L.B. \n Stability`+ `R.B. \n Stability`)<=StabilityM ~ (`L.B. \n Stability`+ `R.B. \n Stability`),
      TRUE~ StabilityM),
    Veg.Low=case_when(
      (`L.B. \n Veg`+`R.B.\n Veg`)<=VegM ~ (`L.B. \n Veg`+`R.B.\n Veg`),
      TRUE ~ VegM),
    Veg.Zone.Low=case_when(
      (`L.B. \n Veg Zone`+ `R.B. \n Veg Zone`)<= VegZoneM ~ (`L.B. \n Veg Zone`+ `R.B. \n Veg Zone`),
      TRUE~VegZoneM),
    `HMA \n Score`= case_when(
      HFDH_GRADIENT=="High" ~(EpiCover.Low+ Embed.Low+ VelDep.Low+ SedDep.Low+ Flow.Low+ Chan.Low+ Riffle.Low+ Stability.Low+Veg.Low+ Veg.Zone.Low)/181*100,
      TRUE ~ (EpiCover.Low+ Embed.Low+ VelDep.Low+ SedDep.Low+ Flow.Low+ Chan.Low+ Riffle.Low+ Stability.Low+Veg.Low+ Veg.Zone.Low)/149*100),
    `HMA \n Assess.`=case_when(
      `HMA \n Score`>=80 ~ "Natural",
      `HMA \n Score`>=70&`HMA \n Score`<80 ~ "Altered",
      `HMA \n Score`>=60 & `HMA \n Score` <70 ~ "Moderate",
      TRUE~ "Severe"
    ))

#rename columns
habit.hma<-habit.hma %>% 
  rename(Site=HFDH_EVENT_SMAS_HISTORY_ID,
         Gradient=HFDH_GRADIENT)
habit.hma<-habit.hma %>% 
  mutate_if(is.numeric,round, 1)

#add PWL to the column
habit.hma<-merge(habit.hma,pwl, by.x="Site",by.y="SITE_HISTORY_ID")
habit.hma<-habit.hma %>% 
  rename(PWL=SITE_PWL_ID)

HMA_counts<-habit.hma%>%
  rename(Assessment="HMA \n Assess.")
