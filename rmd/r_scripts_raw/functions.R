#Functions
#KAR 11/5/2020

#filter df's to site desired (for multiple df's and easy typing)

filter.to.sites<-function(df,x){
  sites.l<-unique(sites$SH_SITE_ID)
  df.short<-df %>% 
    dplyr::filter(!!x %in% sites.l)}


############################################################################################
#BAP Plot
bap.graph<-function(df){
  ggplot(df, aes(x=MSSIH_EVENT_SMAS_HISTORY_ID, y=mean))+
    geom_point(aes(shape=SITE_PWL_ID, color=group), size=4)+
    #geom_point(aes(color=group), size=4)+
    scale_shape_manual(name="PWL Segment ID",values = 0:max(l.p))+
    scale_color_manual(breaks=names(group_colors),values=group_colors)+
    #scale_color_discrete(limits=group_colors)+
    geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci, color=group,),  width=.5)+
    theme_bw() + 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),#remove gridlines
      axis.title.y = element_text(
        size = 12,
        family = "serif",
        face = "bold"),
      axis.title.x = element_blank(),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 10,
        family = "serif"), #rotate text angle
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      legend.position = c(.11, .81) + #reduces white space around plot edges
        geom_label(aes(x = .5, y = .5), label = "test")
    )+
    geom_hline(yintercept=2.5,linetype="dashed",color="grey")+
    geom_hline(yintercept=5,linetype="dashed",color="grey")+
    geom_hline(yintercept=7.5,linetype="dashed",color="grey")+
    theme(legend.title=element_blank(),legend.margin=margin(10,10,10,10),legend.key = element_rect(colour = NA, fill = NA),legend.background=element_blank())+
    ylab("Biological Assessment Profile Score")+xlab("Year")+ geom_text(data = labels.df, aes(max(l)+1, y, label = label), color = "black",angle=90)+
    expand_limits(y=c(0,10),x=c(0:max(l+2)))
  
  
}
#BAP plot by PWL#########################################################################
#BAP Plot
bap.graph.pwl<-function(df){
  ggplot(df, aes(x=SITE_PWL_ID, y=mean))+
    geom_point(aes(color=group), size=4)+
    #geom_point(aes(color=group), size=4)+
    scale_color_manual(breaks=names(group_colors),values=group_colors)+
    geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci, color=group,),  width=.5)+
    theme_bw() + 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),#remove gridlines
      axis.title.y = element_text(
        size = 12,
        family = "serif",
        face = "bold"),
      axis.title.x = element_blank(),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 10,
        family = "serif"), #rotate text angle
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      legend.position = c(.11, .81) + #reduces white space around plot edges
        geom_label(aes(x = .5, y = .5), label = "test")
    )+
    geom_hline(yintercept=2.5,linetype="dashed",color="grey")+
    geom_hline(yintercept=5,linetype="dashed",color="grey")+
    geom_hline(yintercept=7.5,linetype="dashed",color="grey")+
    theme(legend.title=element_blank(),legend.margin=margin(10,10,10,10),legend.key = element_rect(colour = NA, fill = NA),legend.background=element_blank())+
    ylab("Biological Assessment Profile Score")+xlab("Year")+ geom_text(data = labels.df.pwl, aes(max(l.p)+1, y, label = label), color = "black",angle=90)+
    expand_limits(y=c(0,10),x=c(0:max(l.p+2)))
  
  
}
###############################################################################################
#function to make the sites a factor in order from upstream to downstream
order_sites<-function(df, sites_table, df_site_col, sites_site_col){
  Sites<-sites_table%>%
    dplyr::rename(SITE_ID=sites_site_col)
  
  df<-df%>%
    dplyr::rename(SITE_ID=df_site_col)
  
  all_data<-left_join(df, Sites)
  all_data<-all_data%>%
    dplyr::arrange(order)%>%
    dplyr::mutate(SITE_ID=factor(SITE_ID, levels = unique(SITE_ID)))
  
  columns<-colnames(Sites)
  columns<-columns[!(columns %in% "SITE_ID")]
  
  all_data<-all_data%>%
    dplyr::select(!tidyselect::any_of(columns))
}
###################################################################################
#create year column
yearly<-function(df,x){
  df$x<-as.Date.character(df$x,"%m/%d/%Y")
}

########################################################################################
#table
#df is the data frame, x and y are the column numbers you want centered, z is the caption
table.f<-function(df,x,y){
  library(flextable)
  tl<-flextable(df) %>% font(i = NULL, j = NULL, fontname="Arial", part = "all") %>% 
    theme_zebra()
  tl<-fontsize(tl,size = 8,part = "all")
  tl<-autofit(tl)
  tl<-set_table_properties(tl,layout="autofit")
  tl<-align(tl, i = NULL, j =(x:y) , align = "center", part = "all")
  tl
}
####################################################################################
#Chemistry grouped by site
#chemistry Graphs
# create graphing function for those that need to be log10 (ug/L or mg/l-so not weird ones)
chem.graph.site <- function(df, na.rm = TRUE, ...){
  df<-df %>% 
    arrange(CHEM_PARAMETER_NAME)
  
  df$CHEM_PARAMETER_NAME<-tolower(df$CHEM_PARAMETER_NAME)

  sbu.chem.statewide$CHEM_PARAMETER_NAME<-tolower(sbu.chem.statewide$CHEM_PARAMETER_NAME)
  if(nrow(stars_site)>1){stars_site$chemical_name<-tolower(stars_site$chemical_name)}
  # create list of chmistry's in data to loop over 

  chem_list <- unique(df$CHEM_PARAMETER_NAME)
  
  l<-length(chem_list)
  
  
  for (i in seq_along(chem_list)) { 
    #order by group
    df$order<-as.numeric(df$order)
    df<-df %>% 
      arrange(order)
    df$CHS_EVENT_SMAS_HISTORY_ID<-forcats::fct_reorder(df$CHS_EVENT_SMAS_HISTORY_ID,df$order)
    
    temp.statewide<-subset(sbu.chem.statewide,sbu.chem.statewide$CHEM_PARAMETER_NAME==chem_list[i])
    if(nrow(stars_site)>1){
      temp.stars<-subset(stars_site,stars_site$chemical_name==chem_list[i])
    }
    
    temp.chem<-subset(df,df$CHEM_PARAMETER_NAME==chem_list[i])
    df.1<-subset(df, df$CHEM_PARAMETER_NAME==chem_list[i])
    
    #mke high flow flags nicer vocab
    df.1<-df.1 %>% 
      mutate(high_flow_flag=case_when(high_flow_flag=="high"~ "High flow",
                                      TRUE~"Base flow"))
    
    #df.1$SITE_PWL_ID<-droplevels(df.1$SITE_PWL_ID)
    #y=tail(df.1$SITE_PWL_ID) #get the number of levels needed for each one
    
    df.1$CHS_EVENT_SMAS_HISTORY_ID<-droplevels(df.1$CHS_EVENT_SMAS_HISTORY_ID)
    y=tail(df.1$CHS_EVENT_SMAS_HISTORY_ID) #get the number of levels needed for each one
    
    temp.n.per.site<-df.1 %>% 
      group_by(CHS_EVENT_SMAS_HISTORY_ID) %>% 
      summarise(number=n(),.groups="drop")
    
    # create plot for each PWL in df 
    plot <- 
      ggplot(df.1,
             aes(CHS_EVENT_SMAS_HISTORY_ID,CHR_RESULT_VALUE,color=group,drop=TRUE))
    
    if(max(temp.n.per.site$number)>2){
      plot<-plot+geom_boxplot()
      

      
      label="Boxes represent the interquartile range (25th to 75th percentiles) of the data for each site, whiskers represent the IQR +/- 1.5 times the IQR and dots indicate potential outlier values, or those outside of the IQR +/- 1.5 times the IQR. Stars at the bottom of the graph indicate a violation of a WQS (if applicable). Axis are presented in log scale for comparison by site." 
      
    }
    if(max(temp.n.per.site$number) <= 2){
      plot<-plot+geom_point()
      label="Stars at the bottom of the graph indicate a violation of a WQS (if applicable). Axis are presented in log scale for comparison by site." 
      
    }
    
    # scale_shape_manual(name="PWL Segment ID",values = 0:max(l))+
    
    plot<-plot+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      scale_y_log10(paste(str_to_title(df.1$CHEM_PARAMETER_NAME[df.1$CHEM_PARAMETER_NAME==chem_list[i]]), df.1$CHEM_PARAMETER_UNIT[df.1$CHEM_PARAMETER_NAME==chem_list[i]])) + 
      xlab("Site ID")+
      # geom_hline(yintercept=temp.statewide$q95,color="grey53")+
      # geom_hline(yintercept=temp.statewide$q75,color="grey53")+
      # geom_hline(yintercept=temp.statewide$q25,color="grey53")+
      scale_color_manual(breaks=names(group_colors),values=group_colors)+
      #scale_color_manual(values = group_colors,breaks = group_colors)+
      theme(legend.position="right",legend.title = element_blank()) +
      # #expand_limits(x=nlevels(y)+5)+
      # annotate(geom="text", label=paste("95th",temp.statewide$q95,sep="-"),x=nlevels(y)+2, y=temp.statewide$q95, vjust=-1,color="grey61",size=3)+
      # annotate(geom="text", label=paste("75th",temp.statewide$q75,sep="-"), x=nlevels(y)+2, y=temp.statewide$q75, vjust=-1,color="grey61",size=3)+
      # annotate(geom="text", label=paste("25th",temp.statewide$q25,sep="-"), x=nlevels(y)+2, y=temp.statewide$q25, vjust=-1,color="grey61",size=3)+
      #theme(legend.title=element_blank(),legend.margin=margin(10,10,10,10),legend.key = element_rect(colour = NA, fill = NA),legend.background=element_blank())+
      coord_cartesian(clip = "off")
    

  #look to see if there is high flow

      #facet_wrap(~high_flow_flag,drop = TRUE)
  
    #white hex #ffffff
    
    if(nrow(stars)>=1){
      plot+annotate(geom = "text",label=paste("*"),x=temp.stars$site_id, y=(min(df.1$CHR_RESULT_VALUE[df.1$CHEM_PARAMETER_NAME==chem_list[i]]+100,na.rm=TRUE)),color="black")
    }
    
    flows<-unique(df.1$high_flow_flag)
    
    if(stringr::str_detect(flows,"High flow")){
      plot<-plot+geom_point(data=filter(df.1,high_flow_flag=="High flow"),color="yellow")
    }
    
    
    #geom_rect(aes(xmin = nlevels(y), xmax = nlevels(y)+5, ymin = log10(min(df$result_value[df$chemical_name==chem_list[i]])), ymax = log10(max(df$result_value[df$chemical_name==chem_list[i]]))),
    # fill = "white", alpha = 0.1)# print plots to screen
    print(plot)
    cat('\n\n') 
    
  }
}

############################################################################################
#chemistry Graphs
# create graphing function by PWL
chem.graph.pwl <- function(df, na.rm = TRUE, ...){
   label="Horizontal lines represent the 95th, 75th, and 25th percentiles of statewide data for each endpoint.Boxes represent the interquartile range (25th to 75th percentiles) of the data for each site, whiskers represent the IQR +/- 1.5 times the IQR and dots indicate potential outlier values, or those outside of the IQR +/- 1.5 times the IQR. Stars at the bottom of the graph indicate a violation of a WQS (if applicable). Axis are presented in log scale for comparison by site." 
     
  df$CHEM_PARAMETER_NAME<-tolower(df$CHEM_PARAMETER_NAME)
 sbu.chem.statewide$CHEM_PARAMETER_NAME<-tolower(sbu.chem.statewide$CHEM_PARAMETER_NAME)
  
  # create list of chmistry's in data to loop over 
   df<-df %>% 
    arrange(CHEM_PARAMETER_NAME)
   
 chem_list <- unique(df$CHEM_PARAMETER_NAME)
  l<-length(chem_list)
  
  
  for (i in seq_along(chem_list)) { 
    #order by group
    df$order<-as.numeric(df$order)
    df<-df %>% 
      arrange(order)
    df$SITE_PWL_ID<-forcats::fct_reorder(df$SITE_PWL_ID,df$order)
    
    temp.statewide<-subset(sbu.chem.statewide,sbu.chem.statewide$CHEM_PARAMETER_NAME==chem_list[i])
    if(nrow(stars)>1){
      temp.stars<-subset(stars,stars$Parameter==chem_list[i])
    }
    
    temp.chem<-subset(df,df$CHEM_PARAMETER_NAME==chem_list[i])
    df.1<-subset(df, df$CHEM_PARAMETER_NAME==chem_list[i])
    
    df.1$SITE_PWL_ID<-droplevels(df.1$SITE_PWL_ID)
    y=tail(df.1$SITE_PWL_ID) #get the number of levels needed for each one
    
    #df.1$CHS_EVENT_SMAS_HISTORY_ID<-droplevels(df.1$CHS_EVENT_SMAS_HISTORY_ID)
    #y=tail(df.1$CHS_EVENT_SMAS_HISTORY_ID) #get the number of levels needed for each one
    
    
    # create plot for each PWL in df 
    plot <- 
      ggplot(df.1,
             aes(SITE_PWL_ID,CHR_RESULT_VALUE,color=group,drop=TRUE))+
      
      geom_boxplot() +

      #scale_shape_manual(name="PWL Segment ID",values = 0:max(l))+
      
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme(legend.position="right") + 
      
      scale_y_log10(paste(str_to_title(df.1$CHEM_PARAMETER_NAME[df.1$CHEM_PARAMETER_NAME==chem_list[i]]), df.1$CHEM_PARAMETER_UNIT[df.1$CHEM_PARAMETER_NAME==chem_list[i]])) + 
      xlab("PWL ID")+
      # geom_hline(yintercept=temp.statewide$q95,color="grey53")+
      # geom_hline(yintercept=temp.statewide$q75,color="grey53")+
      # geom_hline(yintercept=temp.statewide$q25,color="grey53")+
       scale_color_manual(breaks=names(group_colors),values=group_colors)+
      geom_jitter(data=df.1 %>%filter(high_flow_flag=="high"),
                       aes(SITE_PWL_ID,CHR_RESULT_VALUE, color="yellow"))+
      # #expand_limits(x=nlevels(y)+5)+
      # annotate(geom="text", label=paste("95th",temp.statewide$q95,sep="-"),x=nlevels(y)+2, y=temp.statewide$q95, vjust=-1,color="grey61",size=3)+
      # annotate(geom="text", label=paste("75th",temp.statewide$q75,sep="-"), x=nlevels(y)+2, y=temp.statewide$q75, vjust=-1,color="grey61",size=3)+
      # annotate(geom="text", label=paste("25th",temp.statewide$q25,sep="-"), x=nlevels(y)+2, y=temp.statewide$q25, vjust=-1,color="grey61",size=3)+
      theme(legend.title=element_blank(),legend.margin=margin(10,10,10,10),legend.key = element_rect(colour = NA, fill = NA),legend.background=element_blank())+
      coord_cartesian(clip = "off")

      
    
    if(nrow(stars)>=1){
      plot+annotate(geom = "text",label=paste("*"),x=temp.stars$PWL_segment, y=(min(df.1$CHR_RESULT_VALUE[df.1$CHEM_PARAMETER_NAME==chem_list[i]],na.rm=TRUE)-10),color="black")}
    
    #geom_rect(aes(xmin = nlevels(y), xmax = nlevels(y)+5, ymin = log10(min(df$result_value[df$chemical_name==chem_list[i]])), ymax = log10(max(df$result_value[df$chemical_name==chem_list[i]]))),
    # fill = "white", alpha = 0.1)# print plots to screen
    label="Boxes represent the interquartile range (25th to 75th percentiles) of the data for each site, whiskers represent the IQR +/- 1.5 times the IQR and dots indicate potential outlier values, or those outside of the IQR +/- 1.5 times the IQR. Stars at the bottom of the graph indicate a violation of a WQS (if applicable). Axis are presented in log scale for comparison by site." 
    
    flows<-unique(df.1$high_flow_flag)
    
    if(stringr::str_detect(flows,"High flow")){
      plot<-plot+geom_point(data=filter(df.1,high_flow_flag=="High flow"),color="yellow")
    }
    
    print(plot)
    cat('\n\n') 
    
  }
}
##########################################################################################
# create graphing function for those that DONT need to be log10 (ug/L or mg/l-so not weird ones)
# by SITE
chem.graph.continuous.site <- function(df, na.rm = TRUE, ...){
  df$CHEM_PARAMETER_NAME<-tolower(df$CHEM_PARAMETER_NAME)
   label="Horizontal lines represent the 95th, 75th, and 25th percentiles of statewide data for each endpoint.Boxes represent the interquartile range (25th to 75th percentiles) of the data for each site, whiskers represent the IQR +/- 1.5 times the IQR and dots indicate potential outlier values, or those outside of the IQR +/- 1.5 times the IQR. Stars at the bottom of the graph indicate a violation of a WQS (if applicable). Axis are presented in log scale for comparison by site." 
      
  sbu.chem.statewide$CHEM_PARAMETER_NAME<-tolower(sbu.chem.statewide$CHEM_PARAMETER_NAME)
  
  # create list of chmistry's in data to loop over 
    df<-df %>% 
    arrange(CHEM_PARAMETER_NAME)
  
  chem_list <- unique(df$CHEM_PARAMETER_NAME)
  l<-length(chem_list)
  
  
  for (i in seq_along(chem_list)) { 
    #order by group
    df$order<-as.numeric(df$order)
    df<-df %>% 
      arrange(order)
    df$CHS_EVENT_SMAS_HISTORY_ID<-forcats::fct_reorder(df$CHS_EVENT_SMAS_HISTORY_ID,df$order)
    
    temp.statewide<-subset(sbu.chem.statewide,sbu.chem.statewide$CHEM_PARAMETER_NAME==chem_list[i])
    if(nrow(stars)>1){temp.stars<-subset(stars,stars$Parameter==chem_list[i])}
    
    temp.chem<-subset(df,df$CHEM_PARAMETER_NAME==chem_list[i])
    df.1<-subset(df, df$CHEM_PARAMETER_NAME==chem_list[i])
    #df.1$SITE_PWL_ID<-droplevels(df.1$SITE_PWL_ID)
    #y=tail(df.1$SITE_PWL_ID) #get the number of levels needed for each one
    
    df.1$CHS_EVENT_SMAS_HISTORY_ID<-droplevels(df.1$CHS_EVENT_SMAS_HISTORY_ID)
    y=tail(df.1$CHS_EVENT_SMAS_HISTORY_ID) #get the number of levels needed for each one
    
    # create plot for each PWL in df 
    plot <- 
      ggplot(df.1,
             aes(CHS_EVENT_SMAS_HISTORY_ID,CHR_RESULT_VALUE,color=group,drop=TRUE))
      
      if(nrow(temp.chem)>2){plot<-plot+geom_boxplot()
     } 
     if(nrow(temp.chem)<=2){plot<-plot+geom_point()
     label="Horizontal lines represent the 95th, 75th, and 25th percentiles of statewide data for each endpoint. Stars at the bottom of the graph indicate a violation of a WQS (if applicable). Axis are presented in log scale for comparison by site.The total number of reported values illustrated for each sampling location can vary due to non-detection and QA/QC procedures. Descriptions of removed records are presented in Appendix IV."
 }
      
      #scale_shape_manual(name="PWL Segment ID",values = 0:max(l))+
      
     plot<-plot+
       theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme(legend.position="right") + 
      
      scale_y_continuous(paste(str_to_title(df.1$CHEM_PARAMETER_NAME[df.1$CHEM_PARAMETER_NAME==chem_list[i]]), df.1$CHEM_PARAMETER_UNIT[df.1$CHEM_PARAMETER_NAME==chem_list[i]])) + 
      xlab("Site ID")+
      # geom_hline(yintercept=temp.statewide$q95,color="grey53")+
      # geom_hline(yintercept=temp.statewide$q75,color="grey53")+
      # geom_hline(yintercept=temp.statewide$q25,color="grey53")+
       scale_color_manual(breaks=names(group_colors),values=group_colors)+
      # #expand_limits(x=nlevels(y)+5)+
      # annotate(geom="text", label=paste("95th",temp.statewide$q95,sep="-"),x=nlevels(y)+2, y=temp.statewide$q95, vjust=-1,color="grey61",size=3)+
      # annotate(geom="text", label=paste("75th",temp.statewide$q75,sep="-"), x=nlevels(y)+2, y=temp.statewide$q75, vjust=-1,color="grey61",size=3)+
      # annotate(geom="text", label=paste("25th",temp.statewide$q25,sep="-"), x=nlevels(y)+2, y=temp.statewide$q25, vjust=-1,color="grey61",size=3)+
      theme(legend.title=element_blank(),legend.margin=margin(10,10,10,10),legend.key = element_rect(colour = NA, fill = NA),legend.background=element_blank())+
      coord_cartesian(clip = "off")
    
    if(nrow(stars)>=1){plot+annotate(geom = "text",label=paste("*"),x=temp.stars$PWL_segment, y=(min(df.1$CHR_RESULT_VALUE[df.1$CHEM_PARAMETER_NAME==chem_list[i]])-10),color="black")}
    
     flows<-unique(df.1$high_flow_flag)
     
     if(stringr::str_detect(flows,"High flow")){
       plot<-plot+geom_point(data=filter(df.1,high_flow_flag=="High flow"),color="yellow")
     }
     
    #geom_rect(aes(xmin = nlevels(y), xmax = nlevels(y)+5, ymin = log10(min(df$result_value[df$chemical_name==chem_list[i]])), ymax = log10(max(df$result_value[df$chemical_name==chem_list[i]]))),
    # fill = "white", alpha = 0.1)# print plots to screen
    print(plot)
    cat('\n\n') 
    
  }
  
  
}
#############################################################################################
# create graphing function for those that DONT need to be log10 (ug/L or mg/l-so not weird ones)
# by PWL
chem.graph.continuous.pwl <- function(df, na.rm = TRUE, ...){
  df$CHEM_PARAMETER_NAME<-tolower(df$CHEM_PARAMETER_NAME)
   label="Horizontal lines represent the 95th, 75th, and 25th percentiles of statewide data for each endpoint.Boxes represent the interquartile range (25th to 75th percentiles) of the data for each site, whiskers represent the IQR +/- 1.5 times the IQR and dots indicate potential outlier values, or those outside of the IQR +/- 1.5 times the IQR. Stars at the bottom of the graph indicate a violation of a WQS (if applicable). Axis are presented in log scale for comparison by WI/PWL ID." 
      
  sbu.chem.statewide$CHEM_PARAMETER_NAME<-tolower(sbu.chem.statewide$CHEM_PARAMETER_NAME)
  
  # create list of chmistry's in data to loop over 
   df<-df %>% 
    arrange(CHEM_PARAMETER_NAME)
  
   chem_list <- unique(df$CHEM_PARAMETER_NAME)
  l<-length(chem_list)
  
  
  for (i in seq_along(chem_list)) { 
    #order by group
    df$order<-as.numeric(df$order)
    df<-df %>% 
      arrange(order)
    df$SITE_PWL_ID<-forcats::fct_reorder(df$SITE_PWL_ID,df$order)
    
    temp.statewide<-subset(sbu.chem.statewide,sbu.chem.statewide$CHEM_PARAMETER_NAME==chem_list[i])
    if(nrow(stars)>1){temp.stars<-subset(stars,stars$Parameter==chem_list[i])}
    
    temp.chem<-subset(df,df$CHEM_PARAMETER_NAME==chem_list[i])
    df.1<-subset(df, df$CHEM_PARAMETER_NAME==chem_list[i])
    
    df.1$SITE_PWL_ID<-droplevels(df.1$SITE_PWL_ID)
    y=tail(df.1$SITE_PWL_ID) #get the number of levels needed for each one
    
    #df.1$CHS_EVENT_SMAS_HISTORY_ID<-droplevels(df.1$CHS_EVENT_SMAS_HISTORY_ID)
    #y=tail(df.1$CHS_EVENT_SMAS_HISTORY_ID) #get the number of levels needed for each one
    
    # create plot for each PWL in df 
    plot <- 
      ggplot(df.1,
             aes(SITE_PWL_ID,CHR_RESULT_VALUE,color=group,drop=TRUE))+
      
      geom_boxplot() +
      #geom_point()+
      
      #scale_shape_manual(name="PWL Segment ID",values = 0:max(l))+
      
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme(legend.position="right") + 
      
      scale_y_continuous(paste(str_to_title(df.1$CHEM_PARAMETER_NAME[df.1$CHEM_PARAMETER_NAME==chem_list[i]]), df.1$CHEM_PARAMETER_UNIT[df.1$CHEM_PARAMETER_NAME==chem_list[i]])) + 
      xlab("PWL ID")+
      # geom_hline(yintercept=temp.statewide$q95,color="grey53")+
      # geom_hline(yintercept=temp.statewide$q75,color="grey53")+
      # geom_hline(yintercept=temp.statewide$q25,color="grey53")+
       scale_color_manual(breaks=names(group_colors),values=group_colors)+
      # #expand_limits(x=nlevels(y)+5)+
      # annotate(geom="text", label=paste("95th",temp.statewide$q95,sep="-"),x=nlevels(y)+2, y=temp.statewide$q95, vjust=-1,color="grey61",size=3)+
      # annotate(geom="text", label=paste("75th",temp.statewide$q75,sep="-"), x=nlevels(y)+2, y=temp.statewide$q75, vjust=-1,color="grey61",size=3)+
      # annotate(geom="text", label=paste("25th",temp.statewide$q25,sep="-"), x=nlevels(y)+2, y=temp.statewide$q25, vjust=-1,color="grey61",size=3)+
      theme(legend.title=element_blank(),legend.margin=margin(10,10,10,10),legend.key = element_rect(colour = NA, fill = NA),legend.background=element_blank())+
      coord_cartesian(clip = "off")
    
    if(nrow(temp.stars)>=1){plot+annotate(geom = "text",label=paste("*"),x=temp.stars$PWL_segment, y=(min(df.1$CHR_RESULT_VALUE[df.1$CHEM_PARAMETER_NAME==chem_list[i]])-1),color="black")}
   
    flows<-unique(df.1$high_flow_flag)
    
    if(stringr::str_detect(flows,"High flow")){
      plot<-plot+geom_point(data=filter(df.1,high_flow_flag=="High flow"),color="yellow")
    }
    
    #geom_rect(aes(xmin = nlevels(y), xmax = nlevels(y)+5, ymin = log10(min(df$result_value[df$chemical_name==chem_list[i]])), ymax = log10(max(df$result_value[df$chemical_name==chem_list[i]]))),
    # fill = "white", alpha = 0.1)# print plots to screen
    print(plot)
    cat('\n\n') 
    
  }
  
  
}
#################################################################################################
# create graphing function for those that need to be log10 (ug/L or mg/l-so not weird ones)
# by SITE
chem.graph.continuous.site <- function(df, na.rm = TRUE, ...){
  df$CHEM_PARAMETER_NAME<-tolower(df$CHEM_PARAMETER_NAME)
  
  sbu.chem.statewide$CHEM_PARAMETER_NAME<-tolower(sbu.chem.statewide$CHEM_PARAMETER_NAME)
  
  # create list of chmistry's in data to loop over 
   df<-df %>% 
    arrange(CHEM_PARAMETER_NAME)
  
   chem_list <- unique(df$CHEM_PARAMETER_NAME)
  
  l<-length(chem_list)
  
  
  for (i in seq_along(chem_list)) { 
    #order by group
    df$order<-as.numeric(df$order)
    df<-df %>% 
      arrange(order)
    df$CHS_EVENT_SMAS_HISTORY_ID<-forcats::fct_reorder(df$CHS_EVENT_SMAS_HISTORY_ID,df$order)
    
    temp.statewide<-subset(sbu.chem.statewide,sbu.chem.statewide$CHEM_PARAMETER_NAME==chem_list[i])
    if(nrow(stars)>=1){temp.stars<-subset(stars,stars$Parameter==chem_list[i])}
    
    temp.chem<-subset(df,df$CHEM_PARAMETER_NAME==chem_list[i])
    df.1<-subset(df, df$CHEM_PARAMETER_NAME==chem_list[i])
    #df.1$SITE_PWL_ID<-droplevels(df.1$SITE_PWL_ID)
    #y=tail(df.1$SITE_PWL_ID) #get the number of levels needed for each one
    
    df.1$CHS_EVENT_SMAS_HISTORY_ID<-droplevels(df.1$CHS_EVENT_SMAS_HISTORY_ID)
    y=tail(df.1$CHS_EVENT_SMAS_HISTORY_ID) #get the number of levels needed for each one
    
    # create plot for each PWL in df 
    plot <- 
      ggplot(df.1,
             aes(CHS_EVENT_SMAS_HISTORY_ID,CHR_RESULT_VALUE,color=group,drop=TRUE))
      
      if(nrow(df.1)>=3){plot<-plot+geom_boxplot()
      label="Boxes represent the interquartile range (25th to 75th percentiles) of the data for each site, whiskers represent the IQR +/- 1.5 times the IQR and dots indicate potential outlier values, or those outside of the IQR +/- 1.5 times the IQR. Stars at the bottom of the graph indicate a violation of a WQS (if applicable). Axis are presented in log scale for comparison by site. Yellow dots (if included) indicate samples taken at event flow rates" 
     }
      if(nrow(df.1)<=2){plot<-plot+geom_point()
           label="Stars at the bottom of the graph indicate a violation of a WQS (if applicable). Axis are presented in log scale for comparison by site.The total number of reported values illustrated for each sampling location can vary due to non-detection and QA/QC procedures. Descriptions of removed records are presented in Appendix IV. Yellow dots (if included) indicate samples taken at event flow rates."
 }
      
      #scale_shape_manual(name="PWL Segment ID",values = 0:max(l))+
     plot<-plot+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme(legend.position="right") + 
      
      scale_y_continuous(paste(str_to_title(df.1$CHEM_PARAMETER_NAME[df.1$CHEM_PARAMETER_NAME==chem_list[i]]), df.1$CHEM_PARAMETER_UNIT[df.1$CHEM_PARAMETER_NAME==chem_list[i]])) + 
      xlab("Site ID")+
      # geom_hline(yintercept=temp.statewide$q95,color="grey53")+
      # geom_hline(yintercept=temp.statewide$q75,color="grey53")+
      # geom_hline(yintercept=temp.statewide$q25,color="grey53")+
       scale_color_manual(breaks=names(group_colors),values=group_colors)+
      # #expand_limits(x=nlevels(y)+5)+
      # annotate(geom="text", label=paste("95th",temp.statewide$q95,sep="-"),x=nlevels(y)+2, y=temp.statewide$q95, vjust=-1,color="grey61",size=3)+
      # annotate(geom="text", label=paste("75th",temp.statewide$q75,sep="-"), x=nlevels(y)+2, y=temp.statewide$q75, vjust=-1,color="grey61",size=3)+
      # annotate(geom="text", label=paste("25th",temp.statewide$q25,sep="-"), x=nlevels(y)+2, y=temp.statewide$q25, vjust=-1,color="grey61",size=3)+
      theme(legend.title=element_blank(),legend.margin=margin(10,10,10,10),legend.key = element_rect(colour = NA, fill = NA),legend.background=element_blank())+
      coord_cartesian(clip = "off")
    
    if(nrow(stars)>=1){plot+annotate(geom = "text",label=paste("*"),x=temp.stars$PWL_segment, y=(min(df.1$CHR_RESULT_VALUE[df.1$CHEM_PARAMETER_NAME==chem_list[i]])-10),color="black")}
    
    #geom_rect(aes(xmin = nlevels(y), xmax = nlevels(y)+5, ymin = log10(min(df$result_value[df$chemical_name==chem_list[i]])), ymax = log10(max(df$result_value[df$chemical_name==chem_list[i]]))),
    # fill = "white", alpha = 0.1)# print plots to screen
     flows<-unique(df.1$high_flow_flag)
     
     if(stringr::str_detect(flows,"High flow")){
       plot<-plot+geom_point(data=filter(df.1,high_flow_flag=="High flow"),color="yellow")
     }
     
     
     print(plot)
    cat('\n\n') 
    
  }
  
  
}

############################################################################################
#in situ graphs
#by SITE
chem.graph.insitu.site <- function(df, na.rm = TRUE, ...){
  
  df$CHEM_PARAMETER_NAME<-tolower(df$CHEM_PARAMETER_NAME)
  
  #sbu.insitu.statewide$CHEM_PARAMETER_NAME<-tolower(sbu.insitu.statewide$CHEM_PARAMETER_NAME)
  
  # create list of chmistry's in data to loop over 
  df<-df %>% 
    arrange(CHEM_PARAMETER_NAME)
  
  chem_list.2 <- unique(df$CHEM_PARAMETER_NAME)
  
  
  for (i in seq_along(chem_list.2)) { 
    
    #create dataframe for statewide
    temp.insitu.sw<-subset(sbu.insitu.statewide,sbu.insitu.statewide$CHEM_PARAMETER_NAME==chem_list.2[i])
    
    #order by group
    df$order<-as.numeric(df$order)
    df<-df %>% 
      arrange(order)
    df$ISWC_EVENT_SMAS_HISTORY_ID<-forcats::fct_reorder(df$ISWC_EVENT_SMAS_HISTORY_ID,df$order)
    
    
    if(nrow(stars)>1){temp.stars.i<-subset(stars.insitu,stars$Parameter==chem_list.2[i])}
    df.1<-subset(df, df$CHEM_PARAMETER_NAME==chem_list.2[i])
    
    #df.1$SITE_PWL_ID<-droplevels(df.1$SITE_PWL_ID)#get rid of any PWL's that don't have data
    #y=tail(df.1$SITE_PWL_ID)#get the factor alone for the plots
    
    df.1$ISWC_EVENT_SMAS_HISTORY_ID<-droplevels(df.1$ISWC_EVENT_SMAS_HISTORY_ID)#get rid of any PWL's that don't have data
    y=tail(df.1$ISWC_EVENT_SMAS_HISTORY_ID)#get the factor alone for the plots
    
    temp.n.per.site<-df.1 %>% 
    group_by(ISWC_EVENT_SMAS_HISTORY_ID) %>% 
    summarise(number=n(),.groups="drop")
    
    
    # create plot for each parameter with PWL in df 
    plot <- 
      ggplot(df.1,
             aes(ISWC_EVENT_SMAS_HISTORY_ID,ISWC_RESULT,color=group,drop=TRUE)) 
    
   #  if(max(temp.n.per.site$number>2)){
   # print("this will be a boxplot")}
   #  
   #  if(max(temp.n.per.site$number<2)){
   #    print("this will be a pointt")}
   #  
    if(max(temp.n.per.site$number)< 2){plot<-plot+geom_point()
    label="Horizontal lines represent the 95th, 75th, and 25th percentiles of statewide data for each endpoint. Stars at the bottom of the graph indicate a violation of a WQS (if applicable). Axis are presented in log scale for comparison by site.The total number of reported values illustrated for each sampling location can vary due to non-detection and QA/QC procedures. Descriptions of removed records are presented in Appendix IV. Yellow dots (if included) indicate samples taken at event flow rates"
    }
  
    if(max(temp.n.per.site$number)>2){
      plot<-plot+geom_boxplot()
      label="Horizontal lines represent the 95th, 75th, and 25th percentiles of statewide data for each endpoint.Boxes represent the interquartile range (25th to 75th percentiles) of the data for each site, whiskers represent the IQR +/- 1.5 times the IQR and dots indicate potential outlier values, or those outside of the IQR +/- 1.5 times the IQR. Stars at the bottom of the graph indicate a violation of a WQS (if applicable). Axis are presented in log scale for comparison by site.Yellow dots (if included) indicate samples taken at event flow rates" 
    }
    
    
     
      plot<-plot+
      theme_grey() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme(legend.position="right") +
      
      scale_y_continuous(paste(str_to_title(df.1$CHEM_PARAMETER_NAME[df.1$CHEM_PARAMETER_NAME==chem_list.2[i]]),df.1$CHEM_PARAMETER_UNIT[df.1$CHEM_PARAMETER_NAME==chem_list.2[i]])) + 
      xlab("Site ID")+
      # geom_hline(yintercept=temp.insitu.sw$q95,color="grey53")+
      # geom_hline(yintercept=temp.insitu.sw$q75,color="grey53")+
      # geom_hline(yintercept=temp.insitu.sw$q25,color="grey53")+
       scale_color_manual(breaks=names(group_colors),values=group_colors)+
      # annotate(geom="text", label=paste("95th",temp.insitu.sw$q95,sep="-"), x=nlevels(y)+2, y=temp.insitu.sw$q95, vjust=-1,color="grey61",size=3)+
      # annotate(geom="text", label=paste("75th",temp.insitu.sw$q75,sep="-"), x=nlevels(y)+2, y=temp.insitu.sw$q75, vjust=-1,color="grey61",size=3)+
      # annotate(geom="text", label=paste("25th",temp.insitu.sw$q25,sep="-"), x=nlevels(y)+2, y=temp.insitu.sw$q25, vjust=-1,color="grey61",size=3)+
      theme(legend.title=element_blank(),legend.margin=margin(10,10,10,10),legend.key = element_rect(colour = NA, fill = NA),legend.background=element_blank())+
      coord_cartesian(clip = "off")
    
    if(nrow(stars)>1){plot+annotate(geom = "text",label=paste("*"),
                                    x=temp.stars.i$PWL_segment, 
                                    y=(min(df.1$ISWC_RESULT[df.1$CHEM_PARAMETER_NAME==chem_list.2[i]],na.rm = TRUE)-0.1),
                                    color="black")}
    
    
      flows<-unique(df.1$high_flow_flag)
      
      if(stringr::str_detect(flows,"High flow")){
        plot<-plot+geom_point(data=filter(df.1,high_flow_flag=="High flow"),color="yellow")
      }
      
    # print plots to screen
    print(plot)
    
    cat('\n\n')   
  }
  
}
##############################################################################################
chem.graph.insitu.pwl <- function(df, na.rm = TRUE, ...){
  
  df$CHEM_PARAMETER_NAME<-tolower(df$CHEM_PARAMETER_NAME)
  label="Horizontal lines represent the 95th, 75th, and 25th percentiles of statewide data for each endpoint.Boxes represent the interquartile range (25th to 75th percentiles) of the data for each WI/PWL ID, whiskers represent the IQR +/- 1.5 times the IQR and dots indicate potential outlier values, or those outside of the IQR +/- 1.5 times the IQR. Stars at the bottom of the graph indicate a violation of a WQS (if applicable). Axis are presented in log scale for comparison by WI/PWL ID." 
    
  sbu.insitu.statewide$CHEM_PARAMETER_NAME<-tolower(sbu.insitu.statewide$CHEM_PARAMETER_NAME)
  
  # create list of chmistry's in data to loop over 
  #df$CHEM_PARAMETER_NAME
    df<-df %>% 
    arrange(CHEM_PARAMETER_NAME)
  
  
  chem_list.2 <- unique(df$CHEM_PARAMETER_NAME)
  
  
  for (i in seq_along(chem_list.2)) { 
    
    #create dataframe for statewide
    temp.insitu.sw<-subset(sbu.insitu.statewide,sbu.insitu.statewide$CHEM_PARAMETER_NAME==chem_list.2[i])
    
    #order by group
    df$order<-as.numeric(df$order)
    df<-df %>% 
      arrange(order)
    df$SITE_PWL_ID<-forcats::fct_reorder(df$SITE_PWL_ID,df$order)
    #need to make the PWL for the stars the same?
    factor.levels<-levels(df$SITE_PWL_ID)
    stars.insitu$PWL_segment<-factor(stars.insitu$PWL_segment,
                                     levels=factor.levels,
                                     labels = factor.levels,
                                     exclude = TRUE)
    
    if(nrow(stars.insitu)>=0){temp.stars.i<-subset(stars.insitu,stars.insitu$Parameter==chem_list.2[i])}
    df.1<-subset(df, df$CHEM_PARAMETER_NAME==chem_list.2[i])
    
    df.1$SITE_PWL_ID<-droplevels(df.1$SITE_PWL_ID)#get rid of any PWL's that don't have data
    y=tail(df.1$SITE_PWL_ID)#get the factor alone for the plots
    
    #df.1$ISWC_EVENT_SMAS_HISTORY_ID<-droplevels(df.1$ISWC_EVENT_SMAS_HISTORY_ID)#get rid of any PWL's that don't have data
    #y=tail(df.1$ISWC_EVENT_SMAS_HISTORY_ID)#get the factor alone for the plots
    
    
    # create plot for each parameter with PWL in df 
    plot <- 
      ggplot(df.1,
             aes(SITE_PWL_ID,ISWC_RESULT,color=group,drop=TRUE))+ 
      #SITE_PWL_ID
      geom_boxplot() +
      # geom_point()+
    
      
      theme_grey() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme(legend.position="right") +
      
      scale_y_continuous(paste(str_to_title(df.1$CHEM_PARAMETER_NAME[df.1$CHEM_PARAMETER_NAME==chem_list.2[i]]),df.1$CHEM_PARAMETER_UNIT[df.1$CHEM_PARAMETER_NAME==chem_list.2[i]])) + 
      xlab("PWL ID")+
      # geom_hline(yintercept=temp.insitu.sw$q95,color="grey53")+
      # geom_hline(yintercept=temp.insitu.sw$q75,color="grey53")+
      # geom_hline(yintercept=temp.insitu.sw$q25,color="grey53")+
       scale_color_manual(breaks=names(group_colors),values=group_colors)+
      # annotate(geom="text", label=paste("95th",temp.insitu.sw$q95,sep="-"), x=nlevels(y)+2, y=temp.insitu.sw$q95, vjust=-1,color="grey61",size=3)+
      # annotate(geom="text", label=paste("75th",temp.insitu.sw$q75,sep="-"), x=nlevels(y)+2, y=temp.insitu.sw$q75, vjust=-1,color="grey61",size=3)+
      # annotate(geom="text", label=paste("25th",temp.insitu.sw$q25,sep="-"), x=nlevels(y)+2, y=temp.insitu.sw$q25, vjust=-1,color="grey61",size=3)+
      theme(legend.title=element_blank(),legend.margin=margin(10,10,10,10),legend.key = element_rect(colour = NA, fill = NA),legend.background=element_blank())+
      coord_cartesian(clip = "off")+
      annotate(geom = "text",label=paste("*"),
                                     x=temp.stars.i$PWL_segment, 
                                     y=(min(df.1$ISWC_RESULT[df.1$CHEM_PARAMETER_NAME==chem_list.2[i]],na.rm = TRUE)),
                    color="black")
   
    
    flows<-unique(df.1$high_flow_flag)
    
    if(stringr::str_detect(flows,"High flow")){
      plot<-plot+geom_point(data=filter(df.1,high_flow_flag=="High flow"),color="yellow")
    }
    
    
    # print plots to screen
    print(plot)
    
    cat('\n\n')   
  }
  
}

