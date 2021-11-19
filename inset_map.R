#map package
#10/12/2021
#keleigh reynolds
#making a map maker to create a jpeg and then use it
#
##map function
#bounding box
library(ggmap)
library(ggrepel)
sites$SITE_LATITUDE<-as.numeric(sites$SITE_LATITUDE)
sites$SITE_LONGITUDE<-as.numeric(sites$SITE_LONGITUDE)

sites_dodge<-read.csv(here::here("data/dodge_map.csv"),stringsAsFactors = FALSE)
sites_dodge<-sites_dodge %>% 
  rename(lat=SITE_LATITUDE,
         long=SITE_LONGITUDE)

nybox_small<-make_bbox(long,lat,sites_dodge,f=0.1)

sitespwl<-here::here("data/map")


pwl.1<-rgdal::readOGR(
dsn = sitespwl,
layer="dec_wipwl_streams_kar",
verbose=FALSE
)

pwl.l<-unique(sites$SITE_PWL_ID)

extra.pwl<-list("0201-0066","0201-0026")
pwl.l<-append(pwl.l,extra.pwl)

pwl.cut<-subset(pwl.1,pwl.1$PWL_ID %in% pwl.l )

outline<-rgdal::readOGR(
dsn = sitespwl,
layer="globalwatershed",
verbose=FALSE
)

#change coords to web mercator for the map
pwl.cut<-sp::spTransform(pwl.cut, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
outline<-sp::spTransform(outline, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))

pwl.cut$type<-as.factor(pwl.cut$type)

ny.map1<-get_map(outline@bbox,source = "osm")
small.base<-get_map(nybox_small, source = "osm", zoom = 13)
  
ny.map2<-ggmap(ny.map1)+
 geom_path(data=pwl.cut,aes(x = long, y = lat,group=group),color="dodgerblue3",alpha=0.5,size=1)+
 #geom_path(data=subset(pwl.cut,type=="main"),aes(x = long, y = lat,group=group),color="slateblue3",size=0.9)+
 geom_point(data=sites,aes(x=SITE_LONGITUDE,y=SITE_LATITUDE,label=SITE_HISTORY_ID,color=group),size=4)+ 
        geom_path(data=outline,aes(x=long,y=lat,group=group),color="yellow")+
        geom_label_repel(data=sites,
 label= sites$SITE_HISTORY_ID, 
aes(x=SITE_LONGITUDE,
y=SITE_LATITUDE),box.padding   = 0.35, point.padding = 0.5,
             segment.color = 'grey50',size=2)+
   theme(legend.title=element_blank(),legend.margin=margin(10,10,10,10),
         legend.key = element_rect(colour = NA, fill = NA),
         legend.background=element_blank(),
         axis.title.x = element_blank(),
         axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
        )
      
ny.map2
#get bounding for the outline
outline.df<-fortify(outline)
nybox<-sf::st_as_sfc(sf::st_bbox(outline))

ny.map3<- ny.map2+
  ggsn::scalebar(outline.df, dist = 2, dist_unit = "km",
             transform = TRUE, model = "WGS84",location="topleft",
             height = 0.01, st.size = 4,st.bottom=TRUE,anchor =c(
               x=min(outline.df$long)+0.01,
               y=max(outline.df$lat)-0.01)
             )

ny.map3

library("maps")
states<-sf::st_as_sf(map("state",plot=FALSE,fill=TRUE))
nys<-states %>% 
  filter(ID=="new york")
counties<-sf::st_as_sf(map("county",plot=FALSE,fill=TRUE))
counties<-counties %>% 
  filter(grepl("new york",counties$ID))

n<-ggplot(data = nys) +
    geom_sf(data = nys, fill = "white")+
 geom_sf(data=nybox, fill = NA, color = "red", size = 1.2) +
  theme_void()
n

ny.map4<-ggmap(small.base)+
geom_point(data=sites,aes(x=SITE_LONGITUDE,y=SITE_LATITUDE,label=SITE_HISTORY_ID,color=group),size=4)+ 
   geom_path(data=pwl.cut,aes(x = long, y = lat,group=group),color="dodgerblue3",alpha=0.5,size=1)+
        geom_label_repel(data=sites,
 label= sites$SITE_HISTORY_ID, 
aes(x=SITE_LONGITUDE,
y=SITE_LATITUDE),box.padding   = 0.35, point.padding = 0.5,
             segment.color = 'grey50',size=2)+
  theme(legend.position = "none")+
     theme(legend.title=element_blank(),legend.margin=margin(10,10,10,10),
         legend.key = element_rect(colour = NA, fill = NA),
         legend.background=element_blank(),
         axis.title.x = element_blank(),
         axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
        )
ny.map4

ny.map4<- ny.map4+
  ggsn::scalebar(sites_dodge, dist = 0.5, dist_unit = "km",
             transform = TRUE, model = "WGS84",location="topleft")
ny.map4

s<-ny.map3
library(cowplot)
gg_inset_map= ggdraw()+
  draw_plot(s)+
  draw_plot(n,x=0.5,y=0.07,width = 0.2,height = 0.3)
gg_inset_map 

final<-plot_grid(ny.map4,gg_inset_map, ncol=2,align = "hv")
final

ggsn::north2(gg_inset_map,symbol = 12)

#old_map
##library(leaflet)
#map function
  #bounding box
library(ggmap)
library(ggrepel)
sites$SITE_LATITUDE<-as.numeric(sites$SITE_LATITUDE)
sites$SITE_LONGITUDE<-as.numeric(sites$SITE_LONGITUDE)
#sites_dodge<-read.csv(here::here("data/dodge_map.csv"),stringsAsFactors = FALSE)

#nybox<-make_bbox(sites,lon=SITE_LONGITUDE,lat=SITE_LATITUDE)
#nybox<-make_bbox(sites,lon=SITE_LONGITUDE,lat=SITE_LATITUDE)
#
 
#ny.map<-qmap(nybox,zoom=map_zoom, 
#             source = "osm",color="bw")+geom_point(data=sites,aes(x=SITE_LONGITUDE,y=SITE_LATITUDE,label=SITE_HISTORY_ID,color=group),size=4)+ 
#  geom_label_repel(data=sites,
#    label= sites$SITE_HISTORY_ID, 
#    aes(x=SITE_LONGITUDE,
#    y=SITE_LATITUDE),box.padding   = 0.35, point.padding = 0.5,
#                  segment.color = 'grey50',size=2)+
#theme(legend.title=element_blank(),legend.margin=margin(10,10,10,10),legend.key = element_rect(colour = NA, fill = NA),legend.background=element_blank())
      


#ny.map<-ny.map+ggsn::scalebar(x.min = min(sites$SITE_LONGITUDE),
#                        x.max = max(sites$SITE_LONGITUDE),
#                        y.min = min(sites$SITE_LATITUDE),
#                        y.max = max(sites$SITE_LATITUDE),
#                        dist = 0.5, dist_unit = "km",
#                        transform = TRUE, model = "WGS84",
 #                       location="topright")
#ny.map

