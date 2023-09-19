

library(tidyverse)
library(ggpattern)
library(usdata)
library(usmapdata)
library(showtext)
library(geosphere)
library(data.table)

install.packages("ggpattern")
install.packages("usdata")
install.packages("usmapdata")
install.packages("showtext")
install.packages("geosphere")
install.packages("data.table")

#JUST THE DATA
California: 31-39-3
Florida: 1-8
Idaho: 5-6-1
Nevada: 18-5
Oregon: 4-8
Virginia: 1-1



c("California","Florida","Idaho","Nevada","Oregon","Virginia")
Records<-c("32-39-3","'     2-8","6-6-1","18-6","4-9","1-2")


#####

#get state map coordinates
state_map<-map_data("state")

#default font from showtext
font_add_google("Noto Serif", "Noto Serif")
font_add_google("Roboto", "roboto")
showtext_auto()

#recreate dataset from New York Times
playing<-data.frame(
  state = c("California","Florida","Idaho","Nevada","Oregon","Virginia"),
  status = c("California","Florida","Idaho","Nevada","Oregon","Virginia"),
  pattern = c(rep("weave",3),"weave",rep("weave",2)),
  college = c("Stanford","South Florida","Boise St.","Notre Dame","Oregon","Liberty")
)

#merge dataset with original map data 
map_data<-state_map|>mutate(region=str_to_title(region))|>left_join(playing, by=c("region"="state"))|>mutate(pattern=case_when(is.na(status)~"weave",TRUE~pattern))
#create factor with levels for status to arrange the status in different order. defaults to alphabetical
map_data$status<-factor(map_data$status, levels=c("California","Florida","Idaho","Nevada","Oregon","Virginia"))
#use usdata library function state2abbr to get state abbreviations for our labels
map_data$state_abbr <- state2abbr(map_data$region)
#

#centroid labels - https://stackoverflow.com/questions/38699761/getting-the-centroids-of-lat-and-longitude-in-a-data-frame
findCentroid <- function(Lon, Lat, ...){
  centroid(cbind(Lon, Lat), ...)
}
setDT(map_data)
map_data[, c("cent_long", "cent_lat") := as.list(findCentroid(long, lat)), by = region]
map_data


#create labels
labels<-map_data|>
  distinct(region, state_abbr, cent_long, cent_lat)
  
labels1<-subset(labels,region == "California" |region == "Florida" |region == "Idaho" |region == "Nevada" 
                  |region == "Oregon" |region == "Virginia")

labels2<-labels1%>%
  mutate(record=case_when(region %in% c("California","Florida","Idaho","Nevada","Oregon","Virginia") ~ 
                            c("32-39-3","     2-8","6-6-1","18-6","4-9"," 1-2"), TRUE ~ "n/a"))%>%
  mutate(color=case_when(region %in% c("California","Florida","Idaho","Nevada","Oregon","Virginia") ~  
                           c("ghostwhite","goldenrod1","Blue","gold","Yellow","red"), TRUE ~ "purple"))%>%
mutate(college=case_when(region %in% c("California","Florida","Idaho","Nevada","Oregon","Virginia") ~  
                         c("Next Game:Stanford","South Florida","Boise St.","Notre Dame","Oregon","Liberty"), TRUE ~ "purple"))

#plot
ggplot(map_data,aes(x=long, y=lat, group=group))+
  #plot base map
  geom_polygon(fill='lightsteelblue', size=0.3, color="black")+ #lightsteelblue4 was good
  #plot states with statuses
  geom_polygon_pattern(data=map_data|>filter(!is.na(status)), inherit.aes=FALSE,
                       mapping=aes(x=long, y=lat, group=group, pattern=status, fill=status),
                       pattern_density=0.0, pattern_colour=NA, pattern_fill="white", pattern_spacing=0.02,
                       color="black", size=0.5)+ #grant-could get border for states in play
  #select colors manually
  scale_fill_manual(values=c("firebrick","darkgreen","darkorange","navyblue","green3","navyblue"))+
  scale_pattern_manual(values=c("none","none","none","none","none","none"))+
  #overlay text labels for each state
  geom_text(data=labels2|>filter(region %in% playing$state), inherit.aes=FALSE, 
            mapping=aes(x=cent_long,y=cent_lat, label=record, color=color), fontface="bold", size=5.25,hjust=.5,vjust=.99)+
  #geom_text(data=labels2|>filter(region %in% playing$state), inherit.aes=FALSE, 
            #mapping=aes(x=cent_long,y=cent_lat, label=college, color=color), fontface="bold", size=2.25,hjust=1,vjust=.5)+
  #add scale identity for color to map colors for each labels to actual value, e.g. white or black
  scale_color_identity()+
  coord_map()+
  labs(title="BYU all-time Record in States of 2022 Road/Neutral-Site Games",
       subtitle="In the 6 non-Utah games BYU played this year, here is how they have fared in each state all-time:",
       caption="Data from winsipedia.com, cougarstats.com, and byucougars.com\n Compiled and recreated by @GrantNielson2, code inspired by @Tanya_Shapiro")+
  theme_void()+
  theme(legend.position = "none",
        legend.title=element_blank(),
        legend.key.size = unit(0.2, 'cm'),
        plot.caption=element_text(hjust=0.05, color="black", vjust=12, size=8),
        plot.subtitle = element_text(hjust=0.5, color="black", vjust=-7, size=10, margin=margin(b=25)),
        plot.title=element_text(hjust=0.5, color="Royalblue", face="bold",vjust=-7, size=20, margin=margin(b=25)))

warnings()
rlang::last_error()

##Special thanks to Tanya Shapiro for much of the code inspiration 

