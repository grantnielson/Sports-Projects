#### ?- How does QB passer rating correlate with win %?
install.packages("cfbfastR")
library(cfbfastR)
install.packages("tictoc")
library(tictoc)
remotes::install_github(repo = "sportsdataverse/cfbfastR")
remotes::install_github(repo = "Kazink36/cfbplotR")
install.packages(tidyverse)
library(cfbfastR)
install.packages("cfbplotR")
library(cfbplotR)
library(tidyverse)
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}

# You can install using the pacman package using the following code:
if (!requireNamespace('pacman', quietly = TRUE)){
  install.packages('pacman')
}
pacman::p_load_current_gh("sportsdataverse/cfbplotR", dependencies = TRUE, update = TRUE)


pacman::p_load(zoo, ggimage, gt)
###
pbp <- data.frame()
pbp <- cfbfastR::load_cfb_pbp(2022)

team_info <- cfbfastR::cfbd_team_info()
team_info <- team_info %>% 
  select(team = school,conference,mascot) %>% 
  filter(conference %in% c("Pac-12","ACC","SEC","Big Ten","Big 12"))


WRS<-try(cfbd_player_usage(year = 2022, position = "WR", team = "BYU"))

jaren<-try(cfbd_player_usage(year = 2022, position = "QB", team = "BYU"))
try(cfbd_player_usage(year = 2022, position = "TE", team = "BYU"))

playerstats<-cfbd_game_player_stats(year = 2022,team = "BYU")


TES_pre<-try(cfbd_player_usage(year = 2021, position = "TE", teams = c("BYU","Baylor")))
ALL_QB_pre<-try(cfbd_player_usage(year = 2022, position = "QB"))
QBR<-try(cfbd_game_player_stats(year = 2022, week = 2, team = "BYU", category = 'passing'))
colnames(QBR)

try(cfbd_team_info(conference = "aac"))%>%select(school,team_id)

#betting
try(cfbd_betting_lines(year = 2022, week = , team = "BYU"))



### ESPN QBR
BYU<-252
USF<-58
Baylor<-239
Oregon<-2483
UtSt<- 328
Wyoming<-2751
NotreDame<-87
Arkansas<-8
Liberty<-2335
ECU<-151
Boise<-68
Stanford<-24

schedule<-c(USF,Baylor,Oregon,UtSt,Wyoming,NotreDame,Arkansas,Liberty,ECU,Boise,BYU,Stanford) 
espn_cfb_team_stats(team_id = 252, year = 2022)%>% select(passing_qb_rating)

  
save <- rep(1,12)
for(i in 1:(length(schedule))){
  currteam <- schedule[[i]]
  save[[i]] <- as.numeric(try(espn_cfb_team_stats(team_id =currteam, year = 2022)) %>% select(passing_qb_rating))
    }
qbrs13real<-save
qbrs13 #earlier one
class(qbrs13real)
df <- data.frame(qbrs13real) #this kinda works but turns it into one row and has wack titles for each rating

qbrs13real[2]

qbr_tibble<-as_tibble(df) #not working yet

###
equipos<-c(USF,Baylor,Oregon,UtSt,Wyoming,NotreDame,Arkansas,Liberty,ECU,Boise,BYU,Stanford)
quoteam<-c("South Florida","Baylor","Oregon","Utah State","Wyoming","Notre Dame","Arkansas","Liberty","East Carolina","Boise State","BYU","Stanford")
registro<-cfbd_game_records(2022)%>%
  mutate(record=(total_wins/total_games))%>% select(record)

registro %>% select(record)

cfbd_game_records(2022, team = "South Florida")%>%
  mutate(record=(total_wins/total_games))%>%select(record)



for(i in 1:(length(equipos))){
  currequipo <- quoteam[[i]]
  saving[[i]] <- (as.numeric(cfbd_game_records(2022, team = currequipo)%>%
                            mutate(record=(total_wins/total_games))%>%select(record)))
  }
finrecord13real<-saving
finrecord13 ## thru 11 games
class(finrecord13)
team_rec_tibble<-as_tibble(finrecord13real)





#######
dataforthis <- data.frame(NA_row = rep(NA, 12))
for(i in 1:(length(equipos))){
  currequipo <- quoteam[[i]]
  dataforthis[[i]] <- (cfbd_game_records(2022, team = currequipo)%>%
                               mutate(record=(total_wins/total_games))%>%select(record))
  rownames(dataforthis)[i]<-paste0(currequipo,i)
}

dataforthis[3,]


savings
finrecords<-savings

######
table<-as_tibble(do.call(rbind.data.frame, finrecords))
table4<-as_tibble(do.call(rbind.data.frame, df)) #qbr
table1<-as_tibble(do.call(rbind.data.frame, finrecord13real))
class(finrecord13real)
rekord<-as_tibble(finrecord13real)

QBr<-as_tibble(do.call(rbind.data.frame,table4))
c(qbrs13real)
c(finrecord)
colnames(QBr) = c("QBR") 



#okay, gonna rock with rekord and QBr for now.
QBr
rekord

QBr$Team <-team_names
rekord$Team <-team_names

both<-full_join(QBr, rekord, by='Team')

## get team name list
rm(USF,Baylor,Oregon,UtSt,Wyoming,NotreDame,Arkansas,Liberty,ECU,Boise,BYU,Stanford)
team_names<-c("South Florida","Baylor","Oregon","Utah State","Wyoming","Notre Dame","Arkansas","Liberty","East Carolina","Boise State","BYU","Stanford")
## now time to graph!


ggplot(both, aes(x = value, y = QBR)) +
  #geom_median_lines(aes(v_var = value, h_var = QBR)) +
  geom_cfb_logos(aes(team = Team), width = 0.075) +
  labs(x = "Team Record",y = "QBR") +
  theme_grey()+
  theme(panel.background = element_rect(fill = "#C1E3EB"))+
  theme(panel.border = element_rect(fill = "transparent", # Needed to add the border
                                    color = 1,            # Color of the border
                                    size = 1))  +
  theme(plot.background = element_rect(fill = "gray95"))+ # Background color of the plot
  labs(title = "Team Record vs Quality of QB Play", 
       subtitle="BYU's 2022 opponents' season record compared to season quarterback rating",
       caption = "* QBR data from CFBfastR") +
  theme(plot.title = element_text(color = "black",face="bold"))+
  theme(plot.subtitle = element_text(color = "#434E51"))+
  geom_smooth(method=lm, se=FALSE)
  
#update
#classic
#linedraw

