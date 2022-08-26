
#####################################################

#         Liverpool x Mancherster City

#         Getting Data from Understat

####################################################

# Libraries

library(worldfootballR)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gridExtra)

# Get Premier League 2021/2022 Matches

matches <- understat_league_match_results("EPL",
                                          season_start_year = 2021)

matches <- matches[,c("datetime", "home_team", "home_xG", "away_team", "away_xG")]




# Liverpool Matches Stats

liv_matches <- matches %>% 
      filter(home_team == "Liverpool" | away_team == "Liverpool")
 
# Data For and Against

liv_fa = data.frame(1:38, rep(NA, 38), rep(NA, 38), rep(NA, 38), rep(NA, 38))
colnames(liv_fa) = c("Matches", "Team", "xG", "Other", "xGAgainst")

for (i in 1:38){
  
  if (liv_matches$home_team[i] == "Liverpool"){
    
    liv_fa$Team[i] = "Liverpool"
    liv_fa$xG[i] = liv_matches$home_xG[i]
    liv_fa$Other[i] = liv_matches$away_team[i]
    liv_fa$xGAgainst[i] = liv_matches$away_xG[i]
    
  } else {
    
    liv_fa$Team[i] = "Liverpool"
    liv_fa$xG[i] = liv_matches$away_xG[i]
    liv_fa$Other[i] = liv_matches$home_team[i]
    liv_fa$xGAgainst[i] = liv_matches$home_xG[i]
    
  }
  
}



# Manchester City Matches Stats

mci_matches <- matches %>% 
  filter(home_team == "Manchester City" | away_team == "Manchester City")

# Data For and Against

mci_fa = data.frame(1:38, rep(NA, 38), rep(NA, 38), rep(NA, 38), rep(NA, 38))
colnames(mci_fa) = c("Matches", "Team", "xG", "Other", "xGAgainst")

for (i in 1:38){
  
  if (mci_matches$home_team[i] == "Manchester City"){
    
    mci_fa$Team[i] = "Manchester City"
    mci_fa$xG[i] = mci_matches$home_xG[i]
    mci_fa$Other[i] = mci_matches$away_team[i]
    mci_fa$xGAgainst[i] = mci_matches$away_xG[i]
    
  } else {
    
    mci_fa$Team[i] = "Manchester City"
    mci_fa$xG[i] = mci_matches$away_xG[i]
    mci_fa$Other[i] = mci_matches$home_team[i]
    mci_fa$xGAgainst[i] = mci_matches$home_xG[i]
    
  }

}


# Graphs

# Liverpool
liverpool = ggplot() +
  geom_point(liv_fa, 
             mapping = aes(x = Matches, y = xG, color = "xG"), 
             color = "red", fill = "red", shape = 21, size = 2) +
  geom_line(liv_fa, 
            mapping = aes(x = Matches, y = xG, colour = "xG"), 
            size = 1) +
  geom_line(liv_fa, 
            mapping = aes(x = Matches, y = xGAgainst, colour = "xGA"), 
            size = 0.5, linetype = 2) +
  geom_vline(xintercept = c(7, 31), size = 0.8,
             col = "purple", linetype = 3) +
  scale_color_manual(name = "", 
                     values = c("xG" = "red",
                                "xGA" = "black",
                                "Liverpool vs City" = "purple")) +
  theme_fivethirtyeight() +
  theme(    
    legend.position = c(1, 1.2),
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "black"),
    plot.title = element_text(
      size = 18, face = "bold", colour = "brown"
    )) +
  ggtitle("Liverpool - 2021/2022") + ylim(0,6)


# Manchester City
city = ggplot() +
  geom_point(mci_fa, 
             mapping = aes(x = Matches, y = xG, color = "xG"), 
             color = "lightblue", fill = "lightblue", 
             shape = 21, size = 2) +
  geom_line(mci_fa, 
            mapping = aes(x = Matches, y = xG, colour = "xG"), size = 1) +
  geom_line(mci_fa, 
            mapping = aes(x = Matches, y = xGAgainst, colour = "xGA"), 
            linetype = 2, size = 0.5) +
  scale_color_manual(name = "", 
                     values = c("xG" = "lightblue",
                                "xGA" = "black",
                                "Liverpool vs City" = "purple")) +
  geom_vline(xintercept = c(7, 31), size = 0.8,
             col = "purple", linetype = 3) +
  theme_fivethirtyeight() +
  theme(    
    legend.position = c(1, 1.2),
    legend.justification = c(1, 1),
    legend.background = element_rect(colour = "black"),
    plot.title = element_text(
      size = 18, face = "bold", colour = "darkblue"
    ), 
    axis.title.x = element_text("Matches")) +
  ggtitle("Manchester City - 2021/2022") + ylim(0,6)


grid.arrange(liverpool, city)

# Liverpool Total
sum(liv_fa$xG)
sum(liv_fa$xGAgainst)

# Manchester City Total
sum(mci_fa$xG)
sum(mci_fa$xGAgainst)
