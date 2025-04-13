

install.packages(c("dplyr", "ggplot2", "ggrepel", "tidyr","forcats","knitr"))

library(dplyr)
library(ggplot2)
library(ggrepel) 
library(tidyr)
library(forcats)
library(knitr)

mydata<-read.csv("England_csv.csv")
head(mydata,10)
#getting a table view of the dataset
View(mydata)

#checking the dimesions of our data
dim(mydata)


#checking the datatypes of the variables
str(mydata)

colnames(mydata)

#checking the statistical summary of each column
summary(mydata)



mydata$Date<-as.Date(mydata$Date,format="%d/%m/%Y")

str(mydata)

#checking the na values in columns
colSums(is.na(mydata))


#filling na values of integer with 0
mydata$HTH.Goals[is.na(mydata$HTH.Goals)]<-0
mydata$HTA.Goals[is.na(mydata$HTA.Goals)]<-0
mydata$H.Shots[is.na(mydata$H.Shots)]<-0
mydata$A.Shots[is.na(mydata$A.Shots)]<-0
mydata$H.Shots[is.na(mydata$H.Shots)]<-0
mydata$H.SOT[is.na(mydata$H.SOT)]<-0
mydata$A.SOT[is.na(mydata$A.SOT)]<-0
mydata$H.Fouls[is.na(mydata$H.Fouls)]<-0
mydata$A.Fouls[is.na(mydata$A.Fouls)]<-0
mydata$H.Yellow[is.na(mydata$H.Yellow)]<-0
mydata$A.Yellow[is.na(mydata$A.Yellow)]<-0
mydata$H.Red[is.na(mydata$H.Red)]<-0
mydata$A.Red[is.na(mydata$A.Red)]<-0
mydata$H.Corners[is.na(mydata$H.Corners)]<-0
mydata$A.Corners[is.na(mydata$A.Corner)]<-0


colSums(is.na(mydata))

#which team played the most home matches 
home<-mydata %>% count(HomeTeam,name="HomeMatches") %>% arrange(desc(HomeMatches))
home
ggplot(home, aes(x = reorder(HomeTeam, HomeMatches), y = HomeMatches)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  coord_flip()+
  labs(title = "Number of Matches Played by Home Teams",
       x = "Home Team",
       y = "Number of Matches") 

#which team played most away matches
away<-mydata %>% count(AwayTeam,name="AwayMatches") %>% arrange(desc(AwayMatches))
away
ggplot(away, aes(x = reorder(AwayTeam, AwayMatches), y = AwayMatches)) + 
  geom_bar(stat = "identity", fill = "orange") + 
  coord_flip()+
  labs(title = "Number of Matches Played by Away Teams",
       x = "Home Team",
       y = "Number of Matches") 


#which team played the most matches"
totalmatches<-full_join(home,away,by=c("HomeTeam"="AwayTeam")) %>% mutate(TotalMatches=HomeMatches+AwayMatches) %>% arrange(desc(TotalMatches))
totalmatches
ggplot(totalmatches, aes(x = reorder(HomeTeam, TotalMatches), y = TotalMatches)) + 
  geom_bar(stat = "identity", fill = "purple") + 
  coord_flip()+
  labs(title = "Number of Matches Played by Teams",
       x = "Teams",
       y = "Number of Matches") 


colnames(mydata)

#finding the goal analysis of teams 
goal_analysis <- mydata %>%
  group_by(Team = HomeTeam) %>%
  summarise(
    HomeGoalsScored = sum(FTH.Goals, na.rm = TRUE),
    HomeGoalsConceded = sum(FTA.Goals, na.rm = TRUE),
    HomeGames = n()
  ) %>%
  left_join(
    mydata %>%
      group_by(Team = AwayTeam) %>%
      summarise(
        AwayGoalsScored = sum(FTA.Goals, na.rm = TRUE),
        AwayGoalsConceded = sum(FTH.Goals, na.rm = TRUE),
        AwayGames = n()
      ),
    by = "Team"
  ) %>%
  filter(HomeGames >= 100 & AwayGames >= 100) %>%
  mutate(
    GoalDifference_Home = HomeGoalsScored - HomeGoalsConceded,
    GoalDifference_Away = AwayGoalsScored - AwayGoalsConceded,
  )
View(goal_analysis)

goal_analysis<-goal_analysis%>%
  mutate(
    TotalGoals=HomeGoalsScored +AwayGoalsScored 
  )%>%
  arrange(desc(TotalGoals))
View(goal_analysis)



topscoringteams <- goal_analysis %>%
  slice_head(n = 10)
topscoringteams
#top 10 most scoring teams in the datasets time frame are Man United,Arsenal,Liverpool,chelsea, etc..

ggplot(topscoringteams, aes(x = reorder(Team, TotalGoals), y = TotalGoals)) +
  geom_bar(stat = "identity", fill = "#ff6361") +
  coord_flip() +
  labs(
    title = "Top 10 Most Scoring Teams",
    x = "Team",
    y = "Total Goals Scored"
  ) +
  theme_minimal()

#analysing the home performance of teams
home_performance <- mydata %>%
  group_by(HomeTeam) %>%
  summarise(
    Wins = sum(FT.Result == "H"),
    Draws = sum(FT.Result == "D"),
    Losses = sum(FT.Result == "A"),
    TotalMatches = n(),
    WinRate = Wins / TotalMatches*100
  ) %>%
  arrange(desc(WinRate)) %>%
  filter(TotalMatches >= 100)
home_performance
#teams like Man United,arsenal and liverpool are showing the highest win percentage in home fields

ggplot(home_performance, aes(x = reorder(HomeTeam,WinRate), y = WinRate)) + 
  geom_bar(stat = "identity", fill = "#63c78f") + 
  coord_flip()+
  labs(title = "Home win Rate of Teams",
       x = "Teams",
       y = "Win Rate %") 


#analysing the away performance of teams
away_perform<-mydata %>%
  group_by(AwayTeam)%>%
  summarise(
    Wins=sum(FT.Result=="A"),
    Draws=sum(FT.Result=="D"),
    Losses=sum(FT.Result=="H"),
    TotalMatches=n(),
    WinRate=Wins/TotalMatches*100
  )%>%filter(TotalMatches>=100)%>%
  arrange(desc(WinRate))
away_perform
#Man United,Chelsea and Arsenal are showing the highest win percentage in away fields

ggplot(away_perform, aes(x = reorder(AwayTeam,WinRate), y = WinRate)) + 
  geom_bar(stat = "identity", fill = "#63c78f") + 
  coord_flip()+
  labs(title = "Away win Rate of Teams",
       x = "Teams",
       y = "Win Rate %") 



#analysing the overall performance of teams
overall_analysis<-merge(home_performance,away_perform,by.x = "HomeTeam",by.y="AwayTeam")
overall_analysis
most_successfull <- overall_analysis %>%
  mutate(
    Team = HomeTeam,
    TotalWins = Wins.x + Wins.y,
    TotalMatches = TotalMatches.x + TotalMatches.y
  ) %>%
  select(Team, TotalWins, TotalMatches)
most_successfull
most_successfull<-most_successfull %>%
  mutate(
    WinRate=TotalWins/TotalMatches*100
  ) %>%arrange(desc(WinRate)) 
most_successfull
#the most successfull teams in the datasets time frame with highest win percentage are Man United,Arsenal,Liverpool etc


most_successfull_top10 <- most_successfull %>%slice_head(n = 10)

ggplot(most_successfull_top10, aes(x = reorder(Team, WinRate), y = WinRate)) +
  geom_bar(stat = "identity", fill = "#4e79a7") +
  coord_flip() +
  labs(
    title = "Top 10 Most Successful Teams by Win Rate",
    x = "Team",
    y = "Win Rate (%)"
  ) +
  theme_minimal()



better_performance<-select(overall_analysis,Team=HomeTeam,HomeWin=WinRate.x,AwayWin=WinRate.y)
better_performance

better_performance$homeAdv<-better_performance$HomeWin-better_performance$AwayWin
better_performance<-arrange(better_performance,desc(homeAdv))
better_performance
#teams Fulham,Stoke and NewCastle have a higher chance of winning home matches than away matches
#we can also see almost all teams have a greater chance of winning in Home than in Away  or we can say they find it comfortable


ggplot(better_performance, aes(x = reorder(Team,homeAdv), y = homeAdv)) + 
  geom_bar(stat = "identity", fill = "#9d4edd") + 
  coord_flip()+
  labs(title = "Teams with greater home advantage",
       x = "Teams",
       y = "Home Advantage") 




#average goals home vs away comparison
avg_goals <- c(mean(mydata$HTA.Goals, na.rm = TRUE), mean(mydata$FTA.Goals, na.rm = TRUE))
goal_labels <- c("Home Goals", "Away Goals")

barplot(avg_goals, names.arg = goal_labels, col = c("blue", "red"),
        main = "Average Home vs. Away Goals per Match",
        ylab = "Average Goals", ylim = c(0, max(avg_goals) + 0.5))



#card analysis of each team
#plotting is not correct
card_analysis_table <- function(mydata) {
  
  # Home stats
  home_cards <- mydata %>%
    group_by(Team = HomeTeam) %>%
    summarise(
      HomeYellowCards = mean(H.Yellow, na.rm = TRUE),
      HomeRedCards = mean(H.Red, na.rm = TRUE)
    )
  
  # Away stats
  away_cards <- mydata %>%
    group_by(Team = AwayTeam) %>%
    summarise(
      AwayYellowCards = mean(A.Yellow, na.rm = TRUE),
      AwayRedCards = mean(A.Red, na.rm = TRUE)
    )
  
  # Overall (home + away combined) stats
  overall_cards <- mydata %>%
    select(HomeTeam, AwayTeam, H.Yellow, A.Yellow, H.Red, A.Red) %>%
    mutate(
      HomeYellow = H.Yellow,
      AwayYellow = A.Yellow,
      HomeRed = H.Red,
      AwayRed = A.Red
    ) %>%
    pivot_longer(cols = c(HomeTeam, AwayTeam), names_to = "Type", values_to = "Team") %>%
    mutate(
      YellowCards = ifelse(Type == "HomeTeam", HomeYellow, AwayYellow),
      RedCards = ifelse(Type == "HomeTeam", HomeRed, AwayRed)
    ) %>%
    group_by(Team) %>%
    summarise(
      AvgYellowCards = mean(YellowCards, na.rm = TRUE),
      AvgRedCards = mean(RedCards, na.rm = TRUE)
    )
  
  # Merge all together
  card_summary <- full_join(home_cards, away_cards, by = "Team") %>%
    left_join(overall_cards, by = "Team") %>%
    relocate(Team)
  
  # Show result
  cat("\nAverage Yellow and Red Cards Received by EPL Teams (Home, Away & Overall):\n")
  print(kable(card_summary, digits = 2, caption = "Card Summary by Team (Home, Away & Overall Averages)"))
  
  return(card_summary)
}
View(card_analysis_table(mydata))


dirty_play_plot <- function(card_summary) {
  # Calculate Dirty Play Score
  card_summary <- card_summary %>%
    mutate(DirtyPlayScore = AvgYellowCards + 2 * AvgRedCards) %>%
    arrange(desc(DirtyPlayScore))
  
  # Select top 10 dirtiest teams
  top_10_dirty_teams <- card_summary %>%
    head(10)
  
  # Plot the top 10 dirtiest teams
  ggplot(top_10_dirty_teams, aes(x = reorder(Team, DirtyPlayScore), y = DirtyPlayScore, fill = Team)) +
    geom_bar(stat = "identity") +
    coord_flip() +  # Flip coordinates for better readability
    labs(
      title = "Top 10 Dirtiest Teams Based on Dirty Play Score",
      x = "Team",
      y = "Dirty Play Score"
    ) +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 10)) +
    scale_fill_brewer(palette = "Set3")
}
dirty_play_plot(card_analysis_table(mydata))



#referee card analysis
unique(mydata$Referee)
referee_card_analysis <- function(mydata) {
  
  referee_summary <- mydata %>%
    group_by(Referee) %>%
    summarise(
      AvgYellowPerMatch = mean(H.Yellow+A.Yellow, na.rm = TRUE),
      AvgRedPerMatch = mean(H.Red+A.Red, na.rm = TRUE),
      Matches = n()
    )%>%  mutate(
      StrictnessScore = AvgYellowPerMatch + 2 * AvgRedPerMatch
    ) %>%
    arrange(desc(StrictnessScore))
  
  cat("\nAverage Yellow and Red Cards Given by Each Referee:\n")
  print(referee_summary, n = Inf)
  return(referee_summary)
}

referee_card_analysis(mydata)
referee<-referee_card_analysis(mydata)
#top 10 strict referees
referee<-referee%>%slice_head(n=10)
ggplot(referee, aes(x = reorder(Referee,StrictnessScore ), y = StrictnessScore)) +
  geom_bar(stat = "identity", fill = "#ff6361") +
  coord_flip() +
  labs(
    title = "Top 10 strict referees",
    x = "Referee",
    y = "Strictness score"
  ) +
  theme_minimal()


#analysing the comeback chances of teams
comeback_count <- function(mydata) {
  
  comeback_summary <- mydata %>%
    filter((HT.Result == "A" & FT.Result == "H") | (HT.Result == "H" & FT.Result == "A")) %>%
    mutate(
      WinningTeam = ifelse(FT.Result == "H", HomeTeam, AwayTeam)
    ) %>%
    group_by(WinningTeam) %>%
    summarise(
      ComebackWins = n()
    ) %>%
    arrange(desc(ComebackWins))
  return(comeback_summary)
}
comeback<-comeback_count(mydata)
comeback
ggplot(comeback, aes(x = reorder(WinningTeam,ComebackWins), y = ComebackWins)) + 
  geom_bar(stat = "identity", fill = "#4A90E2") + 
  coord_flip()+
  labs(title = "Teams with highest comebacks",
       x = "Teams",
       y = "Comebacks") 
#teams like manchester united,tottenham and arsenal have shown the most comebacks



foul_win_analysis <- function(mydata) {
  
  foul_summary <- mydata %>%
    mutate(
      WinningTeam = ifelse(FT.Result == "H", HomeTeam, AwayTeam),
      LosingTeam = ifelse(FT.Result == "H", AwayTeam, HomeTeam),
      WinningFouls = ifelse(FT.Result == "H", H.Fouls, A.Fouls),
      LosingFouls = ifelse(FT.Result == "H", A.Fouls, H.Fouls),
      MoreFouls = ifelse(WinningFouls > LosingFouls, "More Fouls", "Less Fouls")
    ) %>%
    group_by(WinningTeam, MoreFouls) %>%
    summarise(
      Count = n()
    ) %>%
    pivot_wider(names_from = MoreFouls, values_from = Count, values_fill = 0) %>%
    arrange(desc(`More Fouls` + `Less Fouls`))
  
  cat("\nFoul Analysis: Do Winning Teams Commit More or Fewer Fouls?\n")
  kable(foul_summary, digits = 0, caption = "Winning Teams with More or Fewer Fouls")
}

foul_win_analysis(mydata)


#seasonal goal trends of different teams
selected_teams <- c("Arsenal", "Chelsea", "Liverpool", "Man United", "Man City", "Tottenham")

seasonal_performance <- mydata %>%
  mutate(SeasonStart = as.numeric(substr(Season, 1, 4))) %>%
  filter(HomeTeam %in% selected_teams | AwayTeam %in% selected_teams) %>%
  mutate(
    Team = case_when(
      HomeTeam %in% selected_teams ~ HomeTeam,
      AwayTeam %in% selected_teams ~ AwayTeam,
      TRUE ~ NA_character_
    ),
    Points = case_when(
      FT.Result == "H" & Team == HomeTeam ~ 3,
      FT.Result == "A" & Team == AwayTeam ~ 3,
      FT.Result == "D" ~ 1,
      TRUE ~ 0
    ),
    GoalDifference = ifelse(Team == HomeTeam, 
                            FTH.Goals - FTA.Goals, 
                            FTA.Goals - FTH.Goals)
  ) %>%
  group_by(Team, SeasonStart) %>%
  summarise(
    AvgPoints = mean(Points),
    AvgGoalDiff = mean(GoalDifference),
    TotalWins = sum(Points == 3),
    Matches = n()
  ) %>%
  filter(Matches >= 30) 
seasonal_performance

#seasonal goal difference trends
ggplot(seasonal_performance, aes(x = SeasonStart, y = AvgGoalDiff)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(aes(color = Team), linewidth = 1) +
  geom_point(aes(color = Team), size = 2) +
  facet_wrap(~Team, ncol = 2) +
  labs(
    title = "Seasonal Goal Difference Trends",
    x = "Season Start Year",
    y = "Average Goal Difference per Match"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

#win percentage trends across seasons