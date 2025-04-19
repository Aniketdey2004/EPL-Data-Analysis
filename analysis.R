

install.packages(c("dplyr", "ggplot2", "ggrepel", "tidyr","forcats","knitr","zoo","corrplot","pactchwork"))

library(patchwork)
library(dplyr)
library(ggplot2)
library(ggrepel) 
library(tidyr)
library(forcats)
library(knitr)
library(zoo)
library(corrplot)

mydata<-read.csv("England_CSV.csv")

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

mydata2<-mydata
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
findtotalmatches<-function(home,away){
  totalmatches<-full_join(home,away,by=c("HomeTeam"="AwayTeam")) %>% mutate(TotalMatches=HomeMatches+AwayMatches) %>% arrange(desc(TotalMatches))
  totalmatches
  print(ggplot(totalmatches, aes(x = reorder(HomeTeam, TotalMatches), y = TotalMatches)) + 
    geom_bar(stat = "identity", fill = "purple") + 
    coord_flip()+
    labs(title = "Number of Matches Played by Teams",
         x = "Teams",
         y = "Number of Matches") )
  return(totalmatches)
  
}
total_matches<-findtotalmatches(home,away)
colnames(mydata)

#finding the goal analysis of teams 
findteamgoalanalysis<-function(mydata){
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
  return(goal_analysis)
}
teamspecificgoalanalysis<-findteamgoalanalysis(mydata)


findtopscoringteams<-function(goal_analysis){
  goal_analysis<-goal_analysis%>%
    mutate(
      TotalGoals=HomeGoalsScored +AwayGoalsScored 
    )%>%
    arrange(desc(TotalGoals))
  View(goal_analysis)
  topscoringteams <- goal_analysis %>%
    slice_head(n = 10)
  topscoringteams
  
  
  print(ggplot(topscoringteams, aes(x = reorder(Team, TotalGoals), y = TotalGoals)) +
    geom_bar(stat = "identity", fill = "#ff6361") +
    coord_flip() +
    labs(
      title = "Top 10 Most Scoring Teams",
      x = "Team",
      y = "Total Goals Scored"
    ) +
    theme_minimal())
  return(topscoringteams)
}

topscoringteams<-findtopscoringteams(teamspecificgoalanalysis)
#top 10 most scoring teams in the datasets time frame are Man United,Arsenal,Liverpool,chelsea, etc..

#analysing the home performance of teams
homeperformanceanalysis <- function(mydata) {
  home_performance <- mydata %>%
    group_by(HomeTeam) %>%
    summarise(
      Wins = sum(FT.Result == "H"),
      Draws = sum(FT.Result == "D"),
      Losses = sum(FT.Result == "A"),
      TotalMatches = n(),
      WinRate = Wins / TotalMatches * 100
    ) %>%
    arrange(desc(WinRate)) %>%
    filter(TotalMatches >= 100)
  
  print(
    ggplot(home_performance, aes(x = reorder(HomeTeam, WinRate), y = WinRate)) +
      geom_bar(stat = "identity", fill = "#63c78f") +
      coord_flip() +
      labs(title = "Home Win Rate of Teams",
           x = "Teams",
           y = "Win Rate %")
  )
  
  return(home_performance)
}

home_performance<-homeperformanceanalysis(mydata)
#teams like Man United,arsenal and liverpool are showing the highest win percentage in home fields



#analysing the away performance of teams
awayperformanceanalysis <- function(mydata) {
  away_perform <- mydata %>%
    group_by(AwayTeam) %>%
    summarise(
      Wins = sum(FT.Result == "A"),
      Draws = sum(FT.Result == "D"),
      Losses = sum(FT.Result == "H"),
      TotalMatches = n(),
      WinRate = Wins / TotalMatches * 100
    ) %>%
    filter(TotalMatches >= 100) %>%
    arrange(desc(WinRate))
  
  print(
    ggplot(away_perform, aes(x = reorder(AwayTeam, WinRate), y = WinRate)) +
      geom_bar(stat = "identity", fill = "#63c78f") +
      coord_flip() +
      labs(title = "Away Win Rate of Teams",
           x = "Teams",
           y = "Win Rate %")
  )
  
  return(away_perform)
}
away_perform<-awayperformanceanalysis(mydata)
#Man United,Chelsea and Arsenal are showing the highest win percentage in away fields


#analysing the overall performance of teams
overall_analysis<-merge(home_performance,away_perform,by.x = "HomeTeam",by.y="AwayTeam")
overall_analysis


topsuccessfullteams<-function(overall_analysis){
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
  
  
  
  most_successfull_top10 <- most_successfull %>%slice_head(n = 10)
  
  print(ggplot(most_successfull_top10, aes(x = reorder(Team, WinRate), y = WinRate)) +
    geom_bar(stat = "identity", fill = "#4e79a7") +
    coord_flip() +
    labs(
      title = "Top 10 Most Successful Teams by Win Rate",
      x = "Team",
      y = "Win Rate (%)"
    ) +
    theme_minimal())
  return(most_successfull)
}

topsuccess<-topsuccessfullteams(overall_analysis)
#the most successfull teams in the datasets time frame with highest win percentage are Man United,Arsenal,Liverpool etc

homeadvantageanalysis<-function(overall_analysis){
  better_performance<-select(overall_analysis,Team=HomeTeam,HomeWin=WinRate.x,AwayWin=WinRate.y)
  better_performance$homeAdv<-better_performance$HomeWin-better_performance$AwayWin
  better_performance<-arrange(better_performance,desc(homeAdv))
  better_performance
  print(ggplot(better_performance, aes(x = reorder(Team,homeAdv), y = homeAdv)) + 
          geom_bar(stat = "identity", fill = "#9d4edd") + 
          coord_flip()+
          labs(title = "Teams with greater home advantage",
               x = "Teams",
               y = "Home Advantage") )
  return(better_performance)
}
homeadvantageteams<-homeadvantageanalysis(overall_analysis)
#teams Fulham,Stoke and NewCastle have a higher chance of winning home matches than away matches
#we can also see almost all teams have a greater chance of winning in Home than in Away  or we can say they find it comfortable




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
      AvgYellowPerMatch = mean(H.Yellow + A.Yellow, na.rm = TRUE),
      AvgRedPerMatch = mean(H.Red + A.Red, na.rm = TRUE),
      Matches = n()
    ) %>%
    filter(Matches > 100) %>%  # 🔍 Filter referees with more than 100 matches
    mutate(
      StrictnessScore = AvgYellowPerMatch + 2 * AvgRedPerMatch
    ) %>%
    arrange(desc(StrictnessScore))
  
  cat("\nAverage Yellow and Red Cards Given by Each Referee (more than 100 matches):\n")
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
selected_teams <- c("Arsenal", "Chelsea", "Liverpool", "Man United", "Man City", "Tottenham","Newcastle","Everton","Aston Villa","West Ham")

#shows how top 10 teams have dominated over the years
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
    AvgGoalDiff = abs(mean(GoalDifference)),
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

team_performance <- function(team_name, data = mydata) {
  # Check if the team exists in the dataset
  all_teams <- unique(c(data$HomeTeam, data$AwayTeam))
  if (!(team_name %in% all_teams)) {
    stop(paste("Team not found. Available teams:",
               paste(unique(all_teams), collapse = ", ")))
  }
  
  # Define team color palette (fallback to black for unknown teams)
  team_colors <- c(
    "Arsenal" = "#EF0107",
    "Chelsea" = "#034694",
    "Liverpool" = "#C8102E",
    "Man United" = "#DA291C",
    "Man City" = "#6CABDD",
    "Tottenham" = "#132257",
    "default" = "black"
  )
  team_color <- team_colors[team_name]
  if (is.na(team_color)) team_color <- team_colors["default"]
  
  # Process data specifically for the selected team
  team_data <- data %>%
    mutate(SeasonStart = as.numeric(substr(Season, 1, 4))) %>%
    filter(HomeTeam == team_name | AwayTeam == team_name) %>%
    mutate(
      Team = team_name,
      Points = case_when(
        FT.Result == "H" & HomeTeam == team_name ~ 3,
        FT.Result == "A" & AwayTeam == team_name ~ 3,
        FT.Result == "D" ~ 1,
        TRUE ~ 0
      ),
      GoalDifference = ifelse(HomeTeam == team_name,
                              FTH.Goals - FTA.Goals,
                              FTA.Goals - FTH.Goals)
    ) %>%
    group_by(SeasonStart) %>%
    summarise(
      AvgPoints = mean(Points),
      AvgGoalDiff = abs(mean(GoalDifference)),
      Matches = n(),
      .groups = "drop"
    ) %>%
    filter(Matches >= 30)
  
  # Plot the performance
  ggplot(team_data, aes(x = SeasonStart)) +
    geom_area(aes(y = AvgGoalDiff),
              fill = team_color,
              alpha = 0.3) +
    geom_line(aes(y = AvgPoints * 2),
              color = team_color,
              linewidth = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_hline(yintercept = 2 * 1.8, linetype = "dashed", color = "gold") +
    scale_y_continuous(
      name = "Average Goal Difference",
      sec.axis = sec_axis(~./2, name = "Average Points per Match")
    ) +
    labs(
      title = paste(team_name, "Performance Over Time"),
      subtitle = "Goal Difference (area) vs. Points per Match (line)",
      x = "Season Start Year",
      caption = paste("Data from", min(team_data$SeasonStart), "-", max(team_data$SeasonStart))
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, color = team_color),
      axis.title.y.right = element_text(color = team_color),
      axis.text.y.right = element_text(color = team_color)
    )
}

team_performance("Arsenal",mydata)
team_performance("Man City",mydata)
team_performance("Chelsea",mydata)

# Assuming your `mydata` data frame is already loaded and cleaned

analyze_team_performance_over_time <- function(data, team_name, window_size = 10) {
  
  team_data<-data%>%
    filter(HomeTeam==team_name | AwayTeam==team_name)%>%
    arrange(Date)%>%
    mutate(
      SeasonStart=as.numeric(substr(Season,1,4)),
      IsHome=ifelse(HomeTeam==team_name,TRUE,FALSE),
      GoalsScored=ifelse(IsHome==TRUE,FTH.Goals,FTA.Goals),
      GoalsConceded=ifelse(IsHome==TRUE,FTA.Goals,FTH.Goals),
      Result=case_when(
        (IsHome & FT.Result=="H")|(!IsHome & FT.Result=="A")~"Win",
        FT.Result=="D"~"Draw",
        TRUE~"Loss"
      )
    )
  team_data<-team_data%>%
    mutate(
      RollingGoalsScored=rollmean(GoalsScored,k=window_size,fill=NA,align="right"),
      RollingGoalsConceded=rollmean(GoalsConceded,k=window_size,fill=NA,align="right"),
      RollingWins=rollsum(ifelse(Result=="Win",1,0),k=window_size,fill=NA,align="right"),
      RollingLosses=rollsum(ifelse(Result=="Loss",1,0),k=window_size,fill=NA,align="right")
    )
  
  plot_data <- team_data %>%
    select(Date, SeasonStart, RollingGoalsScored, RollingGoalsConceded, RollingWins, RollingLosses) %>%
    pivot_longer(cols = starts_with("Rolling"), names_to = "Metric", values_to = "Value")

  
  ggplot(plot_data, aes(x = Date, y = Value, color = Metric)) +
    geom_line(linewidth = 1) +
    facet_wrap(~ Metric, scales = "free_y", ncol = 2) +
    labs(
      title = paste(team_name, "Rolling Performance Over Time (Window Size:", window_size, "Games)"),
      x = "Date",
      y = "Rolling Average",
      color = "Metric"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}


analyze_team_performance_over_time(mydata, "Man United", window_size = 30)
analyze_team_performance_over_time(mydata, "Arsenal", window_size = 30)
analyze_team_performance_over_time(mydata,"Chelsea",window_size = 30)

#exploring the variation in each numerical variables

#checking the distribution of full time home goals
ggplot(mydata2, aes(x = FTH.Goals)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Full Time Home Goals",
       x = "Full Time Home Goals",
       y = "Frequency") +
  theme_minimal()

ggplot(mydata2, aes(y = FTH.Goals)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Box Plot of Full Time Home Goals",
       y = "Full Time Home Goals") +
  theme_minimal()

ggplot(mydata2, aes(x = FTH.Goals)) +
  geom_density(fill = "purple", alpha = 0.7) +
  labs(title = "Density Plot of Full Time Home Goals",
       x = "Full Time Home Goals",
       y = "Density") +
  theme_minimal()
#The average home team scores between 0–2 goals most of the time.
#Higher goal counts are rare but do occur (long tail).
#The data is discrete, positively skewed, and has outliers on the higher side.

#checking the distribution of full time away goals
ggplot(mydata2, aes(x = FTA.Goals)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Full Time Away Goals",
       x = "Full Time Away Goals",
       y = "Frequency") +
  theme_minimal()

ggplot(mydata2, aes(y = FTA.Goals)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Box Plot of Full Time Away Goals",
       y = "Full Time Away Goals") +
  theme_minimal() 

ggplot(mydata2, aes(x = FTA.Goals)) +
  geom_density(fill = "purple", alpha = 0.7) +
  labs(title = "Density Plot of Full Time Away Goals",
       x = "Full Time Away Goals",
       y = "Density") +
  theme_minimal()
#The average away team scores between 0–2 goals most of the time.
#Higher goal counts are rare but do occur (long tail).
#The data is discrete, positively skewed, and has outliers on the higher side.

#checking the distribution of Home shots
ggplot(mydata2, aes(x = H.Shots)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Home Shots",
       x = "Home Shots",
       y = "Frequency") +
  theme_minimal()

ggplot(mydata2, aes(y = H.Shots)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Box Plot of Home Shots",
       y = "Home Shots") +
  theme_minimal() +
  coord_flip()

ggplot(mydata2, aes(x = H.Shots)) +
  geom_density(fill = "purple", alpha = 0.7) +
  labs(title = "Density Plot of Home Shots",
       x = "Home Shots",
       y = "Density") +
  theme_minimal()
#Most home teams shoot around 10–14 times per match.
#Few teams take 20+ shots, and those are uncommon or extreme matches.
#The data is not normally distributed – it is skewed to the right.
#Outliers (such as 30–40 shots) might warrant further investigation — maybe those were one-sided dominant performances

ggplot(mydata2, aes(x = H.SOT)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Home Shots on Target",
       x = "Home Shots on Target",
       y = "Frequency") +
  theme_minimal()

ggplot(mydata2, aes(y = H.SOT)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Box Plot of Home Shots on Target",
       y = "Home Shots on Target") +
  theme_minimal() +
  coord_flip()

ggplot(mydata2, aes(x = H.SOT)) +
  geom_density(fill = "purple", alpha = 0.7) +
  labs(title = "Density Plot of Home Shots on Target",
       x = "Home Shots on Target",
       y = "Density") +
  theme_minimal()
#most home teams shots on target are 3-7
#the data is positively skewed and not symmetrical
#the outliers are between(15-25) shows domination in attack by home teams



#checking the distribution of away shots
ggplot(mydata2, aes(x = A.Shots)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Away Shots",
       x = "Away Shots",
       y = "Frequency") +
  theme_minimal()

ggplot(mydata2, aes(y = A.Shots)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Box Plot of Away Shots",
       y = "Away Shots") +
  theme_minimal() +
  coord_flip()

ggplot(mydata2, aes(x = A.Shots)) +
  geom_density(fill = "purple", alpha = 0.7) +
  labs(title = "Density Plot of Away Shots",
       x = "Away Shots",
       y = "Density") +
  theme_minimal()
#Most away teams shoot around 7–11 times per match.
#Few teams take 20+ shots, and those are uncommon or extreme matches.
#The data is not normally distributed – it is skewed to the right.
#Outliers (such as 25-30 shots) might warrant further investigation — maybe those were one-sided dominant performances.

#checking the distribution of away shots on target
ggplot(mydata2, aes(x = A.SOT)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Away Shots on Target",
       x = "Away Shots on Target",
       y = "Frequency") +
  theme_minimal()

ggplot(mydata2, aes(y = A.SOT)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Box Plot of Away Shots on Target",
       y = "Away Shots on Target") +
  theme_minimal() +
  coord_flip()

ggplot(mydata2, aes(x = A.SOT)) +
  geom_density(fill = "purple", alpha = 0.7) +
  labs(title = "Density Plot of Away Shots on Target",
       x = "Away Shots on Target",
       y = "Density") +
  theme_minimal()
#most away teams have 2-5 shots on target
#the data is positively skewed and not symmetrical
#the outliers are present between (10-20) shows matches with domination of away teams



#checking the distribution of home fouls
ggplot(mydata2, aes(x = H.Fouls)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Home Fouls",
       x = "Home Fouls",
       y = "Frequency") +
  theme_minimal()

ggplot(mydata2, aes(y = H.Fouls)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Box Plot of Home Fouls",
       y = "Home Fouls") +
  theme_minimal() +
  coord_flip()

ggplot(mydata2, aes(x = H.Fouls)) +
  geom_density(fill = "purple", alpha = 0.7) +
  labs(title = "Density Plot of Home Fouls",
       x = "Home Fouls",
       y = "Density") +
  theme_minimal()
#most home teams commit 9-13 fouls in general
#the distribution is slightly positively skewed and mostly normal
#there are outliers on both sides 
#the outliers on left indicates clean play by home teams
#the outliers on right indicates dirty play by home teams we have to see when this happens
#this may be due to more goals conceded so under pressure they play dirty to prevent further goals

#checking the distribution of home yellow cards
ggplot(mydata2, aes(x = H.Yellow)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Home Yellow Cards",
       x = "Home Yellow Cards",
       y = "Frequency") +
  theme_minimal()

ggplot(mydata2, aes(y = H.Yellow)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Box Plot of Home Yellow Cards",
       y = "Home Yellow Cards") +
  theme_minimal() +
  coord_flip()

ggplot(mydata2, aes(x = H.Yellow)) +
  geom_density(fill = "purple", alpha = 0.7) +
  labs(title = "Density Plot of Home Yellow Cards",
       x = "Home Yellow Cards",
       y = "Density") +
  theme_minimal()
#most home teams get around 0-2 yellow cards
#the distribution is positively skewed
#there are outliers above 4 indicates foul play by home teams

#checking the distribution of home red cards
ggplot(mydata2, aes(x = H.Red)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Home Red Cards",
       x = "Home Red Cards",
       y = "Frequency") +
  theme_minimal()

ggplot(mydata2, aes(y = H.Red)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Box Plot of Home Red Cards",
       y = "Home Red Cards") +
  theme_minimal() +
  coord_flip()

ggplot(mydata2, aes(x = H.Red)) +
  geom_density(fill = "purple", alpha = 0.7) +
  labs(title = "Density Plot of Home Red Cards",
       x = "Home Red Cards",
       y = "Density") +
  theme_minimal()
#most home teams get no red cards but there are instances when the get red cards and sometimes even 2
#the distrbution is not symmetrical
#the outliers indicates extreme foul play by home teams


#checking the distribution of away fouls
ggplot(mydata2, aes(x = A.Fouls)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Away Fouls",
       x = "Away Fouls",
       y = "Frequency") +
  theme_minimal()

ggplot(mydata2, aes(y = A.Fouls)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Box Plot of Away Fouls",
       y = "Away Fouls") +
  theme_minimal() +
  coord_flip()

ggplot(mydata2, aes(x = A.Fouls)) +
  geom_density(fill = "purple", alpha = 0.7) +
  labs(title = "Density Plot of Away Fouls",
       x = "Away Fouls",
       y = "Density") +
  theme_minimal()
#most away teams in general score between 9-13 fouls only
#the distribution is mostly normal but slightly positively skewed
#there are outliers on both sides
#the outliers between 20-30 indicates dirty play by away teams

#we can see both side teams on average commit same no. of fouls
#but there are most instances of clean play by home teams than away teams this may be due to less pressure as they are playing in home ground

ggplot(mydata2, aes(x = A.Yellow)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Away Yellow Cards",
       x = "Away Yellow Cards",
       y = "Frequency") +
  theme_minimal()

ggplot(mydata2, aes(y = A.Yellow)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Box Plot of Away Yellow Cards",
       y = "Away Yellow Cards") +
  theme_minimal() +
  coord_flip()

ggplot(mydata2, aes(x = A.Yellow)) +
  geom_density(fill = "purple", alpha = 0.7) +
  labs(title = "Density Plot of Away Yellow Cards",
       x = "Away Yellow Cards",
       y = "Density") +
  theme_minimal()
#most away teams gets around 1-2 yellow cards 
#there are outliers above 6 indicates foul play by away teams 
#the distribution is positively skewed

#checking the distribution of away red cards
ggplot(mydata2, aes(x = A.Red)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Away Red Cards",
       x = "Away Red Cards",
       y = "Frequency") +
  theme_minimal()

ggplot(mydata2, aes(y = A.Red)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Box Plot of Away Red Cards",
       y = "Away Red Cards") +
  theme_minimal() +
  coord_flip()

ggplot(mydata2, aes(x = A.Red)) +
  geom_density(fill = "purple", alpha = 0.7) +
  labs(title = "Density Plot of Away Red Cards",
       x = "Away Red Cards",
       y = "Density") +
  theme_minimal()
#aways teams mostly get no cards but in some instances get 1 red cards and at max 2
#there are less outliers above 1
#the distribution is positively skewd and not symmetrical

#the fouls scored by away teams is more than home teams in general but the no. of red cards of away teams is less than that of home teams and extreme foul play is by home teams this may be due to respect pressure in case of loosing in home grounds

#checking the distribution of home corners 
ggplot(mydata2, aes(x = H.Corners)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Home Corners",
       x = "Home Corners",
       y = "Frequency") +
  theme_minimal()

ggplot(mydata2, aes(y = H.Corners)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Box Plot of Home Corners",
       y = "Home Corners") +
  theme_minimal() +
  coord_flip()

ggplot(mydata2, aes(x = H.Corners)) +
  geom_density(fill = "purple", alpha = 0.7) +
  labs(title = "Density Plot of Home Corners",
       x = "Home Corners",
       y = "Density") +
  theme_minimal()
#the home team most of the times get 4-7 corners
#the distribution is positively skewed
#there are few instances of home teams getting corners above 15 this may be due to dominance by home teams

#checking the distribution of away corners
ggplot(mydata2, aes(x = A.Corners)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Away Corners",
       x = "Away Corners",
       y = "Frequency") +
  theme_minimal()

ggplot(mydata2, aes(y = A.Corners)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Box Plot of Away Corners",
       y = "Away Corners") +
  theme_minimal() +
  coord_flip()

ggplot(mydata2, aes(x = A.Corners)) +
  geom_density(fill = "purple", alpha = 0.7) +
  labs(title = "Density Plot of Away Corners",
       x = "Away Corners",
       y = "Density") +
  theme_minimal()
#most of the times away teams gets 3-5 corners
#the distribution is positively skewed 
#there are many outliers above 10 this indicates instances of dominance of away teams against home teams
#as we can see away teams get less corners in comparison to home teams this may be due to less attack of away teams or good defense by home teams

#function to see the distribution of  a variable of choice

plot_var_dist <- function(data, var_name) {
  df <- data.frame(x = data[[var_name]])
  
  p1 <- ggplot(df, aes(x = x)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
    labs(title = paste("Histogram of", var_name),
         x = var_name, y = "Frequency") +
    theme_minimal()
  
  p2 <- ggplot(df, aes(y = x)) +
    geom_boxplot(fill = "orange", color = "black") +
    labs(title = paste("Box Plot of", var_name),
         y = var_name) +
    theme_minimal() +
    coord_flip()
  
  p3 <- ggplot(df, aes(x = x)) +
    geom_density(fill = "purple", alpha = 0.7) +
    labs(title = paste("Density Plot of", var_name),
         x = var_name, y = "Density") +
    theme_minimal()
  
  # Arrange vertically
  p1 / p2 / p3
}
plot_var_dist(mydata2, "A.Corners")



#exploring the distribution of categorical variables
#exploring the duration of seasons
season_counts <- mydata %>%
  count(Season, name = "MatchCount") %>%
  arrange(desc(MatchCount))

ggplot(season_counts, aes(x = Season, y = MatchCount)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Number of Matches per Season",
       x = "Season",
       y = "Number of Matches") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

#1993/1994 and 1994/1995 seasons were the longest seasons and 2024/2025 is the shortest season, rest in all the seasons the no. of matches played were same



#exploring the distribution of different types of match outcomes
plot_ft_result_counts <- function(data) {
  ft_result_counts <- data %>%
    count(FT.Result, name = "ResultCount")
  
  ggplot(ft_result_counts, aes(x = FT.Result, y = ResultCount)) +
    geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
    labs(
      title = "Number of Matches per Full Time Result",
      x = "Full Time Result (H=Home Win, A=Away Win, D=Draw)",
      y = "Number of Matches"
    ) +
    theme_minimal()
}
plot_ft_result_counts(mydata)

#more number of matches resulted in home team winning


#checking the top 20 referees by number of matches
# Displaying the top 20 referees to avoid an overly long plot

plot_top_referees <- function(data, referee_col = "Referee", top_n = 20) {
  referee_counts <- data %>%
    count(.data[[referee_col]], name = "MatchCount") %>%
    arrange(desc(MatchCount)) %>%
    slice_head(n = top_n)
  
  ggplot(referee_counts, aes(x = reorder(.data[[referee_col]], MatchCount), y = MatchCount)) +
    geom_bar(stat = "identity", fill = "lightsalmon", color = "black") +
    coord_flip() +
    labs(title = paste("Top", top_n, "Referees by Number of Matches"),
         y = "Number of Matches",
         x = "Referee") +
    theme_minimal()
}
plot_top_referees(mydata, "Referee", 20)



#Bivariate Analysis (Exploring Relationships Between Two Variables)

# Numerical vs. Numerical:
colnames(mydata)
#relationship between full time home goals and full time away goals
ggplot(mydata,aes(x=FTH.Goals,y=FTA.Goals))+
  geom_point()+
  labs(
    title="FTH Goals vs FTA Goals",
    x="FTH Goals",
    y="FTA Goals"
  )+
  theme_minimal()
cor(mydata$FTH.Goals,mydata$FTA.Goals)
#they are showing a negative linear relationship means if away team scores more home team scores less

#relationship between FTH Goals and HTH Goals
#checking whether more half time home goals result in more full time home goals 
ggplot(mydata,aes(x=HTH.Goals,y=FTH.Goals))+
  geom_point()+
  labs(
    title="HTH Goals vs FTH Goals",
    x="HTH Goals",
    y="FTH Goals"
  )+
  theme_minimal()
cor(mydata$HTH.Goals,mydata$FTH.Goals)
#they are showing a strong positive linear relationship,means if by half time home team scores more goals then it will result in higher full time goals

#relationship between FTA Goals and HTA Goals
ggplot(mydata,aes(x=HTA.Goals,y=FTA.Goals))+
  geom_point()+
  labs(
    title="HTA Goals vs FTA Goals",
    x="HTA Goals",
    y="FTA Goals"
  )+
  theme_minimal()
cor(mydata$HTA.Goals,mydata$FTA.Goals)
#they are showing a strong positive linear relationship i.e more half time away goals result in more full time away goals


# Scatter Plot: Home Shots vs. Full Time Home Goals
ggplot(mydata, aes(x = H.Shots, y = FTH.Goals)) +
  geom_point() +
  labs(title = "Home Shots vs. Full Time Home Goals",
       x = "Home Shots",
       y = "Full Time Home Goals") +
  theme_minimal()
cor(mydata$H.Shots,mydata$FTH.Goals)
#they have a positive linear correlation means more shots result in more goals


# Scatter Plot: Home Shots on Target vs. Full Time Home Goals
ggplot(mydata, aes(x = H.SOT, y = FTH.Goals)) +
  geom_point() +
  labs(title = "Home Shots on Target vs. Full Time Home Goals",
       x = "Home Shots on Target",
       y = "Full Time Home Goals") +
  theme_minimal()
cor(mydata$H.SOT,mydata$FTH.Goals)
#they have a positive linear correlation we know from the previous relationship between H shots and FTH goals that more shots result in more goals but more shots are there on target can be better to tell that whether goal could be scored or not

#relationship between away shots and Full time away goals
ggplot(mydata, aes(x = A.Shots, y = FTA.Goals)) +
  geom_point() +
  labs(title = "Away Shots vs. Full Time Away Goals",
       x = "Away Shots",
       y = "Full Time Away Goals") +
  theme_minimal()
cor(mydata$A.Shots,mydata$FTA.Goals)
#they have a strong positive relationship it means more attempts to score result in more goals


#relationship between away shots on target and full time away goals
ggplot(mydata, aes(x = A.SOT, y = FTA.Goals)) +
  geom_point() +
  labs(title = "Away Shots on Target vs. Full Time Away Goals",
       x = "Away Shots on Target",
       y = "Full Time Away Goals") +
  theme_minimal()
cor(mydata$A.SOT,mydata$FTA.Goals)
#they are showing a more positive relationship it means greater the accuracy of shots more will be the chances of goals

#relationship between away fouls and home shots
#checking whether more fouls by away team result in less attack by home teams
ggplot(mydata, aes(x = A.Fouls, y = H.Shots)) +
  geom_point() +
  labs(title = "Away Fouls vs. Home shots",
       x = "Away Fouls",
       y = "Home shots") +
  theme_minimal()
cor(mydata$A.Fouls,mydata$H.Shots)

ggplot(mydata, aes(x = A.Fouls, y = H.SOT)) +
  geom_point() +
  labs(title = "Away Fouls vs. Home shots on target",
       x = "Away Fouls",
       y = "Home shots") +
  theme_minimal()
cor(mydata$A.Fouls,mydata$H.SOT)
#the answer is no, if away teams commit more fouls it does not inhibit the home teams from attacking

#relationship between away fouls and home shots
#checking whether more fouls by home team result in less attack by away teams
ggplot(mydata, aes(x = H.Fouls, y = A.Shots)) +
  geom_point() +
  labs(title = "Home Fouls vs. Away shots",
       x = "Home Fouls",
       y = "Away shots") +
  theme_minimal()
cor(mydata$H.Fouls,mydata$A.Shots)

ggplot(mydata, aes(x = H.Fouls, y = A.SOT)) +
  geom_point() +
  labs(title = "Home Fouls vs. Away shots",
       x = "Home Fouls",
       y = "Away shots") +
  theme_minimal()
cor(mydata$H.Fouls,mydata$A.SOT)
#the answer is no, if home teams commit more fouls it does not inhibit the away teams from attacking
colnames(mydata)

ggplot(mydata, aes(x = H.Corners, y = FTH.Goals)) +
  geom_point() +
  labs(title = "Home Corners vs. Full time home goals",
       x = "Home Corners",
       y = "Full Time home goals") +
  theme_minimal()
cor(mydata$H.Corners,mydata$FTH.Goals)
#they are having a positive linear relationship
ggplot(mydata, aes(x = A.Corners, y = FTA.Goals)) +
  geom_point() +
  labs(title = "Away corners vs. FTA.Goals",
       x = "Away Corners",
       y = "Full time away goals") +
  theme_minimal()
cor(mydata$A.Corners,mydata$FTA.Goals)
#they have a positive linear relationship

#function to compare two numerical variables
analyze_relationship <- function(data, var_x, var_y) {
  x <- data[[var_x]]
  y <- data[[var_y]]
  correlation <- cor(x, y, use = "complete.obs")
  cat("Correlation between", var_x, "and", var_y, "is:", round(correlation, 3), "\n")
  ggplot(data, aes_string(x = var_x, y = var_y)) +
    geom_point(color = "steelblue", alpha = 0.7) +
    labs(
      title = paste(var_x, "vs", var_y),
      x = var_x,
      y = var_y
    ) +
    theme_minimal()
}
analyze_relationship(mydata, "FTH.Goals", "FTA.Goals")
analyze_relationship(mydata, "H.SOT", "FTH.Goals")
analyze_relationship(mydata, "A.Fouls", "H.SOT")

# Correlation Matrix
numerical_data <- mydata[sapply(mydata, is.numeric)]
correlation_matrix <- cor(numerical_data)
print("Correlation Matrix of Numerical Variables:")
print(correlation_matrix)
corrplot(correlation_matrix, method = "number",type="upper")

# Numerical vs. Categorical:
ggplot(mydata, aes(x = FT.Result, y = FTH.Goals, fill = FT.Result)) +
  geom_boxplot() +
  labs(title = "FTH Goals by FT.Result", x = "FT.Result", y = "FTH Goals") +
  theme_minimal()
#when home team wins the median of FTH is higher in comparison to draws and losses and even less for losses than draws
#There is a clear positive relationship between home goals and home wins: When the home team scores more, they are more likely to win.
#Low-scoring matches from the home team are often associated with draws or losses.
#FTH.Goals is a strong predictor of the full-time result, especially for identifying home wins.

ggplot(mydata, aes(x = FT.Result, y = FTA.Goals, fill = FT.Result)) +
  geom_boxplot() +
  labs(title = "FTA Goals by FT.Result", x = "FT.Result", y = "FTA Goals") +
  theme_minimal()
#when away team wins the median of FTA is higher in comparison to draws and losses and even less for losses than draws
#There is a clear positive relationship between away goals and away wins: When the away team scores more, they are more likely to win.
#Low-scoring matches from the away team are often associated with draws or losses.
#FTA.Goals is a strong predictor of the full-time result, especially for identifying home wins.

ggplot(mydata, aes(x = FT.Result, y = H.Shots, fill = FT.Result)) +
  geom_boxplot() +
  labs(title = "Home Shots by FT.Result", x = "FT.Result", y = "Home Shots") +
  theme_minimal()
#when home team wins the median of home shots is higher then it is comparatively less for draws and even less for away
#H.Shots is a good predictor of home wins

ggplot(mydata, aes(x = FT.Result, y = A.Shots, fill = FT.Result)) +
  geom_boxplot() +
  labs(title = "Away Shots by FT.Result", x = "FT.Result", y = "Away Shots") +
  theme_minimal()
#when away team wins the median of away shots is higher then it is comparatively less for draws and even less for home
#A.Shots is a good predictor of away wins

ggplot(mydata, aes(x = FT.Result, y = H.SOT, fill = FT.Result)) +
  geom_boxplot() +
  labs(title = "Home Shots on Target by FT.Result", x = "FT.Result", y = "Home SOT") +
  theme_minimal()
#when home team wins they have more shots on target than when the result is draw or away win
#H.SOT is a strong predictor of home wins

ggplot(mydata, aes(x = FT.Result, y = A.SOT, fill = FT.Result)) +
  geom_boxplot() +
  labs(title = "Away Shots on Target by FT.Result", x = "FT.Result", y = "Away SOT") +
  theme_minimal()
#when away team wins they have more shots on target than when the result is draw or home win
#A.SOT is a strong predictor of away wins

ggplot(mydata, aes(x = FT.Result, y = H.Fouls, fill = FT.Result)) +
  geom_boxplot() +
  labs(title = "Home Fouls by FT.Result", x = "FT.Result", y = "Home Fouls") +
  theme_minimal()
#H.Fouls is almost similar in all three match outcomes hence we cannot use fouls to predict match outcome

ggplot(mydata, aes(x = FT.Result, y = A.Fouls, fill = FT.Result)) +
  geom_boxplot() +
  labs(title = "Away Fouls by FT.Result", x = "FT.Result", y = "Away Fouls") +
  theme_minimal()
#A.Fouls is almost similar in all three cases of match outcomes

ggplot(mydata, aes(x = FT.Result, y = H.Corners, fill = FT.Result)) +
  geom_boxplot() +
  labs(title = "Home Corners by FT.Result", x = "FT.Result", y = "Home Corners") +
  theme_minimal()
#when the home team wins they apparently take more no. of corners

ggplot(mydata, aes(x = FT.Result, y = A.Corners, fill = FT.Result)) +
  geom_boxplot() +
  labs(title = "Away Corners by FT.Result", x = "FT.Result", y = "Away Corners") +
  theme_minimal()
#similarly when away team wins they take more number of corners 
#hence corners can be used to predict match outcome

ggplot(mydata, aes(x = FT.Result, y = H.Yellow, fill = FT.Result)) +
  geom_boxplot() +
  labs(title = "Home Yellow Cards by FT.Result", x = "FT.Result", y = "Home Yellow") +
  theme_minimal()
#same in all three cases
ggplot(mydata, aes(x = FT.Result, y = A.Yellow, fill = FT.Result)) +
  geom_boxplot() +
  labs(title = "Away Yellow Cards by FT.Result", x = "FT.Result", y = "Away Yellow") +
  theme_minimal()
#same in all three cases
ggplot(mydata, aes(x = FT.Result, y = H.Red, fill = FT.Result)) +
  geom_boxplot() +
  labs(title = "Home Red Cards by FT.Result", x = "FT.Result", y = "Home Red") +
  theme_minimal()
#same in all three cases
ggplot(mydata, aes(x = FT.Result, y = A.Red, fill = FT.Result)) +
  geom_boxplot() +
  labs(title = "Away Red Cards by FT.Result", x = "FT.Result", y = "Away Red") +
  theme_minimal()
#same in all three cases
#function to compare numerical and categorical variable
plot_box_by_category <- function(data, num_var, cat_var) {
  ggplot(data, aes_string(x = cat_var, y = num_var, fill = cat_var)) +
    geom_boxplot() +
    labs(
      title = paste(num_var, "by", cat_var),
      x = cat_var,
      y = num_var,
      fill = cat_var
    ) +
    theme_minimal()
}
plot_box_by_category(mydata, "FTH.Goals", "FT.Result")


# Categorical vs. Categorical:.
top_seasons <- names(sort(table(mydata$Season), decreasing = TRUE)[1:5])
top_data <- mydata %>% filter(Season %in% top_seasons)
# Grouped Bar Chart: Full Time Result by Season (limited to top seasons)
result_by_season <- mydata %>%
  filter(Season %in% top_seasons) %>%
  group_by(Season, FT.Result) %>%
  summarise(Count = n(), .groups = 'drop')

ggplot(result_by_season, aes(x = Season, y = Count, fill = FT.Result)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Full Time Result by Top Seasons",
       x = "Season",
       y = "Number of Matches",
       fill = "Full Time Result") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#in most of the top seasons in terms of number of matches the match result was home win



referee<-referee%>%slice_head(n=10)
top_10_referees <- referee$Referee
ftr_distribution <- mydata %>%
  filter(Referee %in% top_10_referees) %>%
  group_by(Referee, FT.Result) %>%
  summarise(MatchCount = n(), .groups = "drop") %>%
  complete(Referee, FT.Result = c("H", "D", "A"), fill = list(MatchCount = 0)) %>%
  mutate(FT.Result = factor(FT.Result, levels = c("H", "D", "A")))  

ggplot(ftr_distribution, aes(x = Referee, y = MatchCount, fill = FT.Result)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("H" = "#66a3ff", "D" = "#33cc33", "A" = "#ff6666")) +
  labs(
    title = "Full Time Result by Referee (Top 10 Strict Referees)",
    x = "Referee",
    y = "Number of Matches",
    fill = "Full Time Result"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
