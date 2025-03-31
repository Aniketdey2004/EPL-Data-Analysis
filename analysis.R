

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


goal_analysis <- mydata %>%
  group_by(Team = HomeTeam) %>%
  summarise(
    HomeGoalsScored = mean(FTH.Goals, na.rm = TRUE),
    HomeGoalsConceded = mean(FTA.Goals, na.rm = TRUE),
    HomeGames = n()
  ) %>%
  left_join(
    mydata %>%
      group_by(Team = AwayTeam) %>%
      summarise(
        AwayGoalsScored = mean(FTA.Goals, na.rm = TRUE),
        AwayGoalsConceded = mean(FTH.Goals, na.rm = TRUE),
        AwayGames = n()
      ),
    by = "Team"
  ) %>%
  filter(HomeGames >= 100 & AwayGames >= 100) %>%
  mutate(
    GoalDifference_Home = HomeGoalsScored - HomeGoalsConceded,
    GoalDifference_Away = AwayGoalsScored - AwayGoalsConceded,
    HomeAdvantage = GoalDifference_Home - GoalDifference_Away
  )
goal_analysis

home_performance <- mydata %>%
  group_by(HomeTeam) %>%
  summarise(
    Wins = sum(FT.Result == "H"),
    Draws = sum(FT.Result == "D"),
    Losses = sum(FT.Result == "A"),
    TotalMatches = n(),
    WinRate = Wins / TotalMatches
  ) %>%
  arrange(desc(Wins)) %>%
  filter(TotalMatches >= 100)
home_performance

avg_goals <- c(mean(mydata$HTA.Goals, na.rm = TRUE), mean(mydata$FTA.Goals, na.rm = TRUE))
goal_labels <- c("Home Goals", "Away Goals")

barplot(avg_goals, names.arg = goal_labels, col = c("blue", "red"),
        main = "Average Home vs. Away Goals per Match",
        ylab = "Average Goals", ylim = c(0, max(avg_goals) + 0.5))


card_analysis_plot <- function(mydata) {
  
  card_summary <- mydata %>%
    group_by(Team = HomeTeam) %>%
    summarise(
      HomeYellowCards = mean(H.Yellow, na.rm = TRUE),
      HomeRedCards = mean(H.Red, na.rm = TRUE)
    ) %>%
    left_join(
      mydata %>%
        group_by(Team = AwayTeam) %>%
        summarise(
          AwayYellowCards = mean(A.Yellow, na.rm = TRUE),
          AwayRedCards = mean(A.Red, na.rm = TRUE)
        ),
      by = "Team"
    )
  
  cat("\nAverage Yellow and Red Cards Received by EPL Teams (Home & Away):\n")
  print(kable(card_summary, digits = 2, caption = "Average Yellow and Red Cards per Match"))
  
  card_summary_long <- card_summary %>%
    pivot_longer(cols = -Team, names_to = "CardType", values_to = "Count")
  
  ggplot(card_summary_long, aes(x = reorder(Team, Count), y = Count, fill = CardType)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +  # Flip coordinates for better readability
    labs(
      title = "Average Yellow and Red Cards Received by EPL Teams (Home & Away)",
      x = "Team",
      y = "Average Number of Cards"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("yellow", "red", "lightyellow", "lightcoral")) +
    theme(axis.text.y = element_text(size = 8))
}

card_analysis_plot(mydata)

unique(mydata$Referee)
referee_card_analysis <- function(mydata) {
  
  referee_summary <- mydata %>%
    group_by(Referee) %>%
    summarise(
      AvgYellowPerMatch = mean(H.Yellow, na.rm = TRUE) + mean(A.Yellow, na.rm = TRUE),
      AvgRedPerMatch = mean(H.Red, na.rm = TRUE) + mean(A.Red, na.rm = TRUE),
      Matches = n()
    ) %>%
    arrange(desc(AvgYellowPerMatch), desc(AvgRedPerMatch))
  
  cat("\nTotal Yellow and Red Cards Given by Each Referee:\n")
  print(referee_summary, n = Inf)
}

referee_card_analysis(mydata)

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
  
  cat("\nNumber of Comeback Wins by Each Team (Trailing at Half Time but Winning at Full Time):\n")
  kable(comeback_summary, digits = 0, caption = "Comeback Wins by Teams")
}

comeback_count(mydata)

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

