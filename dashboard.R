# Load analysis functions from analysis.R
source("analysis.R")


# app.R
library(shiny)
library(ggplot2)
library(dplyr)


# Define UI
ui <- fluidPage(
  titlePanel("Football Match Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Home Matches'",
        helpText("This tab shows the number of matches played by teams as Home teams.")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Away Matches'",
        helpText("This tab displays the number of matches played by teams as Away teams.")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Total Matches'",
        helpText("This tab combines home and away matches to show total matches played.")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Goal Analysis'",
        helpText("Goal analysis includes goals scored and conceded by teams at home and away.")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Top Scoring Teams'",
        helpText("Displays the top 10 highest scoring teams in the league.")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Home Win Rate'",
        helpText("Shows teams ranked by their home win percentage.")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Away Win Rate'",
        helpText("Displays away win percentage by team.")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Overall Success'",
        helpText("Combines home and away results to show most successful teams.")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Home Advantage'",
        helpText("Shows how much better teams perform at home compared to away.")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Card Analysis'",
        helpText("Displays average yellow and red cards received by each EPL team at home, away, and overall.")
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Dirty Play Analysis'",
        helpText("Visualizes the top 10 'dirtiest' teams based on a score calculated as: Yellow Cards + 2 × Red Cards.")
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Referee Analysis'",
        helpText("Displays the average yellow/red cards per match given by each referee, and ranks them based on 'Strictness Score'.")
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Comeback Analysis'",
        helpText("Shows the teams with the highest number of comeback wins—matches where they were trailing at half-time but won by full-time.")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Foul Analysis'",
        helpText("Shows if winning teams tend to commit more or fewer fouls than losing teams.")
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Seasonal Goal Trends'",
        helpText("Displays the goal difference trends for selected teams over multiple seasons.")
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Team Performance Over Time'",
        helpText("Shows the performance (goal difference and points) of selected teams over time.")
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Rolling Performance Analysis'",
        helpText("Analyzes the rolling performance of a team over a specified window size.")
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Home Matches",
                           plotOutput("homeMatchesPlot", width = "95%", height = "1000px"),
                           tableOutput("homeMatchesTable")
                  ),
                  tabPanel("Away Matches",
                           plotOutput("awayMatchesPlot", width = "95%", height = "1000px"),
                           tableOutput("awayMatchesTable")
                  ),
                  tabPanel("Total Matches",
                           plotOutput("totalMatchesPlot", width = "95%", height = "1000px"),
                           tableOutput("totalMatchesTable")
                  ),
                  tabPanel("Goal Analysis",
                           tableOutput("goalAnalysisTable")
                  ),
                  tabPanel("Top Scoring Teams",
                           plotOutput("topScoringPlot", width = "95%", height = "1000px"),
                           tableOutput("topScoringTable")
                  ),
                  tabPanel("Home Win Rate",
                           plotOutput("homeWinRatePlot", width = "95%", height = "1000px"),
                           tableOutput("homeWinRateTable")
                  ),
                  tabPanel("Away Win Rate",
                           plotOutput("awayWinRatePlot", width = "95%", height = "1000px"),
                           tableOutput("awayWinRateTable")
                  ),
                  tabPanel("Overall Success",
                           plotOutput("overallSuccessPlot", width = "95%", height = "1000px"),
                           tableOutput("overallSuccessTable")
                  ),
                  tabPanel("Home Advantage",
                           plotOutput("homeAdvantagePlot", width = "95%", height = "1000px"),
                           tableOutput("homeAdvantageTable")
                  ),
                  tabPanel("Card Analysis",
                           tableOutput("card_table")
                  ),
                  
                  tabPanel("Dirty Play Analysis",
                           plotOutput("dirty_plot", width = "95%", height = "600px")
                  ),
                  
                  tabPanel("Referee Analysis",
                           plotOutput("referee_plot", width = "95%", height = "600px"),
                           h4("Referee Stats Table"),
                           verbatimTextOutput("referee_table")
                  ),
                  
                  tabPanel("Comeback Analysis",
                           plotOutput("comeback_plot", width = "95%", height = "600px"),
                           tableOutput("comeback_table")
                  ),
                  tabPanel("Foul Analysis",
                           tableOutput("foul_table")
                  ),
                  
                  tabPanel("Seasonal Goal Trends",
                           plotOutput("seasonal_goal_trends", width = "95%", height = "600px"),
                           tableOutput("seasonal_performance_table")
                  ),
                  
                  tabPanel("Team Performance Over Time",
                           selectInput("team_selected_perf", "Select a Team:",
                                       choices = c("Man United", "Tottenham", "Arsenal", "Chelsea", "Everton", "Liverpool", 
                                                   "Newcastle", "Aston Villa", "West Ham", "Man City", "Southampton", "Leicester",
                                                   "Fulham", "Blackburn", "Sunderland", "Crystal Palace", "Leeds", "Middlesbrough",
                                                   "Bolton", "West Brom", "Wolves", "Stoke", "Norwich", "Burnley", "Coventry",
                                                   "Charlton", "Watford", "Wigan", "Bournemouth", "Brighton", "Sheffield Weds",
                                                   "Wimbledon", "Birmingham", "Derby", "Portsmouth", "Swansea", "Nott'm Forest",
                                                   "QPR", "Sheffield United", "Hull", "Ipswich", "Brentford", "Reading", "Bradford",
                                                   "Cardiff", "Huddersfield", "Oldham", "Swindon", "Barnsley", "Blackpool", "Luton",
                                                   "Brighton & Hove Albion", "Ipswich Town")),
                           plotOutput("team_performance_plot", width = "95%", height = "600px"),
                           verbatimTextOutput("team_performance_table")
                  ),
                  
                  tabPanel("Rolling Performance Analysis",
                           selectInput("team_selected_rolling", "Select a Team:",
                                       choices = c("Man United", "Tottenham", "Arsenal", "Chelsea", "Everton", "Liverpool", 
                                                   "Newcastle", "Aston Villa", "West Ham", "Man City", "Southampton", "Leicester",
                                                   "Fulham", "Blackburn", "Sunderland", "Crystal Palace", "Leeds", "Middlesbrough",
                                                   "Bolton", "West Brom", "Wolves", "Stoke", "Norwich", "Burnley", "Coventry",
                                                   "Charlton", "Watford", "Wigan", "Bournemouth", "Brighton", "Sheffield Weds",
                                                   "Wimbledon", "Birmingham", "Derby", "Portsmouth", "Swansea", "Nott'm Forest",
                                                   "QPR", "Sheffield United", "Hull", "Ipswich", "Brentford", "Reading", "Bradford",
                                                   "Cardiff", "Huddersfield", "Oldham", "Swindon", "Barnsley", "Blackpool", "Luton",
                                                   "Brighton & Hove Albion", "Ipswich Town")),
                           plotOutput("rolling_performance_plot", width = "95%", height = "600px"),
                           tableOutput("rolling_performance_table")
                  )
                  
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$homeMatchesPlot <- renderPlot({
    ggplot(home, aes(x = reorder(HomeTeam, HomeMatches), y = HomeMatches)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Number of Matches Played by Home Teams", x = "Home Team", y = "Number of Matches")
  })
  output$homeMatchesTable <- renderTable({ home })
  
  output$awayMatchesPlot <- renderPlot({
    ggplot(away, aes(x = reorder(AwayTeam, AwayMatches), y = AwayMatches)) +
      geom_bar(stat = "identity", fill = "orange") +
      coord_flip() +
      labs(title = "Number of Matches Played by Away Teams", x = "Away Team", y = "Number of Matches")
  })
  output$awayMatchesTable <- renderTable({ away })
  
  total_matches <- findtotalmatches(home, away)
  output$totalMatchesPlot <- renderPlot({
    ggplot(total_matches, aes(x = reorder(HomeTeam, TotalMatches), y = TotalMatches)) +
      geom_bar(stat = "identity", fill = "purple") +
      coord_flip() +
      labs(title = "Number of Matches Played by Teams", x = "Team", y = "Total Matches")
  })
  output$totalMatchesTable <- renderTable({ total_matches })
  
  goal_analysis <- findteamgoalanalysis(mydata)
  output$goalAnalysisTable <- renderTable({ goal_analysis })
  
  topscoringteams <- findtopscoringteams(goal_analysis)
  output$topScoringPlot <- renderPlot({
    ggplot(topscoringteams, aes(x = reorder(Team, TotalGoals), y = TotalGoals)) +
      geom_bar(stat = "identity", fill = "#ff6361") +
      coord_flip() +
      labs(title = "Top 10 Most Scoring Teams", x = "Team", y = "Total Goals Scored") +
      theme_minimal()
  })
  output$topScoringTable <- renderTable({ topscoringteams })
  
  home_performance <- homeperformanceanalysis(mydata)
  output$homeWinRatePlot <- renderPlot({
    ggplot(home_performance, aes(x = reorder(HomeTeam, WinRate), y = WinRate)) +
      geom_bar(stat = "identity", fill = "#63c78f") +
      coord_flip() +
      labs(title = "Home Win Rate of Teams", x = "Teams", y = "Win Rate %")
  })
  output$homeWinRateTable <- renderTable({ home_performance })
  
  # Away Performance
  away_perform <- awayperformanceanalysis(mydata)
  output$awayWinRatePlot <- renderPlot({
    ggplot(away_perform, aes(x = reorder(AwayTeam, WinRate), y = WinRate)) +
      geom_bar(stat = "identity", fill = "#63c78f") +
      coord_flip() +
      labs(title = "Away Win Rate of Teams", x = "Teams", y = "Win Rate %")
  })
  output$awayWinRateTable <- renderTable({ away_perform })
  
  # Overall Success
  overall_analysis <- merge(home_performance, away_perform, by.x = "HomeTeam", by.y = "AwayTeam")
  topsuccess <- topsuccessfullteams(overall_analysis)
  output$overallSuccessPlot <- renderPlot({
    ggplot(topsuccess %>% slice_head(n = 10), aes(x = reorder(Team, WinRate), y = WinRate)) +
      geom_bar(stat = "identity", fill = "#4e79a7") +
      coord_flip() +
      labs(title = "Top 10 Most Successful Teams by Win Rate", x = "Team", y = "Win Rate (%)") +
      theme_minimal()
  })
  output$overallSuccessTable <- renderTable({ topsuccess })
  
  # Home Advantage
  homeadvantageteams <- homeadvantageanalysis(overall_analysis)
  output$homeAdvantagePlot <- renderPlot({
    ggplot(homeadvantageteams, aes(x = reorder(Team, homeAdv), y = homeAdv)) +
      geom_bar(stat = "identity", fill = "#9d4edd") +
      coord_flip() +
      labs(title = "Teams with Greater Home Advantage", x = "Teams", y = "Home Advantage")
  })
  output$homeAdvantageTable <- renderTable({ homeadvantageteams })
  
  card_analysis_table <- function(mydata) {
    home_cards <- mydata %>%
      group_by(Team = HomeTeam) %>%
      summarise(
        HomeYellowCards = mean(H.Yellow, na.rm = TRUE),
        HomeRedCards = mean(H.Red, na.rm = TRUE)
      )
    
    away_cards <- mydata %>%
      group_by(Team = AwayTeam) %>%
      summarise(
        AwayYellowCards = mean(A.Yellow, na.rm = TRUE),
        AwayRedCards = mean(A.Red, na.rm = TRUE)
      )
    
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
    
    full_join(home_cards, away_cards, by = "Team") %>%
      left_join(overall_cards, by = "Team") %>%
      relocate(Team)
  }
  
  dirty_play_plot <- function(card_summary) {
    card_summary %>%
      mutate(DirtyPlayScore = AvgYellowCards + 2 * AvgRedCards) %>%
      arrange(desc(DirtyPlayScore)) %>%
      head(10) %>%
      ggplot(aes(x = reorder(Team, DirtyPlayScore), y = DirtyPlayScore, fill = Team)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        title = "Top 10 Dirtiest Teams Based on Dirty Play Score",
        x = "Team",
        y = "Dirty Play Score"
      ) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10)) +
      scale_fill_brewer(palette = "Set3")
  }
  
  referee_card_analysis <- function(mydata) {
    mydata %>%
      group_by(Referee) %>%
      summarise(
        AvgYellowPerMatch = mean(H.Yellow + A.Yellow, na.rm = TRUE),
        AvgRedPerMatch = mean(H.Red + A.Red, na.rm = TRUE),
        Matches = n()
      ) %>%
      mutate(
        StrictnessScore = AvgYellowPerMatch + 2 * AvgRedPerMatch
      ) %>%
      arrange(desc(StrictnessScore))
  }
  
  comeback_count <- function(mydata) {
    mydata %>%
      filter((HT.Result == "A" & FT.Result == "H") | (HT.Result == "H" & FT.Result == "A")) %>%
      mutate(WinningTeam = ifelse(FT.Result == "H", HomeTeam, AwayTeam)) %>%
      group_by(WinningTeam) %>%
      summarise(ComebackWins = n()) %>%
      arrange(desc(ComebackWins))
  }
  
  # --- Outputs ---
  
  output$card_table <- renderTable({
    card_analysis_table(mydata)
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$dirty_plot <- renderPlot({
    dirty_play_plot(card_analysis_table(mydata))
  })
  
  output$referee_table <- renderPrint({
    referee_card_analysis(mydata)
  })
  
  output$referee_plot <- renderPlot({
    referee <- referee_card_analysis(mydata)
    referee <- referee %>% slice_head(n = 10)
    ggplot(referee, aes(x = reorder(Referee, StrictnessScore), y = StrictnessScore)) +
      geom_bar(stat = "identity", fill = "#ff6361") +
      coord_flip() +
      labs(
        title = "Top 10 Strict Referees",
        x = "Referee",
        y = "Strictness Score"
      ) +
      theme_minimal()
  })
  
  output$comeback_plot <- renderPlot({
    comeback <- comeback_count(mydata)
    ggplot(comeback, aes(x = reorder(WinningTeam, ComebackWins), y = ComebackWins)) + 
      geom_bar(stat = "identity", fill = "#4A90E2") + 
      coord_flip() +
      labs(title = "Teams with Highest Comebacks",
           x = "Teams",
           y = "Comebacks")
  })
  
  output$comeback_table <- renderTable({
    comeback_count(mydata)
  }, striped = TRUE, bordered = TRUE, hover = TRUE)
  
  output$foul_table <- renderTable({
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
    return(foul_summary)
  })
  
  # Seasonal Goal Trends - Displaying the trends of selected teams over multiple seasons
  output$seasonal_goal_trends <- renderPlot({
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
  })
  
  # Team Performance Over Time - Performance of teams over multiple seasons
  output$team_performance_plot <- renderPlot({
    req(input$team_selected_perf)
    team_performance(input$team_selected_perf)
  })
  
  output$team_performance_table <- renderText({
    paste("Displaying data for:", input$team_selected_perf)
  })
  
  output$rolling_performance_plot <- renderPlot({
    req(input$team_selected_rolling)
    analyze_team_performance_over_time(mydata, input$team_selected_rolling, window_size = 30)
  })
  
  output$rolling_performance_table <- renderTable({
    # You can return a table if you extract specific stats from the rolling analysis function
    # For now, return last few rows as sample
    req(input$team_selected_rolling)
    team_data <- mydata %>%
      filter(HomeTeam == input$team_selected_rolling | AwayTeam == input$team_selected_rolling)
    head(team_data, 5)
  })

}

shinyApp(ui = ui, server = server)

