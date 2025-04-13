library(shiny)
library(ggplot2)
library(dplyr)


source("analysis.R")


ui <- fluidPage(
  titlePanel("EPL Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      helpText("View the number of matches played by teams in the Home and Away sections.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Home Matches",
                 plotOutput("homeMatchesPlot"),
                 tableOutput("homeMatchesTable")),
        tabPanel("Away Matches",
                 plotOutput("awayMatchesPlot"),
                 tableOutput("awayMatchesTable"))
      )
    )
  )
)


server <- function(input, output) {
  output$homeMatchesPlot <- renderPlot({
    ggplot(home, aes(x = reorder(HomeTeam, HomeMatches), y = HomeMatches)) + 
      geom_bar(stat = "identity", fill = "steelblue") + 
      coord_flip() +
      labs(title = "Number of Matches Played by Home Teams",
           x = "Home Team",
           y = "Number of Matches")
  })
  
  output$homeMatchesTable <- renderTable({
    home
  })
  
  
  output$awayMatchesPlot <- renderPlot({
    ggplot(away, aes(x = reorder(AwayTeam, AwayMatches), y = AwayMatches)) + 
      geom_bar(stat = "identity", fill = "orange") + 
      coord_flip() +
      labs(title = "Number of Matches Played by Away Teams",
           x = "Away Team",
           y = "Number of Matches")
  })
  
  output$awayMatchesTable <- renderTable({
    away
  })
}


shinyApp(ui = ui, server = server)
