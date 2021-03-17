library(shiny)
library(tidyverse)
library(jsonlite)
library(httr)
library(fmsb)
library(shinythemes)
library(shinyWidgets)


#usethis::edit_r_environ("project")
readRenviron(".Renviron")


# Stats in season 2021
r <- GET(
  "https://api.sportsdata.io/v3/nba/stats/json/PlayerSeasonStats/2021",
  query = list(
    key = Sys.getenv("nba_key")
  )
)
stop_for_status(r)
json <- content(r, as = "text")
stats_2021 <- fromJSON(json)


# Player information
r <- GET(
  "https://api.sportsdata.io/v3/nba/scores/json/Players",
  query = list(
    key = Sys.getenv("nba_key")
  )
)

stop_for_status(r)
json <- content(r, as = "text")
players <- fromJSON(json)


# Team information
r <- GET(
  "https://api.sportsdata.io/v3/nba/scores/json/teams",
  query = list(
    key = Sys.getenv("nba_key")
  )
)
stop_for_status(r)
json <- content(r, as = "text")
teams <- fromJSON(json)


# Define UI 
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  setBackgroundColor("white"),
  titlePanel(h1("NBA Players' Stats Comparison in Season 20-21", align = "center")),

    tabPanel("Player Comparison",
             sidebarLayout(
               sidebarPanel(
                 h4("Player 1", align = "center"),
                 selectInput("team1", "Team", choices = teams$Key),
                 uiOutput("teamplayer1"),
                 h4("Player 2", align = "center"),
                 selectInput("team2", "Team", choices = teams$Key, selected = "CHA"),
                 uiOutput("teamplayer2"),
                 h4("Combine Radar Plots", align = "center"),
                 selectInput("uc", "Choice", choices = c("Yes", "No"), selected = "No"),
                 uiOutput("plot_choice")                 
               ),
               mainPanel(
                 h4("Statistics Table", align = "center"),
                 tableOutput("comparison_table"),
                 h4("Basic Stats Comparison", align = "center"),
                 uiOutput("radar_plot")
               )
             )
    )
)


# Define server function
server <- function(input, output) {
  
  # Select first player
  output$teamplayer1 <- renderUI({
    curr_data <- players %>% filter(players$Team %in% input$team1)
    selectInput("teamplayer1", "Player", 
                c(curr_data %>% pull(LastName)) %>% unique %>% sort())
  })
  
  # Select second player
  output$teamplayer2 <- renderUI({
    curr_data <- players %>% filter(players$Team %in% input$team2)
    selectInput("teamplayer2", "Player", 
                c(curr_data %>% pull(LastName)) %>% unique %>% sort())
  })
  
  # Statistics table of two players
  output$comparison_table <- renderTable({
    
    req(!is.null(input$teamplayer1))
    req(!is.null(input$teamplayer2))
    
    player1 <- players %>% 
      full_join(stats_2021, by = "PlayerID") %>% 
      filter(LastName == input$teamplayer1 & Team.y == input$team1) %>%
      mutate(PTS = Points/Games, REB = Rebounds/Games, AST = Assists/Games, STL = Steals/Games, BLK = BlockedShots/Games,
             P = Position.x, 'FG%' = FieldGoalsPercentage, 'FT%' = FreeThrowsPercentage, 
             '3P%' = ThreePointersPercentage, TO = Turnovers/Games, PF = PersonalFouls/Games, GP = Games) %>% 
      select(LastName, P, GP, PTS, REB, AST, BLK, STL, "FG%", "3P%", "FT%", TO, PF)
      
    player2 <- players %>% 
      full_join(stats_2021, by = "PlayerID") %>% 
      filter(LastName == input$teamplayer2 & Team.y == input$team2) %>%
      mutate(PTS = Points/Games, REB = Rebounds/Games, AST = Assists/Games, STL = Steals/Games, BLK = BlockedShots/Games,
             P = Position.x, 'FG%' = FieldGoalsPercentage, 'FT%' = FreeThrowsPercentage, 
             '3P%' = ThreePointersPercentage, TO = Turnovers/Games, PF = PersonalFouls/Games, GP = Games) %>% 
      select(LastName, P, GP, PTS, REB, AST, BLK, STL, "FG%", "3P%", "FT%", TO, PF)
      
      tbl <- rbind(player1, player2) %>% as.data.frame()
  })
  
  # Radar plot of statistics by user choice
  output$radar_plot <- renderUI({
    
    req(!is.null(input$teamplayer1))
    req(!is.null(input$teamplayer2))
    
    if (input$uc == "Yes") {
      
      plotOutput("two_plots")
    }
    
    else{
      
      fluidRow(
        splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot1"), plotOutput("plot2"))
      )
    }
  })
  
  # Giving plot one of two
  output$plot1 <- renderPlot({
    
    temp <- stats_2021 %>% 
      mutate(PTS = Points/Games, REB = Rebounds/Games, AST = Assists/Games,
             STL = Steals/Games, BLK = BlockedShots/Games) %>% 
      select(PTS, REB, AST, BLK, STL)
    
    point <- temp %>% select(PTS)
    max_point = max(point[!is.na(point)])
    
    rebound <- temp %>% select(REB)
    max_rebound = max(rebound[!is.na(rebound)])
    
    assit <- temp %>% select(AST)
    max_assit = max(assit[!is.na(assit)])
    
    steal <- temp %>% select(STL)
    max_steal = max(steal[!is.na(steal)])
    
    block <- temp %>% select(BLK)
    max_block = max(block[!is.na(block)])
    
    max_radar <- c(max_point, max_rebound, max_assit, max_steal, max_block)
    
    min_radar <- c(0, 0, 0, 0, 0)
    
    player1 <- players %>% 
      full_join(stats_2021, by = "PlayerID") %>% 
      filter(LastName == input$teamplayer1 & Team.y == input$team1) %>%
      mutate(PTS = Points/Games, REB = Rebounds/Games, AST = Assists/Games, STL = Steals/Games, BLK = BlockedShots/Games) %>% 
      select(PTS, REB, AST, STL, BLK)
    
    player2 <- players %>% 
      full_join(stats_2021, by = "PlayerID") %>% 
      filter(LastName == input$teamplayer2 & Team.y == input$team2) %>%
      mutate(PTS = Points/Games, REB = Rebounds/Games, AST = Assists/Games, STL = Steals/Games, BLK = BlockedShots/Games) %>% 
      select(PTS, REB, AST, STL, BLK)
    
    tbl <- rbind(max_radar, min_radar, player1, player2) %>% as.data.frame()
    
    row.names(tbl) <- c('Max', 'Min', 'Player 1', 'Player 2')
    
    radarchart(tbl[c('Max', 'Min', 'Player 1'),],
               seg = 5,
               title = "Player 1",
               pcol = scales::alpha("darkgray", 0.9),
               pfcol = scales::alpha("gray", 0.3),
               plwd = 2)
               
  }) 
  
  # Giving plot two of two
  output$plot2 <- renderPlot({
    
    temp <- stats_2021 %>% 
      mutate(PTS = Points/Games, REB = Rebounds/Games, AST = Assists/Games,
             STL = Steals/Games, BLK = BlockedShots/Games) %>% 
      select(PTS, REB, AST, BLK, STL)
    
    point <- temp %>% select(PTS)
    max_point = max(point[!is.na(point)])
    
    rebound <- temp %>% select(REB)
    max_rebound = max(rebound[!is.na(rebound)])
    
    assit <- temp %>% select(AST)
    max_assit = max(assit[!is.na(assit)])
    
    steal <- temp %>% select(STL)
    max_steal = max(steal[!is.na(steal)])
    
    block <- temp %>% select(BLK)
    max_block = max(block[!is.na(block)])
    
    max_radar <- c(max_point, max_rebound, max_assit, max_steal, max_block)
    
    min_radar <- c(0, 0, 0, 0, 0)
    
    player1 <- players %>% 
      full_join(stats_2021, by = "PlayerID") %>% 
      filter(LastName == input$teamplayer1 & Team.y == input$team1) %>%
      mutate(PTS = Points/Games, REB = Rebounds/Games, AST = Assists/Games, STL = Steals/Games, BLK = BlockedShots/Games) %>% 
      select(PTS, REB, AST, STL, BLK)
    
    player2 <- players %>% 
      full_join(stats_2021, by = "PlayerID") %>% 
      filter(LastName == input$teamplayer2 & Team.y == input$team2) %>%
      mutate(PTS = Points/Games, REB = Rebounds/Games, AST = Assists/Games, STL = Steals/Games, BLK = BlockedShots/Games) %>% 
      select(PTS, REB, AST, STL, BLK)
    
    tbl <- rbind(max_radar, min_radar, player1, player2) %>% as.data.frame()
    
    row.names(tbl) <- c('Max', 'Min', 'Player 1', 'Player 2')
    
    radarchart(tbl[c('Max', 'Min', 'Player 2'),],
               seg = 5,
               title = "Player 2",
               pcol = scales::alpha("gold", 0.9),
               pfcol = scales::alpha("gold", 0.1),
               plwd = 2)
    }) 
  
  # Giving one plot of two players
  output$two_plots <- renderPlot({
    
    temp <- stats_2021 %>% 
      mutate(PTS = Points/Games, REB = Rebounds/Games, AST = Assists/Games,
             STL = Steals/Games, BLK = BlockedShots/Games) %>% 
      select(PTS, REB, AST, BLK, STL)
    
    point <- temp %>% select(PTS)
    max_point = max(point[!is.na(point)])
    
    rebound <- temp %>% select(REB)
    max_rebound = max(rebound[!is.na(rebound)])
    
    assit <- temp %>% select(AST)
    max_assit = max(assit[!is.na(assit)])
    
    steal <- temp %>% select(STL)
    max_steal = max(steal[!is.na(steal)])
    
    block <- temp %>% select(BLK)
    max_block = max(block[!is.na(block)])
    
    max_radar <- c(max_point, max_rebound, max_assit, max_steal, max_block)
    
    min_radar <- c(0, 0, 0, 0, 0)
    
    player1 <- players %>% 
      full_join(stats_2021, by = "PlayerID") %>% 
      filter(LastName == input$teamplayer1 & Team.y == input$team1) %>%
      mutate(PTS = Points/Games, REB = Rebounds/Games, AST = Assists/Games, STL = Steals/Games, BLK = BlockedShots/Games) %>% 
      select(PTS, REB, AST, STL, BLK)
    
    player2 <- players %>% 
      full_join(stats_2021, by = "PlayerID") %>% 
      filter(LastName == input$teamplayer2 & Team.y == input$team2) %>%
      mutate(PTS = Points/Games, REB = Rebounds/Games, AST = Assists/Games, STL = Steals/Games, BLK = BlockedShots/Games) %>% 
      select(PTS, REB, AST, STL, BLK)
    
    tbl <- rbind(max_radar, min_radar, player1, player2) %>% as.data.frame()
    
    row.names(tbl) <- c('Max', 'Min', 'Player 1', 'Player 2')
    
    colors_fill <- c(scales::alpha("gray", 0.1),
                     scales::alpha("gold", 0.1))
    
    colors_line <- c(scales::alpha("darkgray", 0.9),
                     scales::alpha("gold", 0.9))
    
    radarchart(tbl,
               seg = 5,
               title = "Players Radar Chart",
               pcol = colors_line,
               pfcol = colors_fill,
               plwd = 4)
    
    legend(x=0.6,
           y=1.35,
           legend = rownames(tbl[-c(1,2),]),
           bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
    
  })
}


shinyApp(ui = ui, server = server)