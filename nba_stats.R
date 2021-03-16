library(shiny)
library(tidyverse)
library(jsonlite)
library(httr)
library(fmsb)
library(shinythemes)

# Define fill colors
colors_fill <- c(scales::alpha("gray", 0.1),
                 scales::alpha("gold", 0.1))

# Define line colors
colors_line <- c(scales::alpha("darkgray", 0.9),
                 scales::alpha("gold", 0.9))


#usethis::edit_r_environ("project")
#readRenviron(".Renviron")
apikey = "3741c4cba7b74a79827902c85827578e"

# Player information
r <- GET(
  "https://api.sportsdata.io/v3/nba/scores/json/Players",
  query = list(
    key = apikey
  )
)

stop_for_status(r)
json <- content(r, as = "text")
players <- fromJSON(json)

# Team information
r <- GET(
  "https://api.sportsdata.io/v3/nba/scores/json/teams",
  query = list(
    key = apikey
  )
)
stop_for_status(r)
json <- content(r, as = "text")
teams <- fromJSON(json)

# Scores in 2021
r <- GET(
  "https://api.sportsdata.io/v3/nba/stats/json/PlayerSeasonStats/2021",
  query = list(
    key = apikey
  )
)
stop_for_status(r)
json <- content(r, as = "text")
stats_2021 <- fromJSON(json)


ui <- fluidPage(
  theme = shinytheme("cerulean"),
  setBackgroundColor("white"),
  titlePanel(h1("Compare Two players' Stats in Season 2021", align = "center")),

    tabPanel("Player Comparison",
             sidebarLayout(
               sidebarPanel(
                 h4("Player 1", align = "center"),
                 selectInput("team1", "Team", choices = teams$Key),
                 uiOutput("teamplayer1"),
                 h4("Player 2", align = "center"),
                 selectInput("team2", "Team", choices = teams$Key),
                 uiOutput("teamplayer2"),
               ),
               mainPanel(
                 h4("Basic Statistics Table", align = "center"),
                 tableOutput("comparison_table"),
                 h4("Stats Comparison"),
                 plotOutput("radar_plot"),
               )
             )
    )
)


server <- function(input, output) {
  
  output$teamplayer1 <- renderUI({
    curr_data <- players %>% filter(players$Team %in% input$team1)
    selectInput("teamplayer1", "Player", 
                c(curr_data %>% pull(LastName)) %>% unique %>% sort())
  })
  
  output$teamplayer2 <- renderUI({
    curr_data <- players %>% filter(players$Team %in% input$team2)
    selectInput("teamplayer2", "Player", 
                c(curr_data %>% pull(LastName)) %>% unique %>% sort())
  })
  
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
  
  output$radar_plot <- renderPlot({
    
    req(!is.null(input$teamplayer1))
    req(!is.null(input$teamplayer2))
    
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
    
    radarchart(tbl,
               seg = 5,
               title = "Players Radar Chart",
               pcol = colors_line,
               pfcol = colors_fill,
               plwd = 4)
    
    # radarchart(tbl[c('Max', 'Min', 'Player 1'),],
    #            seg = 5,
    #            title = "Iron Man Radar Chart",
    #            plwd = 2)
    #
    # radarchart(tbl[c('Max', 'Min', 'Player 2'),],
    #            seg = 5,
    #            title = "Iron Man Radar Chart",
    #            plwd = 2)

    legend(x=0.6,
           y=1.35,
           legend = rownames(tbl[-c(1,2),]),
           bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
  })
  
}

shinyApp(ui = ui, server = server)