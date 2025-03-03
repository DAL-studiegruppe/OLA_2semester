library(mongolite)
library(jsonlite)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(ggsoccer)
library(tidyverse)

# Connection + DATA
{
  conm <- mongo(
    url = "mongodb://localhost",
    db = "fodbold2",
    collection = "matches",
  )
  
  cong <- mongo(
    url = "mongodb://localhost",
    db = "fodbold2",
    collection = "games",
  )

  matches <- conm$find(
    query = '{}',
    fields = '{}'
  )
}

dfg_shot <- readRDS("dfg_shot.rds")

# dfg_pass <- readRDS("all_passes.rds")

# OPG. 4.1 - data
{
  XGgoal <- dfg_shot %>%
    filter(shot.isGoal == TRUE) %>%  # Filtrer for kun at inkludere mål
    group_by(player.name, match_info.competitionId, match_info.seasonId) %>%        # Gruppér efter spillerens navn
    summarise(Sum_Mål = n(),
              mean_XG = mean(shot.xg, na.rm = TRUE))  # Summér xG, ignorer NA-værdier
}
# OPG. 4.2 - data - MONGO
{
# Med mongo skal der ikke indhetes data ud collection matches, da alle matchId'er skal bruges - se vector "mygames"
# Ud over dette skal der laves connection til games, da query køres i server og henter data der.
# Der agregeres på den data der kommer gennem query inden plot, her: PLOT - Opgave 4.2 – Afleveringer og spillere - MONGO
}
# OPG. 4.2 - data - UDEN MONGO
{
# data hentes gennem rds filen all_passes, hvor på den og agregering ligger i serveren.
}
# OPG. 4.3 - data
{
  shots_data <- dfg_shot %>%
    select(
      liga = match_info.competitionId,
      season = match_info.seasonId,
      round = match_info.gameweek,
      match = match_info.label,      # Vælger match_info.label og omdøber den til match
      team = team.name,
      shots = shot.isGoal,           # Vælger shot.isGoal og omdøber den til shots
      xstart = location.x,           # Vælger location.x og omdøber den til xstart
      ystart = location.y            # Vælger location.y og omdøber den til ystart
    ) %>%
    mutate(
      xend = 100,                    # Sætter xend til 100 (kan justeres efter behov)
      yend = runif(n(), 45, 55)      # Genererer tilfældige y-koordinater mellem 40 og 60
    )
}

# Mapping til automatisk dropdown
{
    # Liga mapping
    liga_mapping <- c("692" = "Polen", "635" = "Holland")
    liga_choices <- setNames(names(liga_mapping), liga_mapping)  # Viser navne, gemmer ID'er
    
    # Sæson mappings for hver liga
    season_mapping_polen <- c("186215" = "2021/2022", "188088" = "2022/2023")
    season_choices_polen <- setNames(names(season_mapping_polen), season_mapping_polen)
    
    season_mapping_holland <- c("187502" = "2021/2022", "188125" = "2022/2023")
    season_choices_holland <- setNames(names(season_mapping_holland), season_mapping_holland)
    
    
    # Dynamisk generering af runde-mapping
    generate_rounds_mapping <- function(data) {
      rounds_mapping <- split(data$round, data$season)
      
      # Konverter til formatet: "1" = "Runde 1", "2" = "Runde 2", ..., "20" = "Runde 20"
      rounds_mapping <- lapply(rounds_mapping, function(r) setNames(as.character(r), paste0("Runde ", r)))
      
      return(rounds_mapping)
    }
    
    # Dynamisk generering af kamp-mapping
    generate_matches_mapping <- function(data) {
      matches_mapping <- split(data[, c("round", "match")], data$season)
      
      matches_mapping <- lapply(matches_mapping, function(season_data) {
        split(setNames(season_data$match, season_data$match), season_data$round)
      })
      
      return(matches_mapping)
    }
    
    # Generér mappings dynamisk fra shots_data
    rounds_mapping <- generate_rounds_mapping(shots_data)
    matches_mapping <- generate_matches_mapping(shots_data)
  }

mygames <- matches[1:905,2]

# UI
{
ui <- dashboardPage(
  dashboardHeader(title = "Kamp data"),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Opgave 4.1 – Skud og spillere", tabName = "skud_spiller", icon = icon("futbol")),
      menuItem("Opgave 4.2 – Afleveringer og spillere MONGO", tabName = "pasninger", icon = icon("arrows-alt")),
      menuItem("Opgave 4.2 – Afleveringer og spillere", tabName = "pasninger2", icon = icon("arrows-alt")),
      menuItem("Opgave 4.3 – Skud i en kamp", tabName = "skud", icon = icon("rocket"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "skud_spiller",
              fluidRow(
                box(title = "Vælg Data", width = 4, status = "primary", solidHeader = TRUE,
                    selectInput("selected_liga", "Vælg Liga:", choices = liga_choices, selected = NULL),
                    selectInput("selected_season_skudSpiller", "Vælg Sæson:", choices = character(0), selected = NULL),
                    selectInput("numPlayers", label = "Vælg antal spillere", choices = seq(1, max(1, nrow(XGgoal))), selected = 3)
                ),
                box(title = "Graf over sum af xG & totale mål fordelt på antal spillere", width = 8, status = "primary", solidHeader = TRUE,
                    plotOutput("skudSpillerPlot", height = "500px"))
              )
      ),
      # MONGO
      tabItem(tabName = "pasninger", 
              fluidRow(
                box(title = "Vælg Data", width = 4, status = "primary", solidHeader = TRUE,
                    selectInput("selected_liga", "Vælg Liga:", choices = liga_choices, selected = NULL),
                    selectInput("selected_season_pasninger", "Vælg Sæson:", choices = character(0), selected = NULL),  
                    selectInput("selected_round_pasninger", "Vælg Runde:", choices = character(0), selected = NULL),
                    selectInput("selected_match_pasninger", "Vælg Kamp:", choices = mygames, selected = NULL)
                ),
                box(title = "Afleveringer per hold", width = 8, status = "primary", solidHeader = TRUE,
                    plotOutput("pasPlot", height = "500px")),
                box(title = "Hent fra MongoDB", width = 3, status = "info", solidHeader = TRUE,
                    textOutput("mongo_time_display"))
              )
      ),
      # UDEN MONGO
      tabItem(tabName = "pasninger2",
              fluidRow(
                box(title = "Vælg Data", width = 4, status = "primary", solidHeader = TRUE,
                    selectInput("selected_liga", "Vælg Liga:", choices = liga_choices, selected = NULL),
                    selectInput("selected_season_pasninger2", "Vælg Sæson:", choices = character(0), selected = NULL),  
                    selectInput("selected_round_pasninger2", "Vælg Runde:", choices = character(0), selected = NULL),
                    selectInput("selected_match_pasninger2", "Vælg Kamp:", choices = mygames, selected = NULL)
                ),
                box(title = "Afleveringer per hold", width = 8, status = "primary", solidHeader = TRUE,
                    plotOutput("pasPlot2", height = "500px")),
                box(title = "Hent fra R", width = 3, status = "info", solidHeader = TRUE,
                    textOutput("df_time_display"))
              )
      ),
      
      tabItem(tabName = "skud",
              fluidRow(
                box(title = "Vælg Data", width = 4, status = "primary", solidHeader = TRUE,
                    selectInput("selected_liga", "Vælg Liga:", choices = liga_choices, selected = NULL),
                    selectInput("selected_season", "Vælg Sæson:", choices = character(0), selected = NULL),  
                    selectInput("selected_round", "Vælg Runde:", choices = character(0), selected = NULL),
                    selectInput("selected_match", "Vælg Kamp:", choices = character(0), selected = NULL)
                ),
                box(title = "Skudfordeling i den valgte kamp", width = 8, status = "primary", solidHeader = TRUE,
                    plotOutput("shotPlot", height = "500px"))
              )
      )
    )
  )
)
}


# SERVER
server <- function(input, output, session) {
  
  # Tag tid på data
  time_values <- reactiveValues(mongo_time = NA, df_time = NA)

  # Dynamisk sæson-mapping baseret på liga
  season_mapping <- reactive({
    if (input$selected_liga == "692") {
      return(season_choices_polen)   
    } else if (input$selected_liga == "635") {
      return(season_choices_holland) 
    } else {
      return(character(0))
    }
  })
  
  # Opdaterer dropdowns baseret på valgt liga
  #skud spiller
  {
    observeEvent(input$selected_liga, {
      updateSelectInput(session, "selected_season_skudSpiller", choices = season_mapping(), selected = NULL)
    })
  }
  #pasninger - MONGO
  {
    observeEvent(input$selected_liga, {
      updateSelectInput(session, "selected_season_pasninger", choices = season_mapping(), selected = NULL)
      updateSelectInput(session, "selected_round_pasninger", choices = character(0), selected = NULL)
      updateSelectInput(session, "selected_match_pasninger", choices = character(0), selected = NULL)
    })
    
    observeEvent(input$selected_season_pasninger, {
      req(input$selected_season_pasninger)
      
      available_rounds <- rounds_mapping[[input$selected_season_pasninger]]  
      
      updateSelectInput(session, "selected_round_pasninger", choices = available_rounds, selected = NULL)
      updateSelectInput(session, "selected_match_pasninger", choices = character(0), selected = NULL)
    })
    
    observeEvent(input$selected_round_pasninger, {
      req(input$selected_season_pasninger, input$selected_round_pasninger)
      
      available_matches <- matches_mapping[[input$selected_season_pasninger]][[input$selected_round_pasninger]]  
      
      updateSelectInput(session, "selected_match_pasninger", choices = available_matches, selected = NULL)
    })
  }
  #pasninger - UDEN MONGO
  {
    observeEvent(input$selected_liga, {
      updateSelectInput(session, "selected_season_pasninger2", choices = season_mapping(), selected = NULL)
      updateSelectInput(session, "selected_round_pasninger2", choices = character(0), selected = NULL)
      updateSelectInput(session, "selected_match_pasninger2", choices = character(0), selected = NULL)
    })
    
    observeEvent(input$selected_season_pasninger2, {
      req(input$selected_season_pasninger2)
      
      available_rounds <- rounds_mapping[[input$selected_season_pasninger2]]  
      
      updateSelectInput(session, "selected_round_pasninger2", choices = available_rounds, selected = NULL)
      updateSelectInput(session, "selected_match_pasninger2", choices = character(0), selected = NULL)
    })
    
    observeEvent(input$selected_round_pasninger2, {
      req(input$selected_season_pasninger2, input$selected_round_pasninger2)
      
      available_matches <- matches_mapping[[input$selected_season_pasninger2]][[input$selected_round_pasninger2]]  
      
      updateSelectInput(session, "selected_match_pasninger2", choices = available_matches, selected = NULL)
    })
  }
  #skud
  {
    # Opdaterer dropdowns baseret på valgt liga
    observeEvent(input$selected_liga, {
      updateSelectInput(session, "selected_season", choices = season_mapping(), selected = NULL)
      updateSelectInput(session, "selected_round", choices = character(0), selected = NULL)
      updateSelectInput(session, "selected_match", choices = character(0), selected = NULL)
    })
    
    observeEvent(input$selected_season, {
      req(input$selected_season)
      
      available_rounds <- rounds_mapping[[input$selected_season]]  
      
      updateSelectInput(session, "selected_round", choices = available_rounds, selected = NULL)
      updateSelectInput(session, "selected_match", choices = character(0), selected = NULL)
    })
    
    observeEvent(input$selected_round, {
      req(input$selected_season, input$selected_round)
      
      available_matches <- matches_mapping[[input$selected_season]][[input$selected_round]]  
      
      updateSelectInput(session, "selected_match", choices = available_matches, selected = NULL)
    })
  }
  
  # Filtrering af data
  # Opg. 4.1
  {
  filtered_data1 <- reactive({
    req(input$selected_liga, input$selected_season_skudSpiller, input$numPlayers)
    
    XGgoal %>%
      filter(match_info.competitionId == input$selected_liga, 
             match_info.seasonId == input$selected_season_skudSpiller) %>%
      group_by(player.name) %>%
      summarise(mean_XG = mean(mean_XG, na.rm = TRUE), 
                Sum_Mål = sum(Sum_Mål, na.rm = TRUE)) %>%
      arrange(desc(Sum_Mål)) %>%
      head(as.numeric(input$numPlayers))
  })
  }
  # Opg. 4.2 - MONGO
  {
  filtered_data3 <- reactive({
    req(input$selected_liga, input$selected_season_pasninger, input$selected_round_pasninger, input$selected_match_pasninger)
    
    start_time_m <- Sys.time()
    
    mlabel <- input$selected_match_pasninger
    
    #fra label til matchId
   
    matchid_list <- matches %>%
      filter(label == mlabel) %>%
      pull("_id")
  
    query <- sprintf('{"type.primary": "pass", "matchId": %d}', matchid_list)
    
    matchPassesBrutto <- cong$find(
      query = query,
      fields = '{}'
    )
  
    matchPassesBrutto <- jsonlite::flatten(matchPassesBrutto)
    
    shiny_pass <- matchPassesBrutto %>%
      group_by(matchId, team.name) %>%
      summarise(
        successful_passes = sum(pass.accurate == TRUE, na.rm = TRUE),  # Antal præcise afleveringer
        failed_passes = sum(pass.accurate == FALSE, na.rm = TRUE),  # Antal unøjagtige afleveringer
        total = n()
      ) %>%
      ungroup() %>%  # Fjerner gruppering
      select(
        Team = team.name,
        successful_passes,
        failed_passes,
        total
      )
    
    end_time_m <- Sys.time()  # Stop måling
    time_values$mongo_time <- as.numeric(difftime(end_time_m, start_time_m, units = "secs"))  # Gem tid i sekunder
    
    # print output
    print("Mongo Data:")
    print(shiny_pass)
    
    return(shiny_pass)
  })
  }
  # Opg. 4.2 - UDEN MONGO
  {
    filtered_data4 <- reactive({
      req(input$selected_liga, input$selected_season_pasninger2, input$selected_round_pasninger2, input$selected_match_pasninger2)
    
    start_time_df <- Sys.time()
    
    mlabel2 <- input$selected_match_pasninger2
    
    dfg_pass <- readRDS("all_passes.rds")
    
    shinypasses <- dfg_pass %>%
      group_by(matchId, team.name) %>%  # Gruppering efter kamp og hold
      summarise(
        successful_passes = sum(pass.accurate == TRUE, na.rm = TRUE),
        failed_passes = sum(pass.accurate == FALSE, na.rm = TRUE),
        total = n(),
        .groups = "drop"  # Undgår grupperingsadvarsler
      ) %>%
      ungroup() %>%
      filter(matchId %in% dfg_pass$matchId[dfg_pass$match_info.label == mlabel2]) %>%  # Filtrér efter det ønskede match
      select(
        Team = team.name,
        successful_passes,
        failed_passes,
        total
      )
    
    end_time_df <- Sys.time()  # Stop måling
    time_values$df_time <- as.numeric(difftime(end_time_df, start_time_df, units = "secs"))  # Gem tid i sekunder
    
    # print output
    print("RDS Data efter filter:")
    print(shinypasses)
    
    return(shinypasses)
    })
  }
  # Opg. 4.3
  {
  filtered_data2 <- reactive({
    req(input$selected_liga, input$selected_season, input$selected_round, input$selected_match)  
    shots_data %>%
      filter(liga == input$selected_liga, season == input$selected_season, round == input$selected_round, match == input$selected_match)
  })
  }
  
  # OUTPUT
  # PLOT - Opgave 4.1 – Skud og spillere
  {
    output$skudSpillerPlot <- renderPlot({
      numPlayers <- as.integer(input$numPlayers)
      data <- filtered_data1()
      
      filteredData <- data %>%
        arrange(desc(Sum_Mål)) %>%
        slice_head(n = numPlayers) %>%
        pivot_longer(cols = c(Sum_Mål, mean_XG), names_to = "Type", values_to = "Value")
      
      # Ændrer rækkefølgen af søjlerne
      filteredData$Type <- factor(filteredData$Type, levels = c("Sum_Mål", "mean_XG"))
      
      # Find skalering mellem de to akser
      scale_factor <- max(filteredData$Value[filteredData$Type == "Sum_Mål"], na.rm = TRUE) /
        max(filteredData$Value[filteredData$Type == "mean_XG"], na.rm = TRUE)
      
      ggplot(filteredData, aes(x = reorder(player.name, -Value), fill = Type)) +
        geom_bar(aes(y = ifelse(Type == "Sum_Mål", Value, Value * scale_factor)), 
                 stat = "identity", position = "dodge") +
        geom_text(aes(y = ifelse(Type == "Sum_Mål", Value, Value * scale_factor), 
                      label = round(Value, 1)), 
                  position = position_dodge(width = 0.9), 
                  vjust = -0.5, size = 5) +
        scale_y_continuous(
          name = "Mål",
          sec.axis = sec_axis(~ . / scale_factor, name = "xG")
        ) +
        labs(x = "Spillere",
             fill = "Type") +
        theme_minimal() +
        scale_fill_manual(values = c("Sum_Mål" = "blue", "mean_XG" = "lightblue")) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  }
  # PLOT - Opgave 4.2 – Afleveringer og spillere - MONGO
  {
    output$pasPlot <- renderPlot({
      data <- filtered_data3()  
      
      # Først sorter datarammen inden du bruger pivot_longer
      data_sorted <- data %>%
        mutate(Team = factor(Team, levels = Team[order(successful_passes, decreasing = TRUE)]))
      
      # Derefter transformer til langt format
      data_long <- data_sorted %>%
        pivot_longer(cols = c("failed_passes", "successful_passes", "total"),
                     names_to = "pass_type",
                     values_to = "pasninger")
      
      # Lav plot med de forudbestemte niveauer
      ggplot(data_long, aes(x = Team, y = pasninger, fill = pass_type)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
        geom_text(aes(label = pasninger), position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
        scale_fill_manual(values = c("failed_passes" = "lightblue", "successful_passes" = "blue", "total" = "grey"),
                          labels = c("Fejlslåede", "Succesfulde", "Total")) +
        labs(x = "Hold",
             y = "Antal Afleveringer",
             fill = "Type") +
        theme_minimal() +
        theme(axis.text.x = element_text(hjust = 1))
    })
  }
  # PLOT - Opgave 4.2 – Afleveringer og spillere - UDEN MONGO
  {
    output$pasPlot2 <- renderPlot({
      data2 <- filtered_data4()
      
      # Først sorter datarammen inden du bruger pivot_longer
      data_sorted2 <- data2 %>%
        mutate(match = factor(Team, levels = Team[order(successful_passes, decreasing = TRUE)]))
      
      # Derefter transformer til langt format
      data_long2 <- data_sorted2 %>%
        pivot_longer(cols = c("successful_passes", "failed_passes", "total"),
                     names_to = "pass_type",
                     values_to = "pasninger")
      
      # Lav plot med de forudbestemte niveauer
      ggplot(data_long2, aes(x = Team, y = pasninger, fill = pass_type)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
        geom_text(aes(label = pasninger), position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
        scale_fill_manual(values = c("failed_passes" = "lightblue", "successful_passes" = "blue", "total" = "grey"),
                          labels = c("Fejlslåede", "Succesfulde", "Total")) +
        labs(x = "Hold",
             y = "Antal Afleveringer",
             fill = "Type") +
        theme_minimal() +
        theme(axis.text.x = element_text(hjust = 1))
    })
  }
  # PLOT - Opgave 4.3 – Skud i en kamp
  {
    output$shotPlot <- renderPlot({
      data <- filtered_data2()

      data$team <- factor(data$team)
      data <- data %>%
        mutate(shot_type = ifelse(shots, paste(team, "Mål"), paste(team, "Miss")))
      
      shot_colors <- setNames(rainbow(length(unique(data$shot_type))), unique(data$shot_type))
      
      ggplot(data) +
        annotate_pitch(colour = "white", fill = "springgreen4", limits = FALSE) +
        geom_point(aes(x = xstart, y = ystart, color = shot_type), size = 8) +  # Tilføjer prikker
        scale_color_manual(name = "Skud", values = shot_colors) +
        theme_pitch() +
        theme(panel.background = element_rect(fill = "springgreen4")) +
        coord_flip(xlim = c(49, 101)) +
        scale_y_reverse()
    })
  }
  # Tidstagning
  {
  output$mongo_time_display <- renderText({
    paste("Hentningstid:", round(time_values$mongo_time, 3), "sek")
  })
  
  output$df_time_display <- renderText({
    paste("Hentningstid:", round(time_values$df_time, 3), "sek")
  })
  }
}

# Kør appen
shinyApp(ui = ui, server = server)

