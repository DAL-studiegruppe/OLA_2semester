library(mongolite)
library(stringr)
library(tidyverse)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(ggsoccer)
library(cowplot)
library(jsonlite)
library(StatsBombR)
library(SBpitch)
library(ggrepel)
library(paletteer)


###############################################################################
###OPGAVE 1.1
conm <- mongo(
  url = "mongodb://localhost",
  db = "fodbold2",
  collection = "matches",)

conp <- mongo(
  url = "mongodb://localhost",
  db = "fodbold2",
  collection = "players",)

cong <- mongo(
  url = "mongodb://localhost",
  db = "fodbold2",
  collection = "games",)

congm <- mongo(
  url = "mongodb://localhost",
  db = "fodbold2",
  collection = "games_matches_combined",)


# •	Der finder ”RKC Waalwijk”’s hjemmekampe. 
{
  dfm <- conm$find(
  query = '{"label": { "$regex": "^RKC Waalwijk" }}',
  fields = "{}"
  )
  
}
# •	Der finder antal skud for den hollandske og polske liga
{
  dfg_holland <- congm$find(
    query = '{"type.primary": "shot", "match_info.competitionId": 635}',
    fields = "{}"
  )
  dfg_holland <- flatten(dfg_holland)
  
  dfg_polen <- congm$find(
    query = '{"type.primary": "shot", "match_info.competitionId": 692}',
    fields = "{}"
  )
  dfg_polen <- flatten(dfg_polen)
  
  # **games indeholder 901 unikke matchId, matches indeholder 905. games_with_players indeholder derfor 901
}
# •	Der finder alle skud fra angribere i alle kampe og printer player samt shot-egenskaberne isGoal og onTarget.
{
    dfg_shot_player <- congp$find(
    query = '{"type.primary": "shot", "player.position": "CF"}',
    fields = '{"player.name": 1, "shot.isGoal": 1, "shot.onTarget": 1}'
  )
  
}
# •	Der finder antal spillere som er over 190 høj.
{
  dfg_height <- congp$find(
    query = '{"height": {"$gt": 190}}',
    fields = '{}'
  )
}
###############################################################################
#### OPGAVE 1.2 – Afleveringer 

# DATA
{
# dfg_pass <- congp$find(
#   query = '{"type.primary": "pass", "match_info.competitionId": 692}',
#   fields = '{}'
# )

# dfg_pass_flat <- flatten(dfg_pass)
# saveRDS(dfg_pass_flat, file = "dfg_pass.rds")
dfg_pass <- readRDS("dfg_pass.rds")
}
dfg_pass <- polishpasses
dfg_pass <- fromJSON(toJSON(dfg_pass),flatten=T)
# • Den gns. længde for afleveringer
{
  ggplot(dfg_pass, aes(x = pass.length, y = ..count.., fill = type.primary)) +
    geom_histogram(binwidth = diff(range(dfg_pass$pass.length)) / 20, 
                   fill = "lightblue", color = "black", position = "identity", alpha = 0.7) +
    scale_x_continuous(breaks = seq(min(dfg_pass$pass.length, na.rm = TRUE), 
                                    max(dfg_pass$pass.length, na.rm = TRUE), 
                                    by = 10)) +  
    labs(title = "Den gns. længde for afleveringer er på 20,1 M",
         subtitle = "Ekstraklasa 2021/2022 - 2022/2023",
         x = "Pasningslængde", 
         y = "Antal afleveringer",
         fill = "Type") +
    theme_minimal()
  
  round(mean(dfg_pass$pass.length), digits = 2)
# Den gennemsnitlige længde for afleveringer ligger på 20,1 meter
}
# • Barplot på nøjagtigheden af afleveringer
{
  dfg_pass_count <- as.data.frame(table(dfg_pass$pass.accurate))
  dfg_pass_count$Perc <- round(dfg_pass_count$Freq / sum(dfg_pass_count$Freq) * 100, digits = 2)
  
  ggplot(dfg_pass_count, aes(x = factor(Var1, labels = c("Fejl = 52238", "Rigtig = 228876")), y = Freq)) +
    geom_bar(stat = "identity", fill = c("lightblue", "lightcoral")) +
    geom_text(aes(label = paste0(round(Perc, 1), "%")), vjust = -0.5, size = 5) +
    labs(title = "Over 81% af afleveringerne er præcise", 
         subtitle = "Ekstraklasa 2021/2022 - 2022/2023",
         x = NULL, 
         y = "Antal afleveringer") +
    theme_minimal()
}
# • Plot på intensiteten af afleveringer
{
  passes_per_minute <- dfg_pass %>%
    group_by(matchPeriod, minute) %>%
    summarise(pass_count = n())
  #fjern overtid
  passes_per_minute <- passes_per_minute[-c(46:59),]
  passes_per_minute <- passes_per_minute[-c(91:107),]
  
  ggplot(passes_per_minute, aes(x = minute, y = pass_count)) +
    geom_line(color = "lightblue", size = 1) +
    labs(title = "Antallet af afleveringer falder jo længere kampen skrider frem",
         subtitle = "Ekstraklasa 2021/2022 - 2022/2023",
         x = "Minut", 
         y = "Antal Afleveringer") +
    scale_x_continuous(breaks = seq(0, 90, by = 15)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 24))
}
# • Hvilken position modtager flest aflevering
{
  pass_recipient <- as.data.frame(table(dfg_pass$pass.recipient.position))
  
  positioner <- data.frame(
    Position = c("GK", "CB", "LCB", "LCB3", "RCB", "RCB3",
                 "LB", "LB5", "LWB", "RB", "RB5", "RWB",
                 "DMF", "LDMF", "RDMF",
                 "LCMF", "LCMF3", "RCMF", "RCMF3",
                 "AMF", "LAMF", "RAMF",
                 "LW", "RW", "LWF", "RWF",
                 "CF", "SS"),
    Group = c("Goalkeeper", "Center Backs", "Center Backs", "Center Backs", "Center Backs", "Center Backs",
              "Fullbacks", "Fullbacks", "Fullbacks", "Fullbacks", "Fullbacks", "Fullbacks",
              "Defensive Midfielders", "Defensive Midfielders", "Defensive Midfielders",
              "Central Midfielders", "Central Midfielders", "Central Midfielders", "Central Midfielders",
              "Attacking Midfielders", "Attacking Midfielders", "Attacking Midfielders",
              "Wingers", "Wingers", "Wingers", "Wingers",
              "Forwards", "Forwards")
  )
  
  colnames(pass_recipient)[colnames(pass_recipient) == "Var1"] <- "Position"
  
  # Flet grupper med data
  merged_positioner <- merge(pass_recipient, positioner, by = "Position")
  
  # Summer afleveringer per gruppe
  merged_grouped <- aggregate(Freq ~ Group, data = merged_positioner, sum)
  
  ggplot(merged_grouped, aes(x = reorder(Group, -Freq), y = Freq, fill = Group)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Freq), vjust = -0.3, size = 5) +
    theme_minimal() +
    labs(title = "Center Backs ligger flest afleveringer",
         subtitle = "Ekstraklasa 2021/2022 - 2022/2023",
         x = "Positionstype",
         y = "Antal afleveringer") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    scale_fill_paletteer_d("ggthemes::Classic_10_Light")  # Ændrer farveskemaet lidt
}
# • Heatmap der viser hvor key passes fører til assist
{
unique_elements <- unique(unlist(dfg_pass$type.secondary))

for (element in unique_elements) {
  dfg_pass[[element]] <- sapply(dfg_pass$type.secondary, function(x) element %in% x)
}

assists_from_keypasses <- dfg_pass %>%
  filter(key_pass == TRUE & assist == TRUE) %>%
  select(location.x, location.y)

# Create pitch with heatmap
create_Pitch(
  grass_colour = "black",
  background_colour = "gray15",
  line_colour = "gray40"
) +
  # Add heatmap layer
  stat_density_2d(
    data = assists_from_keypasses,
    aes(x = location.x, y = location.y, fill = ..density..),
    geom = "tile",
    contour = FALSE,
    alpha = 0.7
  ) +
  # Customize the color scheme
  scale_fill_gradient(
    low = "white",
    high = "lightcoral"
  ) +
  # Add title
  labs(
    title = "Key passes der bliver til en assist",
    subtitle = "Ekstraklasa 2021/2022 - 2022/2023",
    x = "Længde på banen",  
    y = "Bredde på banen"   
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 120), ylim = c(0, 80)) +
  theme(legend.position = "none")  # Fjerner legend
}

######################
# FOR LEGIA
# • Den gns. længde for afleveringer
{
  Legia_Warszawa <- dfg_pass %>% 
    filter(grepl("Legia Warszawa", match_info.label))
  
  ggplot(Legia_Warszawa, aes(x = pass.length, y = ..count.., fill = type.primary)) +
    geom_histogram(binwidth = diff(range(Legia_Warszawa$pass.length)) / 20, 
                   fill = "lightblue", color = "black", position = "identity", alpha = 0.7) +
    scale_x_continuous(breaks = seq(min(Legia_Warszawa$pass.length, na.rm = TRUE),
                                    max(Legia_Warszawa$pass.length, na.rm = TRUE),
                                    by = 10)) +
    labs(title = "Den gns. længde for afleveringer er på 19,7 M",
         subtitle = "Ekstraklasa 2021/2022 - 2022/2023",
         x = "Pasningslængde", 
         y = "Antal afleveringer") +
    theme_minimal()
  
  round(mean(Legia_Warszawa$pass.length), digits = 1)
  # Den gennemsnitlige længde for afleveringer ligger på 19,74 meter
}
# • Barplot på nøjagtigheden af afleveringer
{
  legia_pass_count <- as.data.frame(table(Legia_Warszawa$pass.accurate))
  legia_pass_count$Perc <- round(legia_pass_count$Freq / sum(legia_pass_count$Freq) * 100, digits = 2)
  
  ggplot(legia_pass_count, aes(x = factor(Var1, labels = c("Fejl = 5839", "Rigtig = 27096")), y = Freq)) +
    geom_bar(stat = "identity", fill = c("lightblue", "lightcoral")) +
    geom_text(aes(label = paste0(round(Perc, 1), "%")), vjust = -0.5, size = 5) +
    labs(title = "Over 82% af Legias afleveringer var præcise", 
         subtitle = "Ekstraklasa 2021/2022 - 2022/2023",
         x = NULL, 
         y = "Antal afleveringer") +
    theme_minimal()
}
# • Plot på intensiteten af afleveringer

{
  legia_passes_per_minute <- Legia_Warszawa %>%
    group_by(matchPeriod, minute) %>%
    summarise(pass_count = n())
 #fjern overtid
  legia_passes_per_minute <- legia_passes_per_minute[-c(46:59),]
  legia_passes_per_minute <- legia_passes_per_minute[-c(91:104),]  
   
  ggplot(legia_passes_per_minute, aes(x = minute, y = pass_count)) +
    geom_line(color = "lightblue", size = 1) +
    labs(title = "Legias afleveringer falder jo længere kampen skrider frem",
         subtitle = "Ekstraklasa 2021/2022 - 2022/2023",
         x = "Minut", 
         y = "Antal Afleveringer") +
    scale_x_continuous(breaks = seq(0, 90, by = 15)) +
    theme_minimal() +
    theme(plot.title = element_text(size = 24))
  
}
# • Hvilken position modtager flest aflevering
{
  legia_pass_recipient <- as.data.frame(table(Legia_Warszawa$pass.recipient.position))
  
  legia_positioner <- data.frame(
    Position = c("GK", "CB", "LCB", "LCB3", "RCB", "RCB3",
                 "LB", "LB5", "LWB", "RB", "RB5", "RWB",
                 "DMF", "LDMF", "RDMF",
                 "LCMF", "LCMF3", "RCMF", "RCMF3",
                 "AMF", "LAMF", "RAMF",
                 "LW", "RW", "LWF", "RWF",
                 "CF", "SS"),
    Group = c("Goalkeeper", "Center Backs", "Center Backs", "Center Backs", "Center Backs", "Center Backs",
              "Fullbacks", "Fullbacks", "Fullbacks", "Fullbacks", "Fullbacks", "Fullbacks",
              "Defensive Midfielders", "Defensive Midfielders", "Defensive Midfielders",
              "Central Midfielders", "Central Midfielders", "Central Midfielders", "Central Midfielders",
              "Attacking Midfielders", "Attacking Midfielders", "Attacking Midfielders",
              "Wingers", "Wingers", "Wingers", "Wingers",
              "Forwards", "Forwards")
  )
  
  colnames(legia_pass_recipient)[colnames(legia_pass_recipient) == "Var1"] <- "Position"
  
  # Flet grupper med data
  legia_merged_positioner <- merge(legia_pass_recipient, legia_positioner, by = "Position")
  
  # Summer afleveringer per gruppe
  legia_merged_grouped <- aggregate(Freq ~ Group, data = legia_merged_positioner, sum)
  
  ggplot(legia_merged_grouped, aes(x = reorder(Group, -Freq), y = Freq, fill = Group)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = Freq), vjust = -0.3, size = 5) +
    theme_minimal() +
    labs(title = "Legias Center Backs bidrager med flest afleveringer",
         subtitle = "Ekstraklasa 2021/2022 - 2022/2023",
         x = "Positionstype",
         y = "Antal afleveringer") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
    scale_fill_paletteer_d("ggthemes::Classic_10_Light")
}
# • Heatmap der viser hvor key passes fører til assist
{
  result_warszawa <- Legia_Warszawa %>%
    filter(key_pass == TRUE, assist == TRUE) %>%  # Filtrer for key passes og assists
    summarise(count = n())  # Tæl antallet
  
  assists_from_keypasses_warszawa <- Legia_Warszawa %>%
    filter(key_pass == TRUE & assist == TRUE) %>%
    select(location.x, location.y)
  
  create_Pitch(
    grass_colour = "black",
    background_colour = "gray15",
    line_colour = "gray40"
  ) +
    # Add heatmap layer
    stat_density_2d(
      data = assists_from_keypasses_warszawa,
      aes(x = location.x, y = location.y, fill = ..density..),
      geom = "tile",
      contour = FALSE,
      alpha = 0.7
    ) +
    # Customize the color scheme
    scale_fill_gradient(
      low = "white",
      high = "lightcoral"
    ) +
    # Add title
    labs(
      title = "Legia - Key passes der bliver til en assist",
      x = "Længde på banen",  # Ændrer x-akse label
      y = "Bredde på banen"   # Ændrer y-akse label
    ) +
    theme_minimal() +
    coord_cartesian(xlim = c(0, 120), ylim = c(0, 85)) +
    theme(legend.position = "none")  # Fjerner legend
}

  # Opgave 1.3 – Skud og mål
  # Lav 5-10 figurer, der giver en fodboldtræner et overblik over skud, herunder specielt mål, registeret som 
  # events i filerne, registeret som events for kampe i den polske liga fra sæsonstart 2021/2022 til seneste 
  # observation i sæsonen 2022/2023. Herunder finder I eksempler på, hvad I kunne se nærmere på:
  # a)	Den gennemsnitlige længde og vinkel til målet for skuddene
  # b)	Andel af skud, der bliver til mål i forhold til antallet af skud
  # c)	Den gennemsnitlige xG for alle skud, der ramte målet
  # d)	Etc.
  # Det forventes i laver flere figurer end punkterne herover, der alene er til inspiration og ikke en fyldestgørende besvarelse. 
  # (Hint: I kan med fordel overveje at bruge et andet visualiseringsværktøj end R, men I bestemmer selv)

# DATA
{
  # dfg_shot <- cong$find(
  # query = '{"type.primary": "shot", "match_info.competitionId": 692}',
  # fields = '{}' )
  #  
 # dfg_shot_flat <- flatten(dfg_shot)
 # saveRDS(dfg_shot_flat, file = "dfg_shot_polen.rds")
dfg_shot <- readRDS("dfg_shot_polen.rds")  
}
dfg_shot <- polishshots
dfg_shot <- fromJSON(toJSON(dfg_shot),flatten=T)
# • Den gennemsnitlige længde og vinkel til målet for skuddene
{
distances <- numeric(nrow(dfg_shot))

# Loop gennem hver række
for(i in 1:nrow(dfg_shot)) {
  x_forskel <- 100 - dfg_shot$location.x[i]
  y_forskel <- 50 - dfg_shot$location.y[i]
  # Beregn afstand med Pythagoras
  distances[i] <- sqrt(x_forskel^2 + y_forskel^2)}
  
dfg_shot$shot_distance <- distances

mean(dfg_shot$shot_distance)
}
# • Den gennemsnitlige vinkel til målet for skuddene
{
dfg_shot$vinkel <- atan2(dfg_shot$location.y, dfg_shot$location.x) * (180 / pi)

  ggplot(dfg_shot) +
    annotate_pitch(colour = "white",
                   fill   = "springgreen4",
                   limits = FALSE) +
    geom_point(aes(x = location.x, y = location.y),
               colour = "lightyellow",
               size = 1) +
    geom_point(aes(x = mean(dfg_shot$location.x), 
                   y = mean(dfg_shot$location.y)),
               colour = "lightcoral",
               size = 3) +  # Gennemsnitspunktet
    geom_text(aes(x = mean(dfg_shot$location.x), 
                  y = mean(dfg_shot$location.y)),
              label = "Gns. skudposition",
              colour = "black",
              fontface = "bold",
              size = 6,
              vjust = -1) +  # Justerer teksten over punktet
    theme_pitch() +
    theme(panel.background = element_rect(fill = "springgreen4")) +
    coord_flip(xlim = c(49, 101)) +
    scale_y_reverse() +
    ggtitle("Skud positioner i den polske liga",
            "Ekstraklasa 2021/2022 - 2022/2023")

}

# • Andel af skud, der bliver til mål i forhold til antallet af skud
{
# Shot onTarget isGoal
shot_onTarget_isGoal <- dfg_shot %>%
  filter(shot.onTarget == TRUE & shot.isGoal == TRUE) %>%
  select(location.x, location.y)

ggplot(shot_onTarget_isGoal) +
  annotate_pitch(colour = "white",
                 fill   = "springgreen4",
                 limits = FALSE) +
  geom_point(aes(x = location.x, y = location.y),
             colour = "lightyellow",
             size = 1) +
  geom_point(aes(x = mean(shot_onTarget_isGoal$location.x), 
                 y = mean(shot_onTarget_isGoal$location.y)),
             colour = "lightcoral",
             size = 3) +  # Gennemsnitspunktet
  geom_text(aes(x = mean(shot_onTarget_isGoal$location.x), 
                y = mean(shot_onTarget_isGoal$location.y)),
            label = "Gns. skudposition",
            colour = "black",
            fontface = "bold",
            size = 6,
            vjust = -1) +  # Justerer teksten over punktet
  theme_pitch() +
  theme(panel.background = element_rect(fill = "springgreen4")) +
  coord_flip(xlim = c(49, 101)) +
  scale_y_reverse() +
  ggtitle("Positioner hvor skud bliver til mål",
          "Ekstraklasa 2021/2022 - 2022/2023")
}  
# • Andel af skud, der ikke bliver til mål i forhold til antallet af skud
{
shot_onTarget_notGoal <- dfg_shot %>%
  filter(shot.onTarget == TRUE & shot.isGoal == FALSE) %>%
  select(location.x, location.y)

ggplot(shot_onTarget_notGoal) +
  annotate_pitch(colour = "white",
                 fill   = "springgreen4",
                 limits = FALSE) +
  geom_point(aes(x = location.x, y = location.y),
             colour = "lightyellow",
             size = 1) +
  geom_point(aes(x = mean(shot_onTarget_notGoal$location.x), 
                 y = mean(shot_onTarget_notGoal$location.y)),
             colour = "lightcoral",
             size = 3) +  # Gennemsnitspunktet
  geom_text(aes(x = mean(shot_onTarget_notGoal$location.x), 
                y = mean(shot_onTarget_notGoal$location.y)),
            label = "Gns. skudposition",
            colour = "black",
            fontface = "bold",
            size = 6,
            vjust = -1) +  # Justerer teksten over punktet
  theme_pitch() +
  theme(panel.background = element_rect(fill = "springgreen4")) +
  coord_flip(xlim = c(49, 101)) +
  scale_y_reverse() +
  ggtitle("Positioner hvor skudet rammer målet, men IKKE går i ind",
          "Ekstraklasa 2021/2022 - 2022/2023")

}
# • Fordeling af skud baseret på xG
{
  goals_shot_xg <- dfg_shot %>% 
    filter(shot.isGoal == TRUE) %>% 
    select(shot.xg) 
  
  ggplot(goals_shot_xg, aes(x = shot.xg)) +
    geom_histogram(bins = 25, fill = "lightblue", alpha = 0.7, color = "black") +
    stat_bin(bins = 25, geom = "text", aes(label = ..count..), vjust = -0.5, size = 5) +
    labs(title = "Den gns. xG ved mål lå på 0,24",
         subtitle = "Ekstraklasa 2021/2022 - 2022/2023",
         x = "xG",
         y = "Antal mål") +
    scale_x_continuous(breaks = seq(0, max(goals_shot_xg$shot.xg, na.rm = TRUE), by = 0.05)) +
    theme_minimal() +
    theme(text = element_text(size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1))
}
#Legia 


# • Legia Warszawa spiller præstation ift. xG
{
  # Filtrer for kun Legia Warszawa
  legia_shots <- dfg_shot %>%
    filter(team.name == "Legia Warszawa" | opponentTeam.name == "Legia Warszawa")

  legia_xg_vs_goals <- legia_shots %>%
    group_by(player.name) %>%
    summarise(
      total_xG = mean(shot.xg, na.rm = TRUE),
      actual_goals = sum(shot.isGoal, na.rm = TRUE),
      .groups = "drop"  # Fjerner gruppering for at undgå advarsel
    ) %>%
    filter(actual_goals > 2)

  ggplot(legia_xg_vs_goals, aes(x = total_xG, y = actual_goals, label = player.name)) +
    geom_point(aes(color = actual_goals), size = 5) +
    geom_text_repel(size = 5, box.padding = 0.5, max.overlaps = Inf) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    scale_color_gradient2(low = "black", mid = "white", high = "red3", midpoint = 0) +
    labs(title = "Legia spilleres præstationer ud fra mål & xG",
         subtitle = "Ekstraklasa 2021/2022 - 2022/2023",
         x = "Forventede mål (xG)",
         y = "Mål",
         color = "Mål") +
    theme_minimal() +
    theme(text = element_text(size = 14))
}
# • Legia skudfordeling per halvleg * INKLUDER IKKE
{
  legia_shot_distribution <- legia_shots %>%
    group_by(matchPeriod) %>%
    summarise(
      total_shots = n(),
      goals_scored = sum(shot.isGoal, na.rm = TRUE)  # Tæl antal mål
    ) %>%
    arrange(matchPeriod)
  
  legia_shot_distribution_long <- legia_shot_distribution %>%
    pivot_longer(cols = c(total_shots, goals_scored), names_to = "Type", values_to = "Antal")

  ggplot(legia_shot_distribution_long, aes(x = matchPeriod, y = Antal, fill = Type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6, alpha = 0.8) +
    scale_fill_manual(values = c("total_shots" = "blue", "goals_scored" = "green"), 
                      labels = c("Mål", "Skud")) +
    labs(title = "Flere skud bliver til mål i 2. halvleg | Legia Warszawa",
         subtitle = "Ekstraklasa 2021/2022 - 2022/2023",
         x = "Halvleg",
         y = "Antal",
         fill = "Type") +
    theme_minimal() +
    theme(text = element_text(size = 14))
}
# • Shot goalZone vs shotXg * PowerPoint BILLEDE
{
  goalZone_vs_xG <- dfg_shot %>%
    group_by(shot.goalZone) %>%
    summarise(
      total_xG = mean(shot.xg, na.rm = TRUE))
  
  goal_zone_labels <- c("olt", "ot", "ort", "ol", "glt", "gt", "grt", "gl", "gc", "gr", 
                        "olb", "glb", "gb", "grb", "or", "orb")

  filtered_goalZone_vs_xG <- goalZone_vs_xG[goalZone_vs_xG$shot.goalZone %in% goal_zone_labels, ]
  filtered_goalZone_vs_xG$total_xG <- round(filtered_goalZone_vs_xG$total_xG, digits = 2)
  
}
# • Fordeling af mål ud for kamp periode
#LEGIA
{
  Legiagoals <- legia_shots %>% 
    filter(shot.isGoal == TRUE) %>% 
    select(matchId, minute, matchPeriod) %>%
    # Filtrér mål baseret på matchPeriod og minut
    filter((matchPeriod == "1H" & minute <= 45) | 
             (matchPeriod == "2H" & minute >= 45 & minute <= 90)) %>%
    # Omkod matchPeriod til læsbare labels
    mutate(matchPeriod = recode(matchPeriod, 
                                "1H" = "1. Halvleg", 
                                "2H" = "2. Halvleg"))
  
  count(Legiagoals, Legiagoals$matchPeriod == "1. Halvleg")
  
  ggplot(data.frame(Legiagoals), aes(x = minute, fill = matchPeriod)) +
    geom_bar(position = "dodge", color = "black", alpha = 0.7) +
    labs(title = "Legia Warszawa scorer næsten lige mange mål pr. halvleg - Ordinær spilletid",
         subtitle = "Ekstraklasa 2021/2022 - 2022/2023",
         x = "Minut", 
         y = "Antal mål",
         fill = "Halvleg") +
    scale_x_continuous(breaks = seq(0, 100, by = 3)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    annotate("label", x = 100, y = Inf,  
             label = "1. halvleg = 53 mål | 2. halvleg = 55 mål", 
             fill = "lightgrey", color = "black", size = 4, fontface = "bold", hjust = 1, vjust = 1)
  
  #LIGAEN
  goals <- dfg_shot %>% 
    filter(shot.isGoal == TRUE) %>% 
    select(matchId, minute, matchPeriod) %>%
    # Filtrér mål baseret på matchPeriod og minut
    filter((matchPeriod == "1H" & minute <= 45) | 
             (matchPeriod == "2H" & minute >= 45 & minute <= 90)) %>%
    # Omkod matchPeriod til læsbare labels
    mutate(matchPeriod = recode(matchPeriod, 
                                "1H" = "1. Halvleg", 
                                "2H" = "2. Halvleg"))
  
  count(goals, goals$matchPeriod == "1. Halvleg")
  
  
  ggplot(data.frame(goals), aes(x = minute, fill = matchPeriod)) +
    geom_bar(position = "dodge", color = "black", alpha = 0.7) +
    labs(title = "Flest mål scores i 2. halvleg - Ordinær spilletid",
         subtitle = "Ekstraklasa 2021/2022 - 2022/2023",
         x = "Minut", 
         y = "Antal mål",
         fill = "Halvleg") +
    scale_x_continuous(breaks = seq(0, 100, by = 3)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    annotate("label", x = 100, y = Inf,  
             label = "1. halvleg = 435 mål | 2. halvleg = 500 mål", 
             fill = "lightgrey", color = "black", size = 4, fontface = "bold", hjust = 1, vjust = 1)
  
}

###Mål pr formation###
#########################################################################

# Tæl mål for hver formation
goal_formations <- dfg_shot %>%
  group_by(team.formation) %>%
  summarise(
    total_goals = sum(shot.isGoal, na.rm = TRUE),  # Antal mål
    total_occurrences = n()  # Antal gange formationen forekommer
  )

# Tag top 10 formationer med flest mål
T10_Gformations <- goal_formations %>%
  arrange(desc(total_goals)) %>%  # Sorter efter antal
  slice_head(n = 10)  # Tag de 10 øverste

T10_Gformations <- T10_Gformations %>%
  rename(Formation = "team.formation")

# Plot top 10 formationer med flest mål i Ligaen
ggplot(T10_Gformations, aes(x = reorder(Formation, -total_goals), y = total_goals, fill = Formation)) +
  geom_bar(stat = "identity") +
  scale_fill_paletteer_d("MexBrewer::Alacena") +  # Anvend Acadia farver
  labs(title = "Den offensive 3-4-3 scorer flest mål", 
       subtitle = "Ekstraklasa 2021/2022 - 2022/2023",
       x = "Opstilling",
       y = "Antal mål") +
  theme_minimal() +
  theme(legend.position = "none")

# LEGIA
# Tæl mål for hver formation
Legiagoal_formations <- legia_shots %>%
  group_by(team.formation) %>%
  summarise(
    total_goals = sum(shot.isGoal, na.rm = TRUE),  # Antal mål
    total_occurrences = n()  # Antal gange formationen forekommer
  )

# Tag top 10 formationer med flest mål
LT10_Gformations <- Legiagoal_formations %>%
  arrange(desc(total_goals)) %>%  # Sorter efter antal
  slice_head(n = 10)  # Tag de 10 øverste

LT10_Gformations <- LT10_Gformations %>%
  rename(Formation = "team.formation")

# Plot top 10 formationer med flest mål for Legia Warszawa
ggplot(LT10_Gformations, aes(x = reorder(Formation, -total_goals), y = total_goals, fill = Formation)) +
  geom_bar(stat = "identity") +
  scale_fill_paletteer_d("MexBrewer::Alacena") +  # Anvend Acadia farver
  labs(title = "Legia scorede oftest med 4-2-3-1 formationen",
       subtitle = "Ekstraklasa 2021/2022 - 2022/2023",
       x = "Opstilling",
       y = "Antal mål") +
  theme_minimal() +
  theme(legend.position = "none")

sum(legiashots$shot$isGoal==TRUE)
sum(legia_shots$shot.isGoal==TRUE)
min(legia_shots$match_info.date)
max(legiashots$match_info$date)

sum(polishshots$shot$isGoal==TRUE)
sum(dfg_shot$shot.isGoal==TRUE)
