library(dplyr)
library(jsonlite)
library(stringr)
library(tidyr)
library(mongolite)
library(lubridate)

#connect to mongo
cong=mongo(
  collection = "games",
  db = "wyscoutdutch",
  url= "mongodb://localhost"
)

conm=mongo(
  collection = "matches",
  db = "wyscoutdutch",
  url= "mongodb://localhost"
)
conp=mongo(
  collection = "players",
  db = "wyscoutdutch",
  url= "mongodb://localhost"
)

congm=mongo(
  collection = "games_matches",
  db = "wyscoutdutch",
  url= "mongodb://localhost"
)

allmatches = conm$find(query = '{}',fields = '{}')
allplayer <- conp$find(query = '{}',fields = '{}')

# test
test_query <- '{}'
test_df <- congm$find(query = test_query, limit = 5)

pass_test <- '{"type.primary": "pass"}'
pass_df <- congm$find(query = pass_test, limit = 5)

type_test <- '{"type": {"$exists": true}}'
type_df <- congm$find(query = type_test, limit = 1)

date_test <- congm$find(query = '{}', limit = 5)
date_test$date

season_check <- '{
  "match_info.seasonId": { "$in": [186215, 187502] }
}'
test_season <- congm$find(query = season_check, limit = 5)

# hente kampene for 2021/2022 sæson, seasonId 186215 er for poland og 187502 er for holland

match_query <- '{
  "seasonId": { "$in": [186215, 187502] }
}'

ss.matches <- conm$find(query = match_query, fields = '{"_id": 1}')


aflevering_q <- sprintf('{
  "matchId": { "$in": [%s] },
  "type.primary": "pass"
}', paste(ss.matches$`_id`, collapse = ","))

opg.2 <- congm$find(query = aflevering_q, 
                    fields = '{"matchId": 1, "pass": 1, "matchTimestamp": 1, "type": 1, "match_info.competitionId": 1, "team": 1, "player": 1, "location":1}')

#write.csv(opg.2, "afleveringer_opg.2.csv", row.names=F)
#saveRDS(opg.2,"afleveringer_opg.2.rds")

pl_df <- opg.2 %>%
  filter(sapply(match_info, function(x) x$competitionId == 692))

dc_df <- opg.2 %>%
  filter(sapply(match_info, function(x) x$competitionId == 635))

pl_df_assist <- pl_df %>%
  filter(sapply(type$secondary, function(x) "assist" %in% x))

dc_df_assist <- dc_df %>%
  filter(sapply(type$secondary, function(x) "assist" %in% x))

# opg.2.1 afleveringer

pl_passes <- pl_df %>%
  group_by(team$name) %>%
  summarise(
    total_passes = n(),
    accurate_passes = sum(pass$accurate == TRUE, na.rm = TRUE)
  ) 

pl_passes$pass_pct <- pl_passes$accurate_passes/pl_passes$total_passes


dc_passes <- dc_df %>%
  group_by(team$name) %>%
  summarise(
    total_passes = n(),
    accurate_passes = sum(pass$accurate == TRUE, na.rm = TRUE)
  ) 

dc_passes$pass_pct <- dc_passes$accurate_passes/dc_passes$total_passes

dc_top_bottom <- bind_rows(
  # Top 5
  dc_passes %>%
    filter(`team$name` %in% c("Ajax", "PSV", "Feyenoord", "Twente", "AZ")),
  # Bund 5
  dc_passes %>%
    filter(`team$name` %in% c("Willem II", "Heracles", "PEC Zwolle", "Fortuna Sittard", "Sparta Rotterdam"))
)

pl_top_bottom <- bind_rows(
  # Top 5
  pl_passes %>%
    filter(`team$name` %in% c("Lech Poznań", "Raków Częstochowa", "Pogoń Szczecin", "Lechia Gdańsk", "Piast Gliwice")),
  # Bund 5
  pl_passes %>%
    filter(`team$name` %in% c("Górnik Łęczna", "Wisła Kraków", "Bruk-Bet Termalica", "Śląsk Wrocław", "Stal Mielec"))
)

# opg.2.2 afleveringer sidst i kampene, gsub 0'er og konverter til sec, så 80min=4800s, derefter kan man filtere

test_pl <- pl_df
test_dc <- dc_df

test_pl$matchTimestamp <- gsub("^0", "", test_pl$matchTimestamp) 
test_pl$matchTimestamp <- period_to_seconds(hms(test_pl$matchTimestamp))

test_dc$matchTimestamp <- gsub("^0", "", test_dc$matchTimestamp) 
test_dc$matchTimestamp <- period_to_seconds(hms(test_dc$matchTimestamp))

opg2.2_pl <- test_pl %>%
  mutate(period = if_else(matchTimestamp > 4800, "efter_80min", "før_80min")) %>%
  group_by(team$name, period) %>%
  summarise(
    total_passes = n(),
    pass_pct = sum(pass$accurate == TRUE, na.rm = TRUE)/total_passes
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = c(total_passes, pass_pct)
  )

opg2.2_pl$forskel_pct <- opg2.2_pl$pass_pct_før_80min-opg2.2_pl$pass_pct_efter_80min

opg2.2_dc <- test_dc %>%
  mutate(period = if_else(matchTimestamp > 4800, "efter_80min", "før_80min")) %>%
  group_by(team$name, period) %>%
  summarise(
    total_passes = n(),
    pass_pct = sum(pass$accurate == TRUE, na.rm = TRUE)/total_passes
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = c(total_passes, pass_pct)
  )

opg2.2_dc$forskel_pct <- opg2.2_dc$pass_pct_før_80min-opg2.2_dc$pass_pct_efter_80min

# assist længde
dc_u_assist <- anti_join(dc_df,dc_df_assist,  by = "_id")
mean(dc_u_assist$pass$length) # 20,0666
mean(dc_df_assist$pass$length) # 19,31347

pl_u_assist <- anti_join(pl_df,pl_df_assist,  by = "_id")
mean(pl_u_assist$pass$length) # 19,92643
mean(pl_df_assist$pass$length) # 20,29219

# assist spiller
player_p <- as.data.frame(allplayer$`_id`)
player_p$position <- allplayer$role$name
colnames(player_p)[1] <- "player_id"

dc_df_assist$player_id <- dc_df_assist$player$id
dc_df_assist$player_id <- as.factor(dc_df_assist$player_id)
dc_df_assist <- right_join(positioner,dc_df_assist,by = "player_id")

pl_df_assist$player_id <- pl_df_assist$player$id
pl_df_assist$player_id <- as.factor(pl_df_assist$player_id)
pl_df_assist <- right_join(positioner, pl_df_assist, by = "player_id")

dc_p_df <- dc_df_assist %>% group_by(final_position) %>% summarise(total_assist <- n())
pl_p_df <- pl_df_assist %>% group_by(final_position) %>% summarise(total_assist <- n())

# spiller der laver flest assists
pldcxa <- data.frame(
  players = opg.3_shot$player$name,
  xa = opg.3_shot$shot$xg,
  player_id = opg.3_shot$player_id)

pldcxa <- pldcxa %>%
  group_by(players,player_id) %>%
  summarise(xa = mean(xa))

pldcxa <- right_join(positioner,pldcxa,by = "player_id")


dc_df_assist$players <- dc_df_assist$pass$recipient$name
pl_df_assist$players <- pl_df_assist$pass$recipient$name

dc_xa <- left_join(dc_df_assist, pldcxa, by = "players", relationship = "many-to-many")

pl_xa <- left_join(pl_df_assist, pldcxa, by = "players", relationship = "many-to-many")

  
dc_s_df <- dc_xa %>% group_by(player$name) %>% summarise(total_assist <- n(), gns_xa <- mean(xa), position=final_position)

pl_s_df <- pl_xa %>% group_by(player$name) %>% summarise(total_assist <- n(), gns_xa <- mean(xa), position=final_position)


# test
tdf <- dc_df
ttdf <- anti_join(pl_df_assist,player_p, by = "player_id" )

# opg2.2
aflevering_k <- sprintf('{
  "matchId": { "$in": [%s] },
  "type.primary": "shot"
}', paste(ss.matches$`_id`, collapse = ","))

opg.2_skud <- congm$find(query = aflevering_k, 
                    fields = '{"matchId": 1, "shot": 1, "matchTimestamp": 1, "type": 1, "match_info.competitionId": 1, "team": 1, "player": 1, "location": 1}')


pl_skud <- opg.2_skud %>%
  filter(sapply(match_info, function(x) x$competitionId == 692)) %>%
  group_by(team$name) %>%
  summarise(
    total_shot = n(),
    total_goal = sum(shot$isGoal == TRUE, na.rm = TRUE)
  ) 

dc_skud <- opg.2_skud %>%
  filter(sapply(match_info, function(x) x$competitionId == 635)) %>%
  group_by(team$name) %>%
  summarise(
    total_shot = n(),
    total_goal = sum(shot$isGoal == TRUE, na.rm = TRUE)
  ) 

dcs_top_bottom <- bind_rows(
  # Top 5
  dc_skud %>%
    filter(`team$name` %in% c("Ajax", "PSV", "Feyenoord", "Twente", "AZ")),
  # Bund 5
  dc_skud %>%
    filter(`team$name` %in% c("Willem II", "Heracles", "PEC Zwolle", "Fortuna Sittard", "Sparta Rotterdam"))
)

pls_top_bottom <- bind_rows(
  # Top 5
  pl_skud %>%
    filter(`team$name` %in% c("Lech Poznań", "Raków Częstochowa", "Pogoń Szczecin", "Lechia Gdańsk", "Piast Gliwice")),
  # Bund 5
  pl_skud %>%
    filter(`team$name` %in% c("Górnik Łęczna", "Wisła Kraków", "Bruk-Bet Termalica", "Śląsk Wrocław", "Stal Mielec"))
)


pl_skud$matchTimestamp <- gsub("^0", "", pl_skud$matchTimestamp) 
pl_skud$matchTimestamp <- period_to_seconds(hms(pl_skud$matchTimestamp))

dc_skud$matchTimestamp <- gsub("^0", "", dc_skud$matchTimestamp) 
dc_skud$matchTimestamp <- period_to_seconds(hms(dc_skud$matchTimestamp))

pl_xg <- pl_skud %>%  
  mutate(period = if_else(matchTimestamp > 4800, "efter_80min", "før_80min")) %>%
  group_by(period) %>%
  summarise(
    antal_skud = n(),
    gns_xg = mean(shot$xg, na.rm = TRUE),
    antal_mål = sum(shot$isGoal == TRUE, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = c(antal_skud, gns_xg, antal_mål)
  )

dc_xg <- dc_skud %>%  
  mutate(period = if_else(matchTimestamp > 4800, "efter_80min", "før_80min")) %>%
  group_by(period) %>%
  summarise(
    antal_skud = n(),
    gns_xg = mean(shot$xg, na.rm = TRUE),
    antal_mål = sum(shot$isGoal == TRUE, na.rm = TRUE)
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = c(antal_skud, gns_xg, antal_mål)
  )

# skud længde
x=100
y=50

pl_goal_length <- pl_skud %>%
  mutate(
    shot_length = sqrt((x - location$x)^2 + (y - location$y)^2) * 105/100,
    is_goal = if_else(shot$isGoal == TRUE, "Mål", "Ikke mål")
  ) %>%
  group_by(is_goal) %>%
  summarise(
    antal_skud = n(),
    gns_længde = mean(shot_length, na.rm = TRUE),
    gns_xg = mean(shot$xg, na.rm = TRUE)
  )

dc_goal_length <- dc_skud %>%
  mutate(
    shot_length = sqrt((x - location$x)^2 + (y - location$y)^2) * 105/100,
    is_goal = if_else(shot$isGoal == TRUE, "Mål", "Ikke mål")
  ) %>%
  group_by(is_goal) %>%
  summarise(
    antal_skud = n(),
    gns_længde = mean(shot_length, na.rm = TRUE),
    gns_xg = mean(shot$xg, na.rm = TRUE)
  )

# position
opg.3_shot$player_id <- opg.3_shot$player$id
opg.3_shot$player_id <- as.factor(opg.3_shot$player_id)

pp_skud <- opg.3_shot %>%
  filter(shot$isGoal == TRUE) %>%
  group_by(player_id) %>%
  right_join(positioner,opg.3_shot,by="player_id")%>%
  group_by(final_position) %>%
  summarise(total_shot <- n()
            )
            



# spiller med flest mål og deres gns.xG
opg.3_shot <- opg.3_shot %>%
  mutate(liga = case_when(
    sapply(match_info, function(x) x$competitionId == 692) ~ "poland",
    sapply(match_info, function(x) x$competitionId == 635) ~ "holland"
  ))


fm_skud <- opg.3_shot %>%
  filter(shot$isGoal == TRUE) %>%
  group_by(player$name) %>%
  summarise(
    total_goal = n(),
    gns.xG = mean(shot$xg),
    liga = first(liga),
    .groups = 'drop'
  )
