library(tree)
library(dplyr)

# 5.1
query2 <- '{"type.primary": "shot"}'
opg.5 <- congm$find(query = query2, 
                    fields = '{"matchId": 1, "shot": 1, "matchTimestamp": 1, "type": 1, "match_info.competitionId": 1, "team": 1, "player":1, "location":1, "match_info.seasonId": 1}')

aekruhbv <- congm$find(query = query2, 
                    fields = '{"matchId": 1, "shot": 1, "matchTimestamp": 1, "type": 1, "match_info.competitionId": 1, "team": 1, "player":1, "location":1, "match_info.seasonId": 1, "possession": 1}')


opg.5 <- opg.5 %>%
  mutate(liga = case_when(
    sapply(match_info, function(x) x$competitionId == 692) ~ "poland",
    sapply(match_info, function(x) x$competitionId == 635) ~ "holland"
  ))

#svar <- unlist(opg.5$type$secondary)

opg.5 <- opg.5 %>%
  mutate(
    distance_to_goal = sqrt((location$x - 100)^2 + (location$y - 50)^2)
  )

opg.5 <- opg.5 %>%
  mutate(
    angle_to_goal = atan2(abs(location$y - 50), (100 - location$x)) * 180 / pi
  )

opg.5 <- opg.5 %>%
  mutate(
    is_foot = as.factor(ifelse(shot$bodyPart %in% c("right_foot", "left_foot"), 1, 0)
  ))

opg.5 <- opg.5 %>%
  mutate(
    angriber = as.factor(ifelse(player$position %in% c("CF", "SS", "LW", "RW", "LWF", "RWF"), 1, 0)),
    midtbane = as.factor(ifelse(player$position %in% c("AMF", "DMF", "LCMF", "RCMF", "LCMF3", "RCMF3", "LAMF", "RAMF", "LDMF", "RDMF"), 1, 0)),
    forsvar = as.factor(ifelse(player$position %in% c("CB", "LCB", "RCB", "LCB3", "RCB3", "LB", "RB", "LB5", "RB5", "LWB", "RWB"), 1, 0))
  )

opg.5 <- opg.5 %>%
  mutate(
    set_pieces = as.factor(ifelse(type$secondary %in% c("shot_after_throw_in","shot_after_free_kick","shot_after_corner"), 1, 0)
  ))

opg.5 <- opg.5 %>%
  mutate(
    is_goal = as.factor(ifelse(shot$isGoal == TRUE, 1, 0))
  )

opg.5 <- opg.5 %>%
  mutate(
    c = as.factor(ifelse(shot$goalZone %in% c("gc", "bc"), 1, 0)),
    r = as.factor(ifelse(shot$goalZone %in% c("gr", "or", "pr"), 1, 0)),
    l = as.factor(ifelse(shot$goalZone %in% c("gl", "ol", "pl"), 1, 0)),
    t = as.factor(ifelse(shot$goalZone %in% c("gt", "ot", "pt","gtr","ptr","gtl","ptl"), 1, 0)),
    b = as.factor(ifelse(shot$goalZone %in% c("gb", "obr", "olb","gbr","pbr", "obr","glb","plb", "olb"), 1, 0))
  )

opg.5$matchTimestamp <- gsub("^0", "", opg.5$matchTimestamp) 
opg.5$matchTimestamp <- period_to_seconds(hms(opg.5$matchTimestamp))

#opg.5 <- flatten(opg.5)

topg.5 <- opg.5
topg.5$player_id <- topg.5$player$id

afleveringer <- opg.3_pass %>%
  filter(sapply(type$secondary, function(x) "shot_assist" %in% x))

afleveringer$player_id <- afleveringer$pass$recipient$id

afleveringer$matchTimestamp <- gsub("^0", "", afleveringer$matchTimestamp) 
afleveringer$matchTimestamp <- period_to_seconds(hms(afleveringer$matchTimestamp))

topg.5 <- topg.5 %>%
  mutate(
    is_assist_shot = ifelse(
      sapply(1:n(), function(i) {
        any(
          afleveringer$matchId == matchId[i] &
            afleveringer$player_id == player_id[i] &
            abs(afleveringer$matchTimestamp - matchTimestamp[i]) <= 5
        )
      }),
      1,  
      0 
    )
  )

topg.5$is_assist_shot <- as.factor(topg.5$is_assist_shot)

topg.5$duration <- aekruhbv$possession$duration

topg.5 <- topg.5 %>%
  mutate(
    is_long_duration = as.factor(ifelse(duration > 15, 1, 0))
  )

topg.5 <- topg.5 %>%
  mutate(
    x = location$x,
    y = location$y
  )


træning <- topg.5 %>%
  mutate(seasonId = map_chr(match_info, ~ .x$seasonId)) %>%
  filter(seasonId %in% c("186215", "187502"))

test <- topg.5 %>%
  mutate(seasonId = map_chr(match_info, ~ .x$seasonId)) %>%
  filter(seasonId %in% c("188088", "188125"))

træning <- opg.5[c(1:10535),]


# 5.3
træning <- træning %>%
  mutate(
    distance_to_goal_scaled = scale(distance_to_goal),
    angle_to_goal_scaled = scale(angle_to_goal)
  )

træning <- træning %>%
  mutate(
    x = location$x,
    y = location$y
  )

test <- test %>%
  mutate(
    x = location$x,
    y = location$y
  )

glm <- glm(is_goal ~ distance_to_goal + angle_to_goal + is_foot, 
           family = binomial, 
           data = træning)
summary(glm)

predict_glm <- predict(glm, type = "response")

predict_glm_test <- predict(glm, newdata = test)


roc_curve <- roc(træning$is_goal, predict_glm)
plot(roc_curve)
auc(roc_curve)

test$pred <- predict_glm_test
resdf <- data.frame(
  xg = test$shot$xg,
  pred = test$pred,
  mål = test$is_goal,
  pred_mål = ifelse(test$pred > 0.11,1,0),
  wyscout_mål=ifelse(test$shot$xg > 0.11,1,0)
  )

conf_matrix <- confusionMatrix(as.factor(resdf$pred_mål), as.factor(resdf$mål))
conf_matrix

wyscout_matrix <- confusionMatrix(as.factor(resdf$wyscout_mål), as.factor(resdf$mål))
wyscout_matrix

rf <- randomForest(factor(is_goal) ~ distance_to_goal + angle_to_goal + is_foot, 
                   data = træning, 
                   ntree = 500)
varImpPlot(rf)

predict_rf <- predict(rf, newdata=test)

rf_matrix <- confusionMatrix(predict_rf,test$is_goal)
rf_matrix

# tree
tree <- tree(is_goal ~ distance_to_goal + angle_to_goal + is_foot + x + y + is_assist_shot + is_long_duration, 
             data = topg.5)
plot(tree)
text(tree, pretty = 0)
summary(tree)

#prune
cv.tree <- cv.tree(tree, FUN = prune.misclass)
cv.tree

plot(cv.tree$size, cv.tree$dev, type = "b")
plot(cv.tree$k, cv.tree$dev, type = "b")

prune.tree <- prune.misclass(tree, best = 3)
plot(prune.tree)
text(prune.tree, pretty = 0)

tree.pred <- predict(prune.tree, test, type = "class")

tree_matrix <- confusionMatrix(tree.pred, test$is_goal)
tree_matrix




