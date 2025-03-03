library(factoextra)
library(plotly)

# hente file
query <- '{"type.primary": "pass"}'
query2 <- '{"type.primary": "shot"}'


opg.3_pass <- congm$find(
  query = query,
  fields = '{"matchId": 1, 
            "pass": 1, 
            "matchTimestamp": 1, 
            "type": 1, 
            "match_info.competitionId": 1, 
            "team": 1,
            "player": 1}'
)

opg.3_shot <- congm$find(query = query2, 
                         fields = '{"matchId": 1, "shot": 1, "matchTimestamp": 1, "type": 1, "match_info.competitionId": 1, "team": 1, "player":1, "location":1}')

#saveRDS(opg.3_pass, file = "all_passes.rds")

opg.3 <- readRDS("afleveringer_opg.2.rds")

# samelt aflevering ift. spiller
sp_opg.3 <- opg.3 %>%
  group_by(player$id,player$name) %>%
  summarise(
    antal_afleveringer = n(),
    gennemsnit_længde = mean(pass$length, na.rm = TRUE),
    #gennemsnit_vinkel = mean(pass$angle, na.rm = TRUE),
    pass_gns = mean(pass$accurate == TRUE, na.rm = TRUE) * 100
  ) %>%
  na.omit()

sp_opg.3 <- sp_opg.3[sp_opg.3$antal_afleveringer >= 400, ]
sp_opg.3$antal_afleveringer <- as.numeric(sp_opg.3$antal_afleveringer)

#af_opg.3 <- opg.3 %>%
  #reframe(
    #længde = pass$length,
    #vinkel = pass$angle,
    #pass.x = pass$endLocation$x,
    #pass.y = pass$endLocation$y
  #) %>%
  #na.omit()

#af_opg.3_scaled <- scale(af_opg.3)


# lave clustering
sp_scaled <- as.data.frame(scale(sp_opg.3[,-c(1:2)]))

# elbow
#fviz_nbclust(af_opg.3_scaled, kmeans, method = "wss", k.max = 10)

fviz_nbclust(sp_scaled, kmeans, method = "wss", k.max = 10)

# kmeans
kmeans_spiller <- kmeans(sp_scaled, centers = 3, nstart = 25)

#kmeans_afl <- kmeans(af_opg.3_scaled, centers = 3, nstart = 10)

#af_opg.3$cluster <- as.factor(kmeans_afl$cluster)
#af_opg.3$player_id <- opg.3_pass$player$id

#faf_opg.3 <- as.data.frame(table(af_opg.3$cluster, af_opg.3$player_id))
#faf_opg.3 <- faf_opg.3 %>%
  #group_by(Var2) %>%
  #mutate(total_freq = sum(Freq),
         #pct = (Freq/total_freq) * 100) %>%
  #ungroup()

sp_opg.3$cluster <- as.factor(kmeans_spiller$cluster)

fviz_cluster(kmeans_spiller, data = sp_scaled)

#fviz_cluster(kmeans_afl, data = af_opg.3)

# gns
aggregate(sp_opg.3, by=list(cluster=sp_opg.3$cluster), mean)

# undersøge position
angriber <- c("CF", "SS","LWF", "RWF")
midtbane <- c("DMF", "LDMF", "RDMF", "LCMF", "RCMF", "LCMF3", "RCMF3","LW", "RW", "AMF", "LAMF", "RAMF")
forsvar <- c("GK", "CB", "LCB", "RCB", "LCB3", "RCB3", "LB", "RB", "LB5", "RB5", "LWB", "RWB")

spp_opg_3 <- opg.3 %>%
  mutate(
    position = case_when(
      player$position %in% angriber ~ "Angriber",
      player$position %in% midtbane ~ "Midtbane",
      player$position %in% forsvar ~ "Forsvar",
      TRUE ~ "Ukendt"
    ),
    spiller = player$name,
    id = player$id
) %>%
  group_by(player$id,player$name) %>%
  reframe(
    position = position
  ) %>%
  na.omit()

positioner <- as.data.frame(table(spp_opg_3$position,spp_opg_3$`player$id`))

positioner <- positioner %>%
  group_by(Var2) %>%
  slice(which.max(Freq)) %>%  
  mutate(
    position_pct = Freq / sum(Freq),
    final_position = if_else(position_pct > 0.5, Var1, "Blandet")
  ) %>%
  select(Var2, final_position)

colnames(positioner)[1] <- "player_id"
colnames(sp_opg.3)[1] <- "player_id"
sp_opg.3$player_id <- as.factor(sp_opg.3$player_id)

position3_1 <- right_join(positioner,sp_opg.3, by = "player_id")

# pca
#data.pca <- princomp(af_opg.3)
#summary(data.pca)
#data.pca$loadings[, 1:2]
#fviz_pca_var(data.pca, col.var = "black")

data.pca <- princomp(sp_scaled)
summary(data.pca)
data.pca$loadings[, 1:2]
fviz_pca_var(data.pca, col.var = "black")

# polsk liga

pl_pass <- opg.3 %>%
  filter(sapply(match_info, function(x) x$competitionId == 692))

pl_shot <- opg.2_skud %>%
  filter(sapply(match_info, function(x) x$competitionId == 692))

sppl_pass <- pl_pass %>%
  group_by(player$id,player$name) %>%
  summarise(
    antal_afleveringer = n(),
    gennemsnit_længde = mean(pass$length, na.rm = TRUE),
    pass_gns = mean(pass$accurate == TRUE, na.rm = TRUE) * 100,
    sd_af = sd(pass$length, na.rm = TRUE)
  ) %>%
  na.omit()



sppl_shot <- pl_shot %>%
  group_by(player.id = player$id, 
           player.name = player$name) %>%
  summarise(
    antal_shots = n(),
    distance_to_goal = mean(sqrt((location$x - 100)^2 + (location$y - 50)^2)),
    shot_accuracy = mean(shot$isGoal == TRUE, na.rm = TRUE) * 100,
    .groups = 'drop'
  ) %>%
  arrange(desc(antal_shots)) %>%
  na.omit()

colnames(sppl_pass)[1] <- "player"
colnames(sppl_shot)[1] <- "player"


anti_pl <- left_join(sppl_pass, sppl_shot, by = "player") %>% na.omit()
anti_pl <- anti_pl[anti_pl$antal_shots >= 4, ]

sp_pl <- as.data.frame(scale(anti_pl[,-c(1,2,7)]))

fviz_nbclust(sp_pl, kmeans, method = "wss", k.max = 10)

spkm <- kmeans(sp_pl,centers = 3,nstart = 25)

anti_pl$cluster <- as.factor(spkm$cluster)

fviz_cluster(spkm,data = sp_pl)

plpl_skud <- anti_pl %>%
  group_by(cluster) %>%
  summarise(
    antal_afleveringer = mean(antal_afleveringer),
    antal_shots = mean(antal_shots),
    gns_længde = mean(gennemsnit_længde),
    pass_acc = mean(pass_gns),
    shot_længde = mean(distance_to_goal),
    shot_acc = mean(shot_accuracy),
    sd_af = mean(sd_af)
  )

colnames(anti_pl)[1] <- "player_id"
anti_pl$player_id <- as.factor(anti_pl$player_id)
position3_2 <- right_join(positioner,anti_pl, by = "player_id")

# pca
data.pca <- princomp(sp_pl)
summary(data.pca)
data.pca$loadings[, 1:4]
fviz_pca_var(data.pca, col.var = "black")

# test
#tpldf <- opg.3_pass
#tpldf$player_id <- tpldf$player$id
#tpldf <- tpldf %>% group_by(player$id)

#tfdf <- anti_join(sppl_shot,player_p,by = "player_id")

#colnames(sppl_pass)[1] <- "player_id"
#colnames(sppl_shot)[1] <- "player_id"

#pltdf <- ttdf %>%
  #group_by(player$id) %>%
  #summarise(
    #antal_afleveringer = n(),
#  ) 

## cluster med 3d aflevering
anti_pl$cluster <- as.factor(spkm$cluster)
anti_pl$position <- position3_2$final_position

sp_opg.3$player <- sp_opg.3$`player$name`
sp_opg.3$position <- position3_1$final_position

centroids_3_1 <- sp_opg.3 %>%
  group_by(cluster) %>%
  summarise(
    X = mean(antal_afleveringer), 
    Y = mean(gennemsnit_længde), 
    Z = mean(pass_gns)
  ) %>%
  ungroup()

P <- plot_ly() %>%
  add_trace(
    data = sp_opg.3, 
    x = ~antal_afleveringer, 
    y = ~gennemsnit_længde, 
    z = ~pass_gns,
    type = "scatter3d", 
    mode = "markers",
    color = ~as.factor(cluster),
    marker = list(size = 5),
    text = ~paste(player, position, sep = "|"),
    hovertemplate = paste(
      "Spiller: %{text}<br>",
      "Afleveringer: %{x:.2f}<br>",
      "Pass Accuracy: %{z:.2f}<br>",
      "Længde: %{y:.2f}<br>",
      "Cluster: %{color}<br>",
      "<extra></extra>"
    ),
    name = "Data points"
  ) %>%
  
  add_trace(
    data = centroids_3_1, 
    x = ~X, 
    y = ~Y, 
    z = ~Z,
    type = "scatter3d", 
    mode = "markers",
    marker = list(size = 10, symbol = "diamond"),
    name = "Centroids"
  ) %>%
  
  layout(
    title = "3D Cluster Plot af alle afleveringer",
    scene = list(
      xaxis = list(title = "Antal Afleveringer"),
      yaxis = list(title = "Gennemsnit Længde"),
      zaxis = list(title = "Pass Accuracy")
    )
  )
P
# 3d cluster med polsk liga

centroids_3_2 <- anti_pl %>%
  group_by(cluster) %>%
  summarise(
    X = mean(antal_afleveringer), 
    Y = mean(distance_to_goal), 
    Z = mean(gennemsnit_længde)
  ) %>%
  ungroup()

P_3_2 <- plot_ly() %>%
  add_trace(
    data = anti_pl, 
    x = ~antal_afleveringer, 
    y = ~distance_to_goal, 
    z = ~gennemsnit_længde,
    type = "scatter3d", 
    mode = "markers",
    color = ~as.factor(cluster),
    marker = list(size = 5),
    text = ~paste(player.name, position, sep = "|"),
    hovertemplate = paste(
      "Spiller: %{text}<br>",
      "Afleveringer: %{x:.2f}<br>",
      "Shot længde: %{y:.2f}<br>",
      "Pass længde: %{z:.2f}<br>",
      "Cluster: %{color}<br>",
      "<extra></extra>"
    ),
    name = "Data points"
  ) %>%
  
  add_trace(
    data = centroids_3_2, 
    x = ~X, 
    y = ~Y, 
    z = ~Z,
    type = "scatter3d", 
    mode = "markers",
    marker = list(size = 10, symbol = "diamond"),
    name = "Centroids"
  ) %>%
  
  layout(
    title = "3D Cluster Plot af Polsk liga",
    scene = list(
      xaxis = list(title = "Antal Afleveringer"),
      yaxis = list(title = "Shot længde"),
      zaxis = list(title = "Pass længde")
    )
  )
P_3_2
