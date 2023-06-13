library(baseballr)
library(dplyr)
library(stringr)


# Fangraphs pitcher data
fg_pitcher2021 <- fg_pitcher_leaders(2021, 2021, "all", "60")
fg_pitcher2022 <- fg_pitcher_leaders(2022, 2022, "all", "60")

# Loads in Statcast pitcher data
sc_xpitcher2021 <- statcast_leaderboards(leaderboard = "expected_statistics", player_type = "pitcher", year = 2021)
sc_pitcher_velo2021 <- statcast_leaderboards(leaderboard = "exit_velocity_barrels", player_type = "pitcher", year = 2021)

# Removes the useless/repeating columns
sc_pitcher_velo2021 = subset(sc_pitcher_velo2021, select = -c(attempts, first_name, last_name, year))

# Joins the two Statcast leaderboards together
sc_pitcher2021 = sc_xpitcher2021 |> left_join(sc_pitcher_velo2021, by = c("player_id" = "player_id"))

# Creates a new column combining the batters' first and last names
sc_pitcher2021$Name = paste(sc_pitcher2021$first_name, sc_pitcher2021$last_name)

# Moves pitcher full name column to before player_id column
sc_pitcher2021 <- sc_pitcher2021 %>% relocate(Name, .before = player_id)
sc_pitcher2021 = subset(sc_pitcher2021, select = -c(player_id, first_name, last_name))

# Replaces the special characters in Spanish into English characters
for (i in colnames(sc_pitcher2021)){
  sc_pitcher2021$Name = str_replace_all(sc_pitcher2021$Name, fixed("é"), "e")
  sc_pitcher2021$Name = str_replace_all(sc_pitcher2021$Name, fixed("í"), "i")
  sc_pitcher2021$Name = str_replace_all(sc_pitcher2021$Name, fixed("ó"), "o")
  sc_pitcher2021$Name = str_replace_all(sc_pitcher2021$Name, fixed("ú"), "u")
  sc_pitcher2021$Name = str_replace_all(sc_pitcher2021$Name, fixed("ñ"), "n")
  sc_pitcher2021$Name = str_replace_all(sc_pitcher2021$Name, fixed("ü"), "u")
}

# Renames the brl_percent column to Barrel Rate
colnames(sc_pitcher2021)[colnames(sc_pitcher2021) == "brl_percent"] ="Barrel Rate"

# Gets rid of all the periods in a name
for (i in colnames(sc_pitcher2021)){
  sc_pitcher2021$Name = str_replace_all(sc_pitcher2021$Name, fixed("."), "")
}
for (i in colnames(fg_pitcher2021)){
  fg_pitcher2021$Name = str_replace_all(fg_pitcher2021$Name, fixed("."), "")
}
for (i in colnames(fg_pitcher2022)){
  fg_pitcher2022$Name = str_replace_all(fg_pitcher2022$Name, fixed("."), "")
}

# Combines the Statcast and FanGraphs tables together
pitcher2021 = fg_pitcher2021 |> left_join(sc_pitcher2021, by = c("Name" = "Name"))


# Creates the Age_score column
pitcher2021$Age_score <- vector("numeric", nrow(pitcher2021))

# Gives every pitcher their age score based on their age
for (i in 1:nrow(pitcher2021)) {
  if (pitcher2021$Age[i] == 28) {
    pitcher2021$Age_score[i] = 11
  } else if (pitcher2021$Age[i] == 27) {
    pitcher2021$Age_score[i] = 10
  } else if (pitcher2021$Age[i] == 26) {
    pitcher2021$Age_score[i] = 9
  } else if (pitcher2021$Age[i] == 23 | pitcher2021$Age[i] == 24) {
    pitcher2021$Age_score[i] = 8
  } else if (pitcher2021$Age[i] == 25) {
    pitcher2021$Age_score[i] = 7
  } else if (pitcher2021$Age[i] == 29) {
    pitcher2021$Age_score[i] = 6
  } else if (pitcher2021$Age[i] == 30) {
    pitcher2021$Age_score[i] = 5
  } else if (pitcher2021$Age[i] == 33 | pitcher2021$Age[i] == 34) {
    pitcher2021$Age_score[i] = 4
  } else if (pitcher2021$Age[i] >= 35) {
    pitcher2021$Age_score[i] = 3
  } else if (pitcher2021$Age[i] == 31) {
    pitcher2021$Age_score[i] = 2
  } else if (pitcher2021$Age[i] == 22 | pitcher2021$Age[i] == 32) {
    pitcher2021$Age_score[i] = 1
  } else {
    pitcher2021$Age_score[i] = 0
  }
}

# Calculating FRA (Forecasted Run Average)
pitcher2021$FRA = ((-4.92283 - (0.07741 * pitcher2021$`K-BB_pct`)) + ((0.11961 * pitcher2021$avg_hit_speed - 0.0833) - (0.03819 * pitcher2021$Age_score)))
pitcher2021$FRA = round(pitcher2021$FRA, 2)

# Calculating pCRA (Predictive Classified Run Average)
pitcher2021$bk_pct = ((pitcher2021$SO + 10.66) / (pitcher2021$TBF + 10.66 + 37.53))
pitcher2021$bbb_pct = ((pitcher2021$BB + 9.58) / (pitcher2021$TBF + 9.58 + 116.2))
pitcher2021$bBarrels_BBE = ((pitcher2021$barrels + 13.33) / (pitcher2021$bip + 13.33 + 191.5))

pitcher2021$pCRA = (-8.89 * pitcher2021$bk_pct) + (8.03 * pitcher2021$bbb_pct) + (5.54 * pitcher2021$bBarrels_BBE) + (106 * (pitcher2021$bBarrels_BBE)^2) + 4.55
pitcher2021$pCRA = round(pitcher2021$pCRA, 2)

# Renaming the columns that are going to be used by year
colnames(pitcher2021)[colnames(pitcher2021) == "ERA"] ="ERA_2021"
colnames(pitcher2021)[colnames(pitcher2021) == "FIP"] ="FIP_2021"
colnames(pitcher2021)[colnames(pitcher2021) == "xFIP"] ="xFIP_2021"
colnames(pitcher2021)[colnames(pitcher2021) == "SIERA"] ="SIERA_2021"
colnames(pitcher2021)[colnames(pitcher2021) == "xera"] ="xERA_2021"
colnames(pitcher2021)[colnames(pitcher2021) == "pCRA"] ="pCRA_2021"
colnames(pitcher2021)[colnames(pitcher2021) == "FRA"] ="FRA_2021"
colnames(pitcher2021)[colnames(pitcher2021) == "K_pct"] ="K_pct_2021"
colnames(pitcher2021)[colnames(pitcher2021) == "K-BB_pct"] ="K-BB_pct_2021"
colnames(pitcher2021)[colnames(pitcher2021) == "woba"] ="wOBA_2021"
colnames(pitcher2021)[colnames(pitcher2021) == "est_woba"] ="xwOBA_2021"
colnames(fg_pitcher2022)[colnames(fg_pitcher2022) == "ERA"] ="ERA_2022"

# Merging the datasets together
merged = pitcher2021 |> left_join(fg_pitcher2022, by = c("Name" = "Name"))

# Removes Luis Garcia because he returns a -16 strike%
# Dallas Keuchel's 2022 ERA is terrible and is an outlier so he is removed too
merged = subset(merged, Name != "Luis Garcia" & Name != "Dallas Keuchel")

# Creating a subsetted version of merged DataFrame that is going to be used to display graphs
useful= subset(merged, select = c(Name, ERA_2021, FIP_2021, xFIP_2021, SIERA_2021, xERA_2021, pCRA_2021, FRA_2021, K_pct_2021, `K-BB_pct_2021`, wOBA_2021, xwOBA_2021, ERA_2022))

# Dropping all the rows that have a NA in it. Some players have values on FanGraphs but not on Savant
final = na.omit(useful)

