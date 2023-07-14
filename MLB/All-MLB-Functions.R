library(baseballr)
library(tidyverse)
library(dplyr)
library(teamcolors)
library(stringr)

# Filters the teamcolors package to only include MLB teams
MLB_colors = teamcolors[teamcolors$league == "mlb",]

# Removes the usless columns
MLB_colors = subset(MLB_colors, select = -c(league, division, location, mascot, sportslogos_name, logo))

# Creates and adds the abbreviations of the MLB teams to the MLB_colors df
Team = c("ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", "HOU", "KCR", "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "OAK", "PHI", "PIT", "SDP", "SFG", "SEA", "STL", "TBR", "TEX", "TOR", "WAS")
MLB_colors = cbind(MLB_colors, Team)


# Fangraphs batter data
fg_batter <- fg_batter_leaders(2023, 2023, "all", "50") |> left_join(MLB_colors, by = c("Team" = "Team"))
fg_batter$Name[fg_batter$Name == "Luis Robert"] <- "Luis Robert Jr."


# Fangraphs pitcher data
fg_pitcher <- fg_pitcher_leaders(2023, 2023, "all", "75") |> left_join(MLB_colors, by = c("Team" = "Team"))


#Loads in Statcast batter data
sc_xbatter <- statcast_leaderboards(leaderboard = "expected_statistics", player_type = "batter", year = 2023)
sc_batter_velo <- statcast_leaderboards(leaderboard = "exit_velocity_barrels", player_type = "batter", year = 2023)

# Loads in Statcast sprint speed data for batters
sprint_speed <- statcast_leaderboards(leaderboard = "sprint_speed", year = 2023)

# Removes the useless/repeating columns
sc_batter_velo = subset(sc_batter_velo, select = -c(attempts, first_name, last_name, year))
sprint_speed = subset(sprint_speed, select = -c(team, first_name, last_name, year, team_id))

# Joins the two Statcast leaderboards together
sc_batter = sc_xbatter |> left_join(sc_batter_velo, by = c("player_id" = "player_id"))
sc_batter = sc_batter |> left_join(sprint_speed, by = c("player_id" = "player_id"))

# Creates a new column combining the batters' first and last names
sc_batter$Name = paste(sc_batter$first_name, sc_batter$last_name)

# Moves batter full name column to before player_id column
sc_batter <- sc_batter %>% relocate(Name, .before = player_id)
sc_batter = subset(sc_batter, select = -c(player_id, first_name, last_name))

# Replaces the special characters in Spanish in English characters
for (i in colnames(sc_batter)){
  sc_batter$Name = str_replace_all(sc_batter$Name, fixed("á"), "a")
  sc_batter$Name = str_replace_all(sc_batter$Name, fixed("é"), "e")
  sc_batter$Name = str_replace_all(sc_batter$Name, fixed("í"), "i")
  sc_batter$Name = str_replace_all(sc_batter$Name, fixed("ó"), "o")
  sc_batter$Name = str_replace_all(sc_batter$Name, fixed("ú"), "u")
  sc_batter$Name = str_replace_all(sc_batter$Name, fixed("ñ"), "n")
  sc_batter$Name = str_replace_all(sc_batter$Name, fixed("ü"), "u")
}

# Renames the brl_percent column to Barrel Rate
colnames(sc_batter)[colnames(sc_batter) == "brl_percent"] ="Barrel_Rate"


# Loads in Statcast pitcher data
sc_xpitcher <- statcast_leaderboards(leaderboard = "expected_statistics", player_type = "pitcher", year = 2023, min_pa = 100)
sc_pitcher_velo <- statcast_leaderboards(leaderboard = "exit_velocity_barrels", player_type = "pitcher", year = 2023, min_pa = 100)

# Removes the useless/repeating columns
sc_pitcher_velo = subset(sc_pitcher_velo, select = -c(attempts, first_name, last_name, year))

# Joins the two Statcast leaderboards together
sc_pitcher = sc_xpitcher |> left_join(sc_pitcher_velo, by = c("player_id" = "player_id"))

# Creates a new column combining the batters' first and last names
sc_pitcher$Name = paste(sc_pitcher$first_name, sc_pitcher$last_name)

# Moves batter full name column to before player_id column
sc_pitcher <- sc_pitcher %>% relocate(Name, .before = player_id)
sc_pitcher = subset(sc_pitcher, select = -c(player_id, first_name, last_name))

# Replaces the special characters in Spanish into English characters
for (i in colnames(sc_batter)){
  sc_pitcher$Name = str_replace_all(sc_pitcher$Name, fixed("é"), "e")
  sc_pitcher$Name = str_replace_all(sc_pitcher$Name, fixed("í"), "i")
  sc_pitcher$Name = str_replace_all(sc_pitcher$Name, fixed("ó"), "o")
  sc_pitcher$Name = str_replace_all(sc_pitcher$Name, fixed("ú"), "u")
  sc_pitcher$Name = str_replace_all(sc_pitcher$Name, fixed("ñ"), "n")
  sc_pitcher$Name = str_replace_all(sc_pitcher$Name, fixed("ü"), "u")
}

# Renames the brl_percent column to Barrel Rate
colnames(sc_pitcher)[colnames(sc_pitcher) == "brl_percent"] ="Barrel_Rate"


# Combines the Statcast and FanGraphs tables together
batter = fg_batter |> left_join(sc_batter, by = c("Name" = "Name"))
pitcher = fg_pitcher |> left_join(sc_pitcher, by = c("Name" = "Name"))

# Gets rid of all the periods in a name
for (i in colnames(pitcher)){
  pitcher$Name = str_replace_all(pitcher$Name, fixed("."), "")
}
for (i in colnames(batter)){
  batter$Name = str_replace_all(batter$Name, fixed("."), "")
}


# Creates the Age_score column
pitcher$Age_score <- vector("numeric", nrow(pitcher))

# Gives every pitcher their age score based on their age
for (i in 1:nrow(pitcher)) {
  if (pitcher$Age[i] == 28) {
    pitcher$Age_score[i] = 11
  } else if (pitcher$Age[i] == 27) {
    pitcher$Age_score[i] = 10
  } else if (pitcher$Age[i] == 26) {
    pitcher$Age_score[i] = 9
  } else if (pitcher$Age[i] == 23 | pitcher$Age[i] == 24) {
    pitcher$Age_score[i] = 8
  } else if (pitcher$Age[i] == 25) {
    pitcher$Age_score[i] = 7
  } else if (pitcher$Age[i] == 29) {
    pitcher$Age_score[i] = 6
  } else if (pitcher$Age[i] == 30) {
    pitcher$Age_score[i] = 5
  } else if (pitcher$Age[i] == 33 | pitcher$Age[i] == 34) {
    pitcher$Age_score[i] = 4
  } else if (pitcher$Age[i] >= 35) {
    pitcher$Age_score[i] = 3
  } else if (pitcher$Age[i] == 31) {
    pitcher$Age_score[i] = 2
  } else if (pitcher$Age[i] == 22 | pitcher$Age[i] == 32) {
    pitcher$Age_score[i] = 1
  } else {
    pitcher$Age_score[i] = 0
  }
}

# Calculating FRA (Forecasted Run Average)
pitcher$FRA = ((-4.92283 - (0.07741 * pitcher$`K-BB_pct`)) + ((0.11961 * pitcher$avg_hit_speed - 0.0833) - (0.03819 * pitcher$Age_score)))
pitcher$FRA = round(pitcher$FRA, 2)

# Calculating pCRA (Predictive Classified Run Average)
pitcher$bk_pct = ((pitcher$SO + 10.66) / (pitcher$TBF + 10.66 + 37.53))
pitcher$bbb_pct = ((pitcher$BB + 9.58) / (pitcher$TBF + 9.58 + 116.2))
pitcher$bBarrels_BBE = ((pitcher$barrels + 13.33) / (pitcher$bip + 13.33 + 191.5))

pitcher$pCRA = (-8.89 * pitcher$bk_pct) + (8.03 * pitcher$bbb_pct) + (5.54 * pitcher$bBarrels_BBE) + (106 * (pitcher$bBarrels_BBE)^2) + 4.55
pitcher$pCRA = round(pitcher$pCRA, 2)

# Calculating ball and strike percentages
pitcher$ball_pct = pitcher$Balls / pitcher$Pitches
pitcher$bip_pct = pitcher$bip / pitcher$Pitches
pitcher$strike_pct = ((pitcher$Strikes / pitcher$Pitches) - pitcher$bip_pct)

# Rounds the columns to two decimal places and turns them into percentages
pitcher$ball_pct = round(pitcher$ball_pct, 2) * 100
pitcher$bip_pct = round(pitcher$bip_pct, 2) * 100
pitcher$strike_pct = round(pitcher$strike_pct, 2) * 100

# Creating a table for the most important pitcher stats
best_stats = subset(pitcher, select = c(Name, ERA, FIP, xFIP, SIERA, xera, pCRA, FRA, BABIP, LOB_pct, strike_pct, ball_pct, bip_pct, GB_pct, SwStr_pct, `K-BB_pct`, woba, est_woba))

# Creating a table to see who's performance is the most sustainable
pitcher$era_minus_SIERA = pitcher$era - pitcher$SIERA
pitcher$era_minus_FRA = pitcher$era - pitcher$FRA
pitcher$era_minus_pCRA = pitcher$era - pitcher$pCRA

true_performance = subset(pitcher, select = c(Name, ERA, era_minus_SIERA, era_minus_FRA, era_minus_pCRA, era_minus_xera_diff, est_woba_minus_woba_diff))
