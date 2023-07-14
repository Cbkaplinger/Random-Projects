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

# Combines the Statcast and FanGraphs tables together
batter = fg_batter |> left_join(sc_batter, by = c("Name" = "Name"))

# Gets rid of all the periods in a name
for (i in colnames(batter)){
  batter$Name = str_replace_all(batter$Name, fixed("."), "")
}
