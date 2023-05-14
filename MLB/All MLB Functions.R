library(baseballr)
library(mlbplotR)
library(dplyr)
library(teamcolors)

# Filters the teamcolors package to only include MLB teams
MLB_colors = teamcolors[teamcolors$league == "mlb",]


# Creates and adds the abbreviations of the MLB teams to the MLB_colors df
Team = c("ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", "HOU", "KCR", "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "OAK", "PHI", "PIT", "SDP", "SFG", "SEA", "STL", "TBR", "TEX", "TOR", "WAS")
MLB_colors = cbind(MLB_colors, Team)

# Baseball Reference Batter and Pitcher data
#bref_batter <- bref_daily_batter("2023-03-30", "2023-05-06")
#bref_pitcher <- bref_daily_pitcher("2023-03-30", "2023-05-06")

# How to lookup for batter fangraphs stats
#Trout <- playerid_lookup("Trout", "Mike")
#Trout_id <- Trout$fangraphs_id
#fg_data_trout <-
  #fg_batter_game_logs(playerid = Trout_id, year = 2022)


# How to lookup for pitcher fangraphs stats
#Nola <- playerid_lookup("Nola", "Aaron")
#Nola_id <- Nola$fangraphs_id
#fg_data_nola <-
  #fg_pitcher_game_logs(playerid = Nola_id, year = 2022)


# Fangraphs batter data
fg_batter <- fg_batter_leaders(2023, 2023) |> left_join(MLB_colors, by = c("Team" = "Team"))


# Fangraphs pitcher data
fg_pitcher <- fg_pitcher_leaders(2023, 2023) |> left_join(MLB_colors, by = c("Team" = "Team"))


#Statcast batter data
sc_batter <- statcast_leaderboards(leaderboard = "expected_statistics", player_type = "batter", year = 2023, min_pa = 50)


# Statcast pitcher data
sc_pitcher <- statcast_leaderboards(leaderboard = "expected_statistics", player_type = "pitcher", year = 2023, min_pa = 25)




