library(readxl)
library(tidyverse)
library(dplyr)
library(eeptools)
library(hexbin)

# Loading in the data
# Change the file path to wherever you have it on your computer
#ALpitches <- read.csv("~/Desktop/Random-Projects/MLB/MLB-App/ALpitches.csv")
# ALleaguePitch <- read.csv("~/Desktop/Random-Projects/MLB/MLB-App/ALleaguePitch.csv")
# ALleagueName <- read.csv("~/Desktop/Random-Projects/MLB/MLB-App/ALleagueName.csv")
# ALleagueNameStart <- read.csv("~/Desktop/Random-Projects/MLB/MLB-App/ALleagueNameStart.csv")
#
#NLpitches <- read.csv("~/Desktop/Random-Projects/MLB/MLB-App/NLpitches.csv")
# NLleaguePitch <- read.csv("~/Desktop/Random-Projects/MLB/MLB-App/NLleaguePitch.csv")
# NLleagueName <- read.csv("~/Desktop/Random-Projects/MLB/MLB-App/NLleagueName.csv")
# NLleagueNameStart <- read.csv("~/Desktop/Random-Projects/MLB/MLB-App/NLleagueNameStart.csv")

ALpitches <- read.csv("ALpitches.csv")
ALleaguePitch <- read.csv("ALleaguePitch.csv")
ALleagueName <- read.csv("ALleagueName.csv")
ALleagueNameStart <- read.csv("ALleagueNameStart.csv")

NLpitches <- read.csv("NLpitches.csv")
NLleaguePitch <- read.csv("NLleaguePitch.csv")
NLleagueName <- read.csv("NLleagueName.csv")
NLleagueNameStart <- read.csv("NLleagueNameStart.csv")

MLBpitches = bind_rows(ALpitches, NLpitches)
MLBleaguePitch = bind_rows(ALleaguePitch, NLleaguePitch) %>%
  arrange(Name, PitchType)
MLBleagueName = bind_rows(ALleagueName, NLleagueName)
MLBleagueNameStart = bind_rows(ALleagueNameStart, NLleagueNameStart)

MLBGrouped = MLBpitches %>%
  group_by(Name, game_date, PitchType) %>%
  summarize(
    Pitches = n(),
    Velo = round(mean(release_speed), 2),
    SpinRate = round(mean(release_spin_rate), 2),
    IVB = round(mean(IVB), 2),
    HB = round(mean(HB), 2),
    RelHeight = round(mean(RelHeight), 2),
    RelSide = round(mean(RelSide), 2),
    Extension = round(mean(release_extension), 2),
    `Stuff+` = round(mean(StuffPlus), 2),
    xwOBA = round(mean(estimated_woba_using_speedangle, na.rm = TRUE), 2),
    RunValue = round(sum(delta_run_exp), 2),
    RV100 = round(mean(delta_run_exp), 2 * 100))  %>%
  mutate(game_date = as.Date(game_date, format = "%Y-%m-%d"),
         xwOBA = ifelse(is.na(xwOBA), 0, xwOBA))

ALGrouped = ALpitches %>%
  group_by(Name, game_date, PitchType) %>%
  summarize(
    Pitches = n(),
    Velo = round(mean(release_speed), 2),
    SpinRate = round(mean(release_spin_rate), 2),
    IVB = round(mean(IVB), 2),
    HB = round(mean(HB), 2),
    RelHeight = round(mean(RelHeight), 2),
    RelSide = round(mean(RelSide), 2),
    Extension = round(mean(release_extension), 2),
    `Stuff+` = round(mean(StuffPlus), 2),
    xwOBA = round(mean(estimated_woba_using_speedangle, na.rm = TRUE), 2),
    RunValue = round(sum(delta_run_exp), 2),
    RV100 = round(mean(delta_run_exp), 2 * 100)) %>%
  mutate(game_date = as.Date(game_date, format = "%Y-%m-%d"),
         xwOBA = ifelse(is.na(xwOBA), 0, xwOBA))

NLGrouped = NLpitches %>%
  group_by(Name, game_date, PitchType) %>%
  summarize(
    Pitches = n(),
    Velo = round(mean(release_speed), 2),
    SpinRate = round(mean(release_spin_rate), 2),
    IVB = round(mean(IVB), 2),
    HB = round(mean(HB), 2),
    RelHeight = round(mean(RelHeight), 2),
    RelSide = round(mean(RelSide), 2),
    Extension = round(mean(release_extension), 2),
    `Stuff+` = round(mean(StuffPlus), 2),
    xwOBA = round(mean(estimated_woba_using_speedangle, na.rm = TRUE), 2),
    RunValue = round(sum(delta_run_exp), 2),
    RV100 = round(mean(delta_run_exp), 2 * 100))  %>%
  mutate(game_date = as.Date(game_date, format = "%Y-%m-%d"),
         xwOBA = ifelse(is.na(xwOBA), 0, xwOBA))




# MLBGrouped %>%
#   ggplot(mapping = aes(x = game_date, y = Velo, color = PitchType)) +
#   geom_line() +
#   scale_color_manual(values = rainbow(length(unique(MLBLineData()$`PitchType`)))) +
#   theme_minimal()


# Color codes each column that's going to be displayed
#AvgVelo <- quantile(UNC$RelSpeed, probs = seq(.05, .95, .01), na.rm = TRUE)
#MaxVelo <- quantile(UNC$RelSpeed, probs = seq(.05, .95, .01), na.rm = TRUE)
#SpinRate <- quantile(UNC$SpinRate, probs = seq(.05, .95, .01), na.rm = TRUE)
#Induced <- quantile(UNC$InducedVertBreak, probs = seq(.05, .95, .01), na.rm = TRUE)
#Horz <- quantile(UNC$HorzBreak, probs = seq(.05, .95, .01), na.rm = TRUE)
#VAA <- quantile(UNC$VertApprAngle, probs = seq(.05, .95, .01), na.rm = TRUE)
#Height <- quantile(UNC$RelHeight, probs = seq(.05, .95, .01), na.rm = TRUE)
#Extension <- quantile(UNC$Extension, probs = seq(.05, .95, .01), na.rm = TRUE)

#clrs <- colorRampPalette(c("#6666FF", "#CCCCFF", NA, "#FFCCCC", "#FF6666", "#FF0000"))(length(Extension) + 1)


