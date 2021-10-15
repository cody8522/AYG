library(tidyverse)
library(dplyr)
library(ggrepel)
library(ggimage)
library(nflfastR)
options(scipen = 9999)

seasons <- 2016:2021
data <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  )
})

data_up <- data %>%
  separate(drive_start_yard_line, c("teamyard", "yrdlines"), sep = " ", remove = FALSE, convert = TRUE)%>%
  mutate("start_FP" = ifelse(teamyard == 50, 50, ifelse(posteam == teamyard, 100 - yrdlines, yrdlines)))%>%
  mutate("season" = substr(game_id,1,4))


team_drives <- data_up %>%
  filter(!is.na(posteam))%>%
  summarize(season, week, defteam, play_id, start_FP, ydsnet,drive_play_id_started,game_id, fixed_drive, posteam, drive_game_clock_start, drive_quarter_start, drive_game_clock_end, drive_quarter_end, drive_start_yard_line, drive_end_yard_line, fixed_drive_result)%>%
  mutate("PNT" = ifelse(fixed_drive_result == "Punt",1,0))%>%
  mutate("FG" = ifelse(fixed_drive_result == "Field goal",1,0))%>%
  mutate("TD" = ifelse(fixed_drive_result == "Touchdown",1,0))%>%
  mutate("INT" = ifelse(fixed_drive_result == "Interception",1,0))%>%
  mutate("FUM" = ifelse(fixed_drive_result == "Fumble",1,0))%>%
  mutate("SAF" = ifelse(fixed_drive_result == "Safety",1,0))%>%
  mutate("TOD" = ifelse(fixed_drive_result == "Turnover on downs",1,0))%>%
  mutate("MFG" = ifelse(fixed_drive_result == "Missed field goal",1,0))%>%
  mutate("OTD" = ifelse(fixed_drive_result == "Opp touchdown",1,0))%>%
  mutate("EOH" = ifelse(fixed_drive_result == "End of half",1,0))%>%
  mutate("EOG" = ifelse(fixed_drive_result == "End of game",1,0))


team_drives$game_drive <- paste(team_drives$game_id, team_drives$fixed_drive)
team_drives$first_play <- (team_drives$play_id - team_drives$drive_play_id_started)


team_drives <- team_drives %>%
  filter(first_play == 0) %>%
  group_by(game_drive) %>%
  summarize(season, fixed_drive, week, posteam,defteam, start_FP, ydsnet, drive_game_clock_start, drive_quarter_start, drive_game_clock_end,drive_quarter_end, drive_start_yard_line, drive_end_yard_line, fixed_drive_result, n(),
            TD, FG, PNT, INT, FUM, TOD, OTD, MFG, EOH,EOG, SAF)

team_drives <- team_drives %>%
  group_by(posteam, week, season) %>%
  arrange(posteam, week, fixed_drive)

team_drives$av_yds_pct <- (team_drives$ydsnet/team_drives$start_FP)
team_drives$PAD <- (cumsum(team_drives$TD)*7) + (cumsum(team_drives$FG) * 3) 
team_drives$PBD <- lag(team_drives$PAD)
team_drives$aPAD <- (cumsum(team_drives$TD)*7) + (cumsum(team_drives$FG) * 3) + (cumsum(team_drives$MFG) * 3)
team_drives$aPBD <- lag(team_drives$aPAD)


off_eff <- team_drives %>%
  filter(week <= 17,!is.na(start_FP))%>%
  group_by(posteam, week, season) %>%
  summarize(o_mean_AYG = mean(av_yds_pct), o_sd_AYG = sd(av_yds_pct),o_drives = n(),TD = sum(TD), FG = sum(FG), PNT = sum(PNT), INT = sum(INT), FUM = sum(FUM), SAF = sum(SAF), TOD = sum(TOD), MFG = sum(MFG), OTD = sum(OTD), EOH =sum(EOH), EOG = sum(EOG))%>%
  arrange(week)

def_eff <- team_drives %>%
  filter(week <= 17, !is.na(start_FP))%>%
  group_by(defteam, week, season) %>%
  summarize(d_mean_AYG = mean(av_yds_pct), d_sd_AYG = sd(av_yds_pct),d_drives = n(),TD = sum(TD), FG = sum(FG), PNT = sum(PNT), INT = sum(INT), FUM = sum(FUM), SAF = sum(SAF), TOD = sum(TOD), MFG = sum(MFG), OTD = sum(OTD), EOH =sum(EOH), EOG = sum(EOG))%>%
  arrange(defteam)

total_eff <- off_eff %>%
  left_join(def_eff, by = c('posteam' = 'defteam', 'week' = 'week', 'season' = 'season'))%>%
  group_by(posteam, season)%>%
  summarize(o_mean_AYG = mean(o_mean_AYG), d_mean_AYG = mean(d_mean_AYG), o_sd_AYG =mean(o_sd_AYG), d_sd_AYG = mean(d_sd_AYG), o_drives = sum(o_drives), d_drives = sum(d_drives))%>%
  arrange(-d_mean_AYG)

  