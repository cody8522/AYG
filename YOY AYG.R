library(tidyverse)
library(dplyr)
library(ggrepel)
library(ggimage)
library(nflfastR)
options(scipen = 9999)

seasons <- 2011:2020
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


drives_AYG <- data_up %>%
  filter(!is.na(posteam))%>%
  summarize(season, week, defteam, play_id, start_FP, ydsnet,drive_play_id_started,game_id, fixed_drive, posteam, drive_game_clock_start, drive_quarter_start, drive_game_clock_end, drive_quarter_end, drive_start_yard_line, drive_end_yard_line, fixed_drive_result)
drives_AYG$game_drive <- paste(drives_AYG$game_id, drives_AYG$fixed_drive)
drives_AYG$first_play <- (drives_AYG$play_id - drives_AYG$drive_play_id_started)

drives_AYG <- drives_AYG %>%
  filter(first_play == 0) %>%
  group_by(game_drive) %>%
  summarize(season, fixed_drive, week, posteam,defteam, start_FP, ydsnet, drive_game_clock_start, drive_quarter_start, drive_game_clock_end,drive_quarter_end, drive_start_yard_line, drive_end_yard_line, fixed_drive_result, n())

drives_AYG <- drives_AYG %>%
  group_by(posteam, week, season) %>%
  arrange(posteam, week, fixed_drive)
drives_AYG$av_yds_pct <- (drives_AYG$ydsnet/drives_AYG$start_FP)

off_AYG <- drives_AYG %>%
  filter(week <= 17,!is.na(start_FP))%>%
  group_by(posteam, season) %>%
  summarize(o_mean_AYG = sum(av_yds_pct)/n(),o_drives = n())%>%
  mutate(lead_off_AYG = lead(o_mean_AYG))

off_AYG %>% lm(formula = lead_off_AYG ~ o_mean_AYG)%>% summary()

EPA <- data %>%
  filter(play_type == c('pass', 'run'), !is.na(epa))%>%
  group_by(posteam, season) %>%
  summarize(epa = mean(epa)) %>%
  mutate(lead_EPA = lead(epa))
EPA %>% lm(formula = lead_EPA ~ epa)%>% summary()
