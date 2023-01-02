# Author: Joshua Ferris

# Load packages and helper functions
pacman::p_load(tidyverse)
source("./funcs.R")

# Load in the data
raw_data = getRawData("./peer_evals_raw_F22.csv")

# Start cleaning process
# * Remove records where the student did not provide their name and unique ID
#   or has a significant number of missing values.
# * Only keep a students most recent submission
bad_data = raw_data %>%
  add_column(missing=rowSums(is.na(raw_data))) %>%
  filter(is.na(name) & is.na(uniqueid) & missing<60)
clean_data = raw_data %>%
  filter(!is.na(name) & !is.na(uniqueid)) %>%
  group_by(name) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  select(-RecordedDate)

# Create summary data by grouping by team member name and group. Averages are
# then taken ignorning null values. Feedback is concatenated.
summary_df = rbind(
  getTeamMember(clean_data, "t1"),
  getTeamMember(clean_data, "t2"),
  getTeamMember(clean_data, "t3"),
  getTeamMember(clean_data, "t4"),
  getTeamMember(clean_data, "t5"),
  getTeamMember(clean_data, "t6")
) %>%
  mutate(
    preparation = as.numeric(preparation),
    team_player = as.numeric(team_player),
    participation = as.numeric(participation),
    collaboration = as.numeric(collaboration),
    quality_of_work = as.numeric(quality_of_work),
    overall = as.numeric(overall),
    happy_future = as.numeric(happy_future),
  ) %>%
  group_by(tm_name, group) %>%
  summarise(
    avg_prep = round(mean(preparation, na.rm=T), 2),
    avg_team_player = round(mean(team_player, na.rm=T), 2),
    avg_participation = round(mean(participation, na.rm=T), 2),
    avg_collaboration = round(mean(collaboration, na.rm=T), 2),
    avg_quality_of_work = round(mean(quality_of_work, na.rm=T), 2),
    avg_overall = round(mean(overall, na.rm=T), 2),
    avg_happy_future = round(mean(happy_future, na.rm=T), 2),
    feedback = Reduce(f=function(x, y) paste(x, y, sep="\n\n"), x=paste(name, feedback, sep="\n"))
  ) %>%
  mutate(avg_all = round(mean(c(avg_prep, avg_team_player, avg_participation, avg_collaboration, avg_quality_of_work, avg_overall, avg_happy_future), na.rm=T), 2)) %>%
  filter(!is.na(tm_name)) %>%
  select(tm_name, group, avg_prep, avg_team_player, avg_participation, avg_collaboration, avg_quality_of_work, avg_overall, avg_happy_future, avg_all, feedback)

write_csv(raw_data, "./output/raw_data.csv")
write_csv(bad_data, "./output/bad_data.csv")
write_csv(summary_df, "./output/summary_data.csv")
