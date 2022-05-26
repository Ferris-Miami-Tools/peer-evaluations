# Author: Joshua Ferris

# Function to return a dataframe containing a single team members answers
# by getting the values from the correct columns.
getTeamMember = function(df, team_member) {
  temp_df = df %>%
    select(name, uniqueid, starts_with(team_member))
  colnames(temp_df) = gsub(paste0(team_member, "_"), "", colnames(temp_df))
  colnames(temp_df) = make.unique(colnames(temp_df))
  return(temp_df %>% rename(tm_name = name.1))
}

# Function to load in the raw CSV and apply basic transformations to make it
# usable.
getRawData = function(file_name) {
  df = read_csv(file_name) %>%
    slice(3:n()) %>%
    select(-StartDate, -EndDate, -Status, -IPAddress, -Progress,
           -`Duration (in seconds)`, -Finished, -ResponseId, -RecipientLastName,
           -RecipientFirstName, -RecipientEmail, -ExternalReference,
           -LocationLatitude, -LocationLongitude, -DistributionChannel,
           -UserLanguage)
  colnames(df) = c(
    "RecordedDate", "name", "uniqueid", "t1_section",
    "t1_group", "t1_name", "t1_preparation", "t1_team_player",
    "t1_participation", "t1_collaboration", "t1_quality_of_work", "t1_overall",
    "t1_happy_future", "t1_feedback",
    "t2_section", "t2_group", "t2_name", "t2_preparation", "t2_team_player",
    "t2_participation", "t2_collaboration", "t2_quality_of_work", "t2_overall",
    "t2_happy_future", "t2_feedback",
    "t3_section", "t3_group", "t3_name", "t3_preparation", "t3_team_player",
    "t3_participation", "t3_collaboration", "t3_quality_of_work", "t3_overall",
    "t3_happy_future", "t3_feedback",
    "t4_section", "t4_group", "t4_name", "t4_preparation", "t4_team_player",
    "t4_participation", "t4_collaboration", "t4_quality_of_work", "t4_overall",
    "t4_happy_future", "t4_feedback",
    "t5_section", "t5_group", "t5_name", "t5_preparation", "t5_team_player",
    "t5_participation", "t5_collaboration", "t5_quality_of_work", "t5_overall",
    "t5_happy_future", "t5_feedback",
    "t6_section", "t6_group", "t6_name", "t6_preparation", "t6_team_player",
    "t6_participation", "t6_collaboration", "t6_quality_of_work", "t6_overall",
    "t6_happy_future", "t6_feedback"
  )
  return(df)
}
