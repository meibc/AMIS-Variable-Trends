# Main Code 
###################################################
# Overall code to run the analysis for AMIS trends
###################################################

source("clean_amis.R")
source("utils.R")
source("rake_state.R")

# Load libraries
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(srvyr)
library(survey)

df_filepath <- '/Users/meibinchen/Documents/GitHub/EEE/EEE Feb 2025/combined_amis.csv'
msm_counts <- read_csv('/Users/meibinchen/Library/CloudStorage/OneDrive-JohnsHopkins/EEE HIV Stigma/EEE Data/Other/GSS and NCHS Data for Marginal Dist/Results/state_level_estimates_final_adj.csv')

# TEMP 
national_perc_targets <- msm_counts %>% 
  select(-group_sum_msm_share) %>% 
  group_by(Category, Label) %>% 
  summarise(value = sum(group_sum_msm, na.rm=TRUE)) %>% 
  group_by(Label) %>%
  mutate(value = value / sum(value, na.rm=TRUE)) %>% 
  arrange(Label, Category)

df_clean <- read_and_clean_amis(df_filepath)
df_nocovid_grouped <- clean_df_nocovid(df_clean)

states <- unique(df_nocovid_grouped$state_calc)

# Combine trimmed, raked data from all states
df_all_trimmed <- purrr::map_dfr(states, ~rake_by_state_trimmed(df_nocovid_grouped, .x, msm_counts))

# Step 2: Create a valid survey design object
svy_df <- df_all_trimmed %>%
  filter(!is.na(raked_weight), is.finite(raked_weight), raked_weight > 0) %>%
  as_survey_design(ids = 1, weights = raked_weight)

# Step 3: Compute weighted mean stigma per state and year group
vars <- c("stigma_ahs", "stigma_gss", "stigma_family", "knowledge_aware",
          "risk_behavior", "talkhiv", "illicit", "homeless_col",
          "insur_bin", "urbrur", "out_gi", "out_gid")

stigma_trends <- compute_weighted_means_by_group(svy_df, vars)

# write_csv(stigma_trends, "stigma_trends.csv")

# repeat for national level 
df_national_trimmed <- rake_by_state_trimmed('ALL', df_nocovid_grouped, national_perc_targets, "rowID", c(0.05, 0.95))

svy_df_national <- df_national_trimmed %>%
  filter(!is.na(raked_weight), is.finite(raked_weight), raked_weight > 0) %>%
  as_survey_design(ids = 1, weights = raked_weight)

stigma_trends_national <- compute_weighted_means_by_group(svy_df_national, vars, group_vars = "year_group")

# write_csv(stigma_trends_national, "stigma_trends_national.csv")
