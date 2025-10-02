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


df_clean <- read_and_clean_amis(df_filepath)
df_nocovid_grouped <- clean_df_nocovid(df_clean)

states <- unique(df_nocovid_grouped$state_calc)

# Combine trimmed, raked data from all states
df_all_trimmed <- purrr::map_dfr(states, ~rake_by_state_trimmed(.x, df_nocovid_grouped, msm_counts))

# Step 2: Create a valid survey design object
svy_df <- df_all_trimmed %>%
  filter(!is.na(raked_weight), is.finite(raked_weight), raked_weight > 0) %>%
  as_survey_design(ids = 1, weights = raked_weight)

# Step 3: Compute weighted mean stigma per state and year group
stigma_trends <- svy_df %>%
  group_by(state_calc, year_group) %>%
  summarise(
    mean_stigma = survey_mean(stigma_ahs, na.rm = TRUE),
    mean_stigma_gss = survey_mean(stigma_gss, na.rm = TRUE),
    mean_stigma_family = survey_mean(stigma_family, na.rm = TRUE),
    mean_ka = survey_mean(knowledge_aware, na.rm = TRUE),
    mean_risk_behavior = survey_mean(risk_behavior, na.rm = TRUE),
    mean_talkhiv = survey_mean(talkhiv, na.rm = TRUE),
    mean_illicit = survey_mean(illicit, na.rm = TRUE),
    mean_homelessness_col = survey_mean(homeless_col, na.rm = TRUE),
    mean_insur_bin = survey_mean(insur_bin, na.rm = TRUE),
    mean_urbrur = survey_mean(urbrur, na.rm = TRUE),
    mean_outgi = survey_mean(out_gi, na.rm = TRUE),
    mean_outhcp = survey_mean(out_gid, na.rm = TRUE),
    
    n = unweighted(n()),  # <- unweighted count of non-missing rows
    .groups = "drop"
  )

write_csv(stigma_trends, "stigma_trends.csv")
