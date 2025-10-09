##############################################
# These functions supports the overall package
##############################################

library(dplyr)
library(srvyr)
library(rlang)

# Helper function: Pull and normalize target margins
get_margins <- function(label, varname, state_abbrev, msm_counts) {
  msm_counts %>%
    filter(ST_ABBREV == state_abbrev, Label == label) %>%
    select(Category, Freq = group_sum_msm_share) %>%
    mutate(Freq = Freq / sum(Freq)) %>%
    rename(!!varname := Category)
}

compute_weighted_means_by_group <- function(svy_df, vars_to_summarize, group_vars = c("state_calc", "year_group")) {
  
  # Convert character variable names to symbols
  summary_exprs <- setNames(
    lapply(vars_to_summarize, function(var) {
      expr(survey_mean(!!sym(var), na.rm = TRUE))
    }),
    paste0("mean_", vars_to_summarize)
  )
  
  # Add unweighted n
  summary_exprs$n <- expr(unweighted(n()))
  
  # Group and summarize
  svy_df %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(!!!summary_exprs, .groups = "drop")
}
