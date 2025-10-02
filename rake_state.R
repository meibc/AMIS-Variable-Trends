##############################################
# This function uses IPF to rake survey weights 
# to known population margins for a given state.
##############################################
source("utils.R")

library(dplyr)

rake_by_state_trimmed <- function(state_abbrev, df, msm_counts, id_var = "rowID", 
                                  trim_quantiles = c(0.01, 0.99)) {
  target_margins <- list(
    agecat     = get_margins("Age", "agecat", state_abbrev, msm_counts),
    rdrace     = get_margins("Race", "rdrace", state_abbrev, msm_counts),
    income_cat = get_margins("Income", "income_cat", state_abbrev, msm_counts),
    school4    = get_margins("Educ", "school4", state_abbrev, msm_counts)
  )
  
  # Filter data for state
  df_state <- df %>%
    filter(state_calc == state_abbrev,
           !is.na(agecat), !is.na(rdrace), !is.na(income_cat), !is.na(school4)) %>%
    mutate(raked_weight = NA_real_)
  
  # Loop through year groups
  for (yg in unique(df_state$year_group)) {
    df_sub <- df_state %>% filter(year_group == yg)
    if (nrow(df_sub) < 30) next
    
    # Filter margins to available categories in the subset
    pop_margins <- list(
      target_margins$agecat     %>% filter(agecat     %in% df_sub$agecat),
      target_margins$rdrace     %>% filter(rdrace     %in% df_sub$rdrace),
      target_margins$income_cat %>% filter(income_cat %in% df_sub$income_cat),
      target_margins$school4    %>% filter(school4    %in% df_sub$school4)
    )
    
    dsgn <- svydesign(ids = ~1, data = df_sub, weights = ~1)
    
    raked <- tryCatch({
      rake(dsgn,
           sample.margins = list(~agecat, ~rdrace, ~income_cat, ~school4),
           population.margins = pop_margins,
           control = list(partial = TRUE))
    }, error = function(e) NULL)
    
    ids <- df_sub[[id_var]]
    
    if (!is.null(raked)) {
      df_state$raked_weight[df_state[[id_var]] %in% ids] <- weights(raked)
    } else {
      message("Raking failed for ", state_abbrev, " in year group ", yg)
    }
  }
  
  # Trim extreme weights
  quantiles <- quantile(df_state$raked_weight, probs = trim_quantiles, na.rm = TRUE)
  
  df_trimmed <- df_state %>%
    filter(raked_weight >= quantiles[1], raked_weight <= quantiles[2])
  
  return(df_trimmed)
}
