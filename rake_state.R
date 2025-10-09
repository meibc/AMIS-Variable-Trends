##############################################
# This function uses IPF to rake survey weights 
# to known population margins for a given state.
##############################################
source("utils.R")

library(dplyr)
library(survey)

rake_by_state_trimmed <- function(state_abbrev, df, msm_counts, id_var = "rowID", trim_quantiles = c(0.01, 0.99)) {
  
  # Determine target margins
  if (state_abbrev == "ALL") {
    target_margins <- list(
      agecat     = national_perc_targets %>%
        filter(Label == "Age") %>%
        ungroup() %>% 
        select(agecat = Category, Freq = value),
      rdrace     = national_perc_targets %>% 
        filter(Label == 'Race') %>% 
        ungroup() %>% 
        select(rdrace = Category, Freq = value),
      income_cat = national_perc_targets %>% 
        filter(Label=='Income') %>% 
        ungroup() %>% 
        select(income_cat = Category, Freq = value),
      school4    = national_perc_targets %>% 
        filter(Label=='Educ') %>%  
        ungroup() %>% 
        select(school4 = Category, Freq = value)
    )
    
    # Filter data for all states
    df_state <- df %>%
      filter(!is.na(agecat), !is.na(rdrace), !is.na(income_cat), !is.na(school4)) %>%
      mutate(raked_weight = NA_real_)
    
  } else {
    target_margins <- list(
      agecat     = get_margins("Age", "agecat", state_abbrev, msm_counts),
      rdrace     = get_margins("Race", "rdrace", state_abbrev, msm_counts),
      income_cat = get_margins("Income", "income_cat", state_abbrev, msm_counts),
      school4    = get_margins("Educ", "school4", state_abbrev, msm_counts)
    )
    
    # Filter data for this state
    df_state <- df %>%
      filter(state_calc == state_abbrev,
             !is.na(agecat), !is.na(rdrace), !is.na(income_cat), !is.na(school4)) %>%
      mutate(raked_weight = NA_real_)
  }
  
  # Loop through each year_group
  for (yg in unique(df_state$year_group)) {
    df_sub <- df_state %>% filter(year_group == yg)
    if (nrow(df_sub) < 30) next  # Skip small groups
    
    # Match available categories
    pop_margins <- list(
      target_margins$agecat     %>% filter(agecat     %in% df_sub$agecat),
      target_margins$rdrace     %>% filter(rdrace     %in% df_sub$rdrace),
      target_margins$income_cat %>% filter(income_cat %in% df_sub$income_cat),
      target_margins$school4    %>% filter(school4    %in% df_sub$school4)
    )
    
    # Create design with equal weights
    dsgn <- svydesign(ids = ~1, data = df_sub, weights = ~1)
    
    # Try raking
    raked <- tryCatch({
      rake(
        design = dsgn,
        sample.margins = list(~agecat, ~rdrace, ~income_cat, ~school4),
        population.margins = pop_margins,
        control = list(partial = TRUE)
      )
    }, error = function(e) {
      message("Raking failed for ", state_abbrev, " in year group ", yg, ": ", e$message)
      return(NULL)
    })
    
    # Save weights if successful
    if (!is.null(raked)) {
      df_state$raked_weight[df_state[[id_var]] %in% df_sub[[id_var]]] <- weights(raked)
    }
  }
  
  # Trim extreme weights
  quantiles <- quantile(df_state$raked_weight, probs = trim_quantiles, na.rm = TRUE)
  
  df_trimmed <- df_state %>%
    filter(!is.na(raked_weight)) %>%
    filter(raked_weight >= quantiles[1], raked_weight <= quantiles[2])
  
  return(df_trimmed)
}
