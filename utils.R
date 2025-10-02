##############################################
# These functions supports the overall package
##############################################

# Helper function: Recode binary variables in data cleaning 
recode_binary <- function(x, one_vals, zero_vals = NULL) {
  case_when(
    x %in% c(7, 9) ~ NA_real_,
    !is.null(zero_vals) & x %in% zero_vals ~ 0,
    x %in% one_vals ~ 1,
    TRUE ~ NA_real_
  )
}

# Helper function: Pull and normalize target margins
get_margins <- function(label, varname, state_abbrev, msm_counts) {
  msm_counts %>%
    filter(ST_ABBREV == state_abbrev, Label == label) %>%
    select(Category, Freq = group_sum_msm_share) %>%
    mutate(Freq = Freq / sum(Freq)) %>%
    rename(!!varname := Category)
}