##############################################
# These functions supports the overall package
##############################################

# Helper function: Pull and normalize target margins
get_margins <- function(label, varname, state_abbrev, msm_counts) {
  msm_counts %>%
    filter(ST_ABBREV == state_abbrev, Label == label) %>%
    select(Category, Freq = group_sum_msm_share) %>%
    mutate(Freq = Freq / sum(Freq)) %>%
    rename(!!varname := Category)
}