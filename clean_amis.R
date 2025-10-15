##############################################
# This code cleans the combined AMIS dataset 
# (2015-2022) and prepares it for analysis.
##############################################

# Load necessary libraries
library(tidyverse)
library(dplyr)
library(lubridate)


# Helper function: Recode binary variables in data cleaning 
recode_binary <- function(x, one_vals, zero_vals = NULL) {
  case_when(
    x %in% c(7, 9) ~ NA_real_,
    !is.null(zero_vals) & x %in% zero_vals ~ 0,
    x %in% one_vals ~ 1,
    TRUE ~ NA_real_
  )
}

# Cleans combined AMIS dataset
read_and_clean_amis <- function(file_path) {
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # PrEP/ART/Test-related denominators and missing codes
  df <- df %>%
    mutate(
      prep_used = case_when(
        `_hivstat` == 2 ~ NA_real_,
        prep_used %in% c(7, 9) ~ 0,
        prep_used == 8 ~ NA_real_,
        TRUE ~ prep_used
      ),
      curramed = case_when(
        `_hivstat` != 2 ~ NA_real_,
        curramed %in% c(7, 9) ~ 0,
        curramed == 8 ~ NA_real_,
        TRUE ~ curramed
      ),
      `_lasthivtest12` = if_else(`_hivstat` == 2, NA_real_, `_lasthivtest12`),
      hivtest12 = `_lasthivtest12`
    )

    # Derived PrEP eligibility
  df <- df %>%
    mutate(
      calc_prep_elig = (`_hivstat` != 2) & (m_mp12oanum > 1) &
        (m_m1uas == 1 | bstid == 0 | (!is.na(m_m1sx) & m_m1hst == 2)),
      calc_prep_elig = if_else(is.na(calc_prep_elig), FALSE, calc_prep_elig),
      calc_prep_elig = if_else(`_hivstat` == 2, NA, calc_prep_elig)
    )

    # Main binary/continuous variables
  df <- df %>%
    mutate(
      urbrur     = recode_binary(`_nchs_rural2013`, one_vals = 1:4, zero_vals = 5:6),
      talkhiv    = if_else(talkhiv %in% c(7, 9), 0, talkhiv),
      insur_bin  = recode_binary(`_inscat`, one_vals = 2:4, zero_vals = 1),
      income_bin = recode_binary(income, one_vals = 4, zero_vals = 1:3),
      risk_behavior = if_else(rowSums(pick(m_muauhs, m_muahp), na.rm = TRUE) > 0, 1L, 0L),
      # risk_behavior = ifelse((risk_behavior == 1) & (prep_used == 1), 0L, risk_behavior),
      out_score = if_else(
        rowSums(is.na(pick(out_gia:out_gif))) > 0, 
        NA_real_, 
        rowSums(pick(out_gia:out_gif), na.rm = TRUE)
      ),
      out_gid = ifelse(out_gid == 2, NA, out_gid),
      homeless_p12m = case_when(homeless_p12m %in% c(7, 9) ~ NA_real_, TRUE ~ homeless_p12m),
      homeless_col = as.integer((homeless_p12m + homeless_fr) > 0),
      seehcp = recode_binary(seehcp, one_vals = 1, zero_vals = 0)
    ) %>%
    mutate(across(starts_with("out_gi"), ~ ifelse(. == 2, NA, .)))

    # Community and Support Scores
  df <- df %>%
    mutate(
      community_member = case_when(
        if_all(starts_with("member"), is.na) ~ NA_integer_,
        if_any(starts_with("member"), ~ .x %in% 1:3) ~ 1L,
        TRUE ~ 0L
      ),
      gay_support = case_when(
        if_all(c(soccap_msm1:soccap_msm4), is.na) ~ NA_integer_,
        if_any(c(soccap_msm1:soccap_msm4), ~ .x %in% 4:5) ~ 1L,
        TRUE ~ 0L
      ),
      gay_support_cont = case_when(
        rowSums(is.na(dplyr::pick(soccap_msm1, soccap_msm2, soccap_msm3, soccap_msm4))) == 4 ~ NA_real_,
        TRUE ~ rowSums(
          dplyr::pick(soccap_msm1, soccap_msm2, soccap_msm3, soccap_msm4) %>%
            mutate(across(everything(), ~ ifelse(.x %in% c(9), 0, .x))) %>% 
            mutate(across(everything(), ~ ifelse(.x <=3, 0, .x))) %>%
            mutate(across(everything(), ~ ifelse(.x > 3, 1, .x))),
          na.rm = TRUE
        )
      )
    )

    # Miscellaneous stigma recoding and summaries
  # df <- df %>%
  #   mutate(
  #     stigma_b1 = recode_binary(stigma_b1, one_vals = 1, zero_vals = 0),
  #     stigma_b2 = recode_binary(stigma_b2, one_vals = 1, zero_vals = 0),
  #     stigma_b3 = recode_binary(stigma_b3, one_vals = 1, zero_vals = 0),
  #     stigma_b4 = recode_binary(stigma_b4, one_vals = 1, zero_vals = 0)
  #   ) %>%
  #   mutate(
  #     stigma_ahs_alt = as.integer(rowSums(across(c(stigma_b1, stigma_b2)), na.rm = TRUE) > 0),
  #     stigma_exphs   = as.integer(rowSums(across(c(stigma_b3, stigma_b4)), na.rm = TRUE) > 0)
  #   )

    # Miscellaneous variables: tolerance, outness
  df <- df %>%
    mutate(
      out_gi        = recode_binary(out_gi, one_vals = 1, zero_vals = 0),
      stigma_a3     = recode_binary(stigma_a3, one_vals = 1, zero_vals = 0),
      tolerant      = recode_binary(tolerant, one_vals = 1:2, zero_vals = 3:6)
    )
  # Community Member 
  df <- df %>%
    mutate(
      across(starts_with("member"), ~ case_when(
        .x %in% c(7, 9) ~ NA_real_,
        .x %in% c(0, 1) ~ 0, 
        .x %in% c(2, 3) ~ 1,
        TRUE ~ NA_real_
      )),
      member = case_when(
        rowSums(is.na(pick(starts_with("member")))) == ncol(pick(starts_with("member"))) ~ NA_real_,
        TRUE ~ rowSums(pick(starts_with("member")), na.rm = TRUE)
      )
    )
  print(unique(df$`_censdiv`))
  # Recode demographic variables for IPF and stratification
  df <- df %>%
    mutate(
      agecat = factor(case_when(
        age >= 18 & age <= 24 ~ "18_24",
        age >= 25 & age <= 34 ~ "25_34",
        age >= 35 & age <= 44 ~ "35_44",
        age >= 45 & age <= 54 ~ "45_54",
        age >= 55 ~ "55_up"
      ), levels = c("18_24", "25_34", "35_44", "45_54", "55_up")),
      
      rdrace = factor(case_when(
        raceomb_rev == 5 ~ "white",
        raceomb_rev == 3 ~ "black",
        raceomb_rev == 4 ~ "hisp",
        raceomb_rev %in% c(1, 2, 6) ~ "other"
      ), levels = c("white", "black", "hisp", "other")),
      
      income_cat = factor(case_when(
        survey == 'amis_15' & hhincome == 0 ~ "lt20k",
        survey == 'amis_15' & hhincome == 1 ~ "20_40k",
        survey == 'amis_15' & hhincome == 2 ~ "40k_80k",
        survey == 'amis_15' & hhincome == 3 ~ "80k",
        survey_year != 2015 & income == 1 ~ "lt20k",
        survey_year != 2015 & income == 2 ~ "20_40k",
        survey_year != 2015 & income == 3 ~ "40k_80k",
        survey_year != 2015 & income == 4 ~ "80k"
      ), levels = c("lt20k", "20_40k", "40k_80k", "80k")),
      
      school4 = factor(case_when(
        `_educat` == 1 | hleducat_le24 %in% c(2,3,4) ~ "lths",
        `_educat` == 2 | hleducat_le24 %in% c(5) ~ "hs",
        `_educat` == 3 | hleducat_le24 %in% c(6) ~ "college",
        `_educat` == 4 | hleducat_le24 %in% c(7)~ "grad"
      ), levels = c("lths", "hs", "college", "grad")),
      
      prep_status = case_when(
        prep_used == 1 ~ "prep_used",
        prep_used == 0 ~ "prep_not_used"
      ),
      
      survey_month = month(as.Date(vdatesub, format = "%m/%d/%Y"))
    ) %>%
    rename(rowID = `...1`)
  print("_censdiv" %in% names(df))
  return(df)
}

# Removes COVID years
clean_df_nocovid <- function(df) {
  
  df_clean <- df %>%
    # Convert vdatesub to date format (if it isn't already)
    mutate(vdatesub = as.Date(vdatesub)) %>% 
    # Remove COVID-disrupted survey window
    filter(vdatesub < as.Date("2020-03-01") | vdatesub > as.Date("2021-07-01")) %>%
    filter(`_censdiv` != 10) %>%
    filter(state_calc != 'DC') %>%
    # Assign 2–3 year groupings
    mutate(year_group = case_when(
      survey_year %in% c(2015, 2016) ~ "2015-2016",
      survey_year %in% c(2017, 2018) ~ "2017-2018",
      survey_year %in% c(2019, 2020) ~ "2019-2020",
      survey_year %in% c(2021, 2022) ~ "2021-2022",
      survey_year %in% c(2023, 2024) ~ "2023-2024",
      TRUE ~ NA_character_
    )) 
  
  return(df_clean)
}

