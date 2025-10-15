
# get constant demographic values for each state
state_dem_vals <- stigma_trends %>%
  select(state_calc, year_group, !ends_with("_se")) %>%
  select(!starts_with('mean_stigma'), -n, -c(mean_knowledge_aware, mean_risk_behavior, mean_talkhiv, mean_out_gi, mean_out_gid, mean_out_gic)) %>%
  mutate(year_group = factor(year_group, levels = year_levels)) %>%
  arrange(year_group, state_calc) %>%
  filter(year_group != "2023-2024") %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = year_group, values_from = value) %>% 
  mutate(variable = str_remove(variable, "^mean_")) %>%
  mutate(mean_variable = rowMeans(across(starts_with("20")), na.rm = TRUE)) %>% 
  mutate(mean_variable = round(mean_variable, 4)) 

# write.csv(stigma_dem_vals, "msm_state_mean_dem_vals.csv", row.names = FALSE)

state_var_trends <- stigma_trends %>%
  select(state_calc, !ends_with("_se"), -n) %>%
  mutate(year = as.numeric(substr(year_group, 1, 4)) + 1) %>% 
  select(!year_group) %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "variable", values_to = "value") %>%
  mutate(variable = str_remove(variable, "^mean_"))

# write.csv(state_var_trends, "variable_trends_state.csv", row.names = FALSE)

national_dem_vals <- stigma_trends_national %>% 
  mutate(year_group = factor(year_group, levels = year_levels)) %>%
  arrange(year_group) %>%
  filter(year_group != "2023-2024") %>%
  select(!ends_with("_se")) %>%
  select(!starts_with('mean_stigma'), -n, -c(mean_knowledge_aware, mean_risk_behavior, mean_talkhiv, mean_out_gi, mean_out_gid, mean_out_gic)) %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "variable", values_to = "value") %>%
  mutate(variable = str_remove(variable, "^mean_")) %>%
  pivot_wider(names_from = year_group, values_from = value) %>% 
  mutate(national_mean = rowMeans(across(starts_with("20")), na.rm = TRUE)) %>% 
  mutate(national_mean = round(national_mean, 4)) %>%
  select(variable, national_mean) 

# write.csv(national_dem_vals, "msm_national_mean_dem_vals.csv", row.names = FALSE)

national_var_trends <- stigma_trends_national %>% 
  select(!ends_with("_se"), -n) %>%
  mutate(year = as.numeric(substr(year_group, 1, 4)) + 1) %>% 
  select(!year_group) %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "variable", values_to = "value") %>%
  mutate(variable = str_remove(variable, "^mean_")) 

# write_csv(national_var_trends, 'variable_trends_national.csv')

