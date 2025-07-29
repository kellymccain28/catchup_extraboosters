# Get % of cases among children under 5 per scenario (to go with age distribution plot)
 # will use the overall df for this 


dfu5 <- df_summ_draws %>% 
  filter(age_grp == '0-5' | age_grp == '0-100' | age_grp == '5-10' | age_grp == '10-15') %>%
  select(-age_lower, -age_upper) %>%
  select(labels,age_grp, halfyear, PEVstrategy, #drawID,
         PEVage, PEVrounds, EPIbooster, EPIextra, massbooster_rep, MDA, label_int, pfpr, seasonality,
         cases, 
         sevcases,
         cases_averted, 
         severe_averted,
         n) %>%
  mutate(school_aged = ifelse(age_grp == '5-10' | age_grp == '10-15', 1, 0),
         U5_CA = ifelse(age_grp == '0-5', cases_averted, NA),
         U5_SA = ifelse(age_grp == '0-5', severe_averted, NA),
         U5_cases = ifelse(age_grp == '0-5', cases, NA),
         U5_severe = ifelse(age_grp == '0-5', sevcases, NA),
         school_cases = ifelse(school_aged == 1, cases, NA),
         school_severe = ifelse(school_aged == 1, sevcases, NA)) %>%
  group_by(labels, halfyear, PEVstrategy, PEVage, PEVrounds, EPIbooster, EPIextra, massbooster_rep, MDA, 
           label_int, pfpr, seasonality) %>%
  tidyr::fill(U5_CA, .direction = 'downup') %>%
  tidyr::fill(U5_SA, .direction = 'downup') %>%
  tidyr::fill(U5_cases, .direction = 'downup') %>%
  tidyr::fill(U5_severe, .direction = 'downup') %>%
  tidyr::fill(school_cases, .direction = 'downup') %>%
  tidyr::fill(school_severe, .direction = 'downup') %>%
  mutate(perc_CA_in_U5 = ifelse(age_grp == '0-100', U5_CA / cases_averted * 100, NA),
         perc_SA_in_U5 = ifelse(age_grp == '0-100', U5_SA / severe_averted * 100, NA),
         p_cases_U5 = ifelse(age_grp == '0-100', U5_cases / cases * 100, NA),
         p_severe_U5 = ifelse(age_grp == '0-100', U5_severe / sevcases * 100, NA),
         p_cases_school = ifelse(age_grp == '0-100', school_cases / cases * 100, NA),
         p_severe_school = ifelse(age_grp == '0-100', school_severe / sevcases * 100, NA)) %>%
  # filter(age_grp == '0-100') %>%
  group_by(labels, age_grp, halfyear, 
           PEVstrategy, PEVage, PEVrounds, EPIbooster, EPIextra, massbooster_rep, MDA, 
           label_int, pfpr, seasonality) %>%
  # Get median, and 95% CrI for each of the variables
  summarize(across(c(cases, sevcases, 
                     U5_cases, U5_severe, p_cases_U5, p_severe_U5,
                     U5_CA, U5_SA,
                     perc_CA_in_U5, perc_SA_in_U5,
                     cases_averted, 
                     severe_averted,
                     school_cases, school_severe, 
                     p_cases_school, p_severe_school),
  list(lower = ~quantile(.x, 0.025, na.rm = TRUE),
       median = ~quantile(.x, 0.5, na.rm = TRUE),
       upper = ~quantile(.x, 0.975, na.rm = TRUE)),
  .names = "{.col}_{.fn}") ) %>%
  rename_with(.fn = \(x)sub("_median","", x))

# Get percent of cases under 5 at baseline
baseline_seas <- dfu5 %>% 
  filter(seasonality == 'seasonal') %>%
  filter(PEVstrategy == 'none', ) %>%
  filter(pfpr == 0.05 | pfpr == 0.25 | pfpr == 0.45) %>%
  arrange(p_cases_U5) %>% 
  mutate(across(starts_with('p_'), \(x) round(x, 0))) %>%
  select(c(p_cases_U5, p_cases_U5_lower, p_cases_U5_upper , 
           p_cases_school, p_cases_school_lower, p_cases_school_upper, 
           p_severe_school, p_severe_school_lower, p_severe_school_upper, seasonality))
# View(baseline_seas) 

baseline_per <- dfu5 %>% 
  filter(seasonality == 'perennial') %>%
  filter(PEVstrategy == 'none') %>%
  filter(pfpr == 0.05 | pfpr == 0.25 | pfpr == 0.45) %>%
  arrange(p_cases_U5) %>% 
  mutate(across(starts_with('p_'), \(x) round(x, 0))) %>%
  select(c(p_cases_U5, p_cases_U5_lower, p_cases_U5_upper , 
           p_cases_school, p_cases_school_lower, p_cases_school_upper, 
           p_severe_school, p_severe_school_lower, p_severe_school_upper, seasonality)) 
# View(baseline_per) 

# Get percent of cases under 5 after vaccination 
vac_seas <- dfu5 %>% 
  filter(seasonality == 'seasonal') %>%
  filter(age_grp != '0-5') %>%
  filter((PEVstrategy == 'AB' & EPIextra == '-') | (PEVstrategy == 'catch-up' &  EPIextra == '-')) %>%
  filter(pfpr == 0.05 | pfpr == 0.25 | pfpr == 0.45) %>%
  arrange(p_cases_U5) %>% 
  mutate(across(starts_with('p_'), \(x) round(x, 0))) %>%
  select(c(p_cases_U5, p_cases_U5_lower, p_cases_U5_upper , 
           p_cases_school, p_cases_school_lower, p_cases_school_upper, 
           p_severe_school, p_severe_school_lower, p_severe_school_upper, seasonality)) 
  # mutate(p_cases_U5 = paste0(p_cases_U5, ' (', p_cases_U5_lower, '-', p_cases_U5_upper,')')) %>% View() 

vac_per <- dfu5 %>% 
  filter(seasonality == 'perennial') %>%
  filter(age_grp != '0-5') %>%
  filter((PEVstrategy == 'AB' & EPIextra == '-') | (PEVstrategy == 'catch-up' &  EPIextra == '-')) %>%
  filter(pfpr == 0.05 | pfpr == 0.25 | pfpr == 0.45) %>%
  arrange(p_cases_U5) %>% 
  mutate(across(starts_with('p_'), \(x) round(x, 0))) %>%
  select(c(p_cases_U5, p_cases_U5_lower, p_cases_U5_upper , 
           p_cases_school, p_cases_school_lower, p_cases_school_upper, 
           p_severe_school, p_severe_school_lower, p_severe_school_upper, seasonality)) 
# mutate(p_cases_U5 = paste0(p_cases_U5, ' (', p_cases_U5_lower, '-', p_cases_U5_upper,')')) %>% View() 

# dfu5 %>% 
#   filter(seasonality == 'perennial') %>%
#   filter(age_grp == '0-100') %>%
#   filter((PEVstrategy == 'AB' & EPIextra == '-') | (PEVstrategy == 'catch-up' &  EPIextra == '-')) %>%
#   filter(pfpr == 0.45) %>%
#   arrange(p_cases_U5) %>% 
#   mutate(across(starts_with('p_'), \(x) round(x, 2))) %>%
#   mutate(p_cases_U5 = paste0(p_cases_U5, ' (', p_cases_U5_lower, '-', p_cases_U5_upper,')')) %>% View() 
# # Get percent of cases AVERTED under 5 after vaccination 
# dfu5 %>% 
#   filter(seasonality == 'seasonal') %>%
#   filter((PEVstrategy == 'AB' & EPIextra == '-') | (PEVstrategy == 'catch-up' &  EPIextra == '-')) %>%
#   filter(pfpr == 0.45) %>%
#   arrange(perc_CA_in_U5) %>% 
#   select(c(U5_CA, cases_averted, 
#            U5_SA, severe_averted, 
#            perc_CA_in_U5, perc_CA_in_U5_lower, perc_CA_in_U5_upper, 
#            perc_SA_in_U5 , perc_SA_in_U5_lower, perc_SA_in_U5_upper, seasonality)) %>% 
#   mutate(across(starts_with('perc'), \(x) round(x, 2))) %>% View() 


# Make nice table for SI 
perccases <- rbind(baseline_seas, baseline_per, 
                     vac_seas, vac_per) %>%
  filter(age_grp == '0-100') %>% ungroup() %>%
  mutate(p_cases_U5 = paste0(p_cases_U5, ' (', p_cases_U5_lower, '-', p_cases_U5_upper,')'),
         # p_severe_U5 = paste0(p_severe_U5, ' (', p_severe_U5_lower, '-', p_severe_U5_upper,')'),
         p_cases_school = paste0(p_cases_school, ' (', p_cases_school_lower, '-', p_cases_school_upper, ")"),
         # p_severe_school = paste0(p_severe_school, ' (', p_severe_school_lower, '-', p_severe_school_upper, ")")
         ) %>%
  select(labels, seasonality, pfpr, p_cases_U5, p_cases_school) %>%
  group_by(seasonality, pfpr) %>%
  arrange(pfpr)
write.csv(perccases, 'plots/percent_cases.csv', row.names = FALSE)
