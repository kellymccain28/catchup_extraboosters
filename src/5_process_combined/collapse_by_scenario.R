# Function to summarize by scenario (get median and 95% CI)

collapse_by_scenario <- function(df){
  df <- df %>%
    group_by(t, age_grp, age_lower, age_upper, int_ID, 
             pfpr, seasonality, PEV, PEVstrategy, PEVcov, PEVage, PEVrounds, EPIbooster, EPIextra, massbooster_rep, MDA,
             labels, label_int, strategytype, EPIextra_labels, scen_labels) %>%
    summarize(across(c(clinical, severe, mortality,
                       n, prop_n, contains('dose'), contains('n_pev'),
                       prevalence_2_10, prevalence_0_100,
                       cases, sevcases, deaths, yll, yld, daly,
                       cases_perpop, sevcases_perpop, deaths_perpop,
                       contains('averted'), contains('baseline'), 
                       clinical_diff, p_CA, p_SA, p_clin_diff),
                     list(lower = ~quantile(.x, 0.025, na.rm = TRUE),
                          median = ~quantile(.x, 0.5, na.rm = TRUE),
                          upper = ~quantile(.x, 0.975, na.rm = TRUE)),
                     .names = "{.col}_{.fn}") ) %>%
    # rename those variables with _median to be just the variable name 
    rename_with(.fn = \(x)sub("_median","", x))
  
  return(df)
    
}