# Function to summarize by scenario (get median and 95% CI)

collapse_by_scenario_cohorts <- function(df, 
                                         by = 'age'){
  
  if(by == 'age'){
    df <- df %>%
      group_by(t, age_grp, int_ID, strategy,age_lower, age_upper, 
               pfpr, seasonality, PEV, PEVstrategy, PEVcov, PEVage, PEVrounds, EPIbooster, EPIextra, massbooster_rep, MDA,
               labels, label_int, strategytype, EPIextra_labels, scen_labels) %>%
      summarize(across(c(n, 
                         cases, sevcases, deaths, 
                         contains('averted'), #totaldoses,
                         cases_per1000pop, sevcases_per1000pop),
                       list(lower = ~quantile(.x, 0.025, na.rm = TRUE),
                            median = ~quantile(.x, 0.5, na.rm = TRUE),
                            upper = ~quantile(.x, 0.975, na.rm = TRUE)),
                       .names = "{.col}_{.fn}") ) %>%
      # rename those variables with _median to be just the variable name 
      rename_with(.fn = \(x)sub("_median","", x))
  } else if (by == 'overall'){
    df <- df %>%
      group_by(int_ID,
               pfpr, seasonality, PEV, PEVstrategy, PEVcov, PEVage, PEVrounds, EPIbooster, EPIextra, massbooster_rep, MDA,
               labels, label_int, strategytype, EPIextra_labels, scen_labels) %>%
      summarize(across(c(n, cases, sevcases, deaths, 
                         contains('averted'), totaldoses,
                         cases_per1000pop, sevcases_per1000pop),
                       list(lower = ~quantile(.x, 0.025, na.rm = TRUE),
                            median = ~quantile(.x, 0.5, na.rm = TRUE),
                            upper = ~quantile(.x, 0.975, na.rm = TRUE)),
                       .names = "{.col}_{.fn}") ) %>%
      # rename those variables with _median to be just the variable name 
      rename_with(.fn = \(x)sub("_median","", x))
  } else if (by == 'ageatvax'){
    df <- df %>%
      group_by(ageatvax, int_ID,strategy,
               pfpr, seasonality, PEV, PEVstrategy, PEVcov, PEVage, PEVrounds, EPIbooster, EPIextra, massbooster_rep, MDA,
               labels, label_int, strategytype, EPIextra_labels, scen_labels) %>%
      summarize(across(c(n, cases, sevcases, deaths, 
                         contains('averted'),  totaldoses,
                         cases_per1000pop, sevcases_per1000pop),
                       list(lower = ~quantile(.x, 0.025, na.rm = TRUE),
                            median = ~quantile(.x, 0.5, na.rm = TRUE),
                            upper = ~quantile(.x, 0.975, na.rm = TRUE)),
                       .names = "{.col}_{.fn}") ) %>%
      # rename those variables with _median to be just the variable name 
      rename_with(.fn = \(x)sub("_median","", x))
  } else if(by == 'ageatvaxandage'){
    df <- df %>%
      group_by(ageatvax, int_ID, age_grp, age_lower, age_upper, 
               strategy,
               pfpr, seasonality, PEV, PEVstrategy, PEVcov, PEVage, PEVrounds, EPIbooster, EPIextra, massbooster_rep, MDA,
               labels, label_int, strategytype, EPIextra_labels, scen_labels) %>%
      summarize(across(c(n, cases, sevcases, deaths, 
                         contains('averted'),  #totaldoses,
                         cases_per1000pop, sevcases_per1000pop),
                       list(lower = ~quantile(.x, 0.025, na.rm = TRUE),
                            median = ~quantile(.x, 0.5, na.rm = TRUE),
                            upper = ~quantile(.x, 0.975, na.rm = TRUE)),
                       .names = "{.col}_{.fn}") ) %>%
      # rename those variables with _median to be just the variable name 
      rename_with(.fn = \(x)sub("_median","", x))
  }
  return(df)
}