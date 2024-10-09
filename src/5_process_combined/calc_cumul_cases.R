# Function to get cumulative outcomes per year
# not used at the moment 
calc_cumul_cases <- function(df){
  
  df <- df %>%
    arrange(t, age_grp, age_lower, age_upper, ID, int_ID, 
            pfpr, seasonality, PEV, PEVstrategy, PEVcov, PEVage, PEVrounds, 
            EPIbooster, EPIextra, massbooster_rep, MDA) %>%
    group_by(age_grp, age_lower, age_upper, ID, int_ID, 
             pfpr, seasonality, PEV, PEVstrategy, PEVcov, PEVage, PEVrounds,
             EPIbooster, EPIextra, massbooster_rep, MDA) %>%
    mutate(across(c(cases, sevcases, 
                    cases_averted, severe_averted),
                  .fns = cumsum, 
                  .names = "cumul_{.col}") )
  
  return(df)
}