# Function that takes a dataframe with draws summarized by age and year and summarizes all variables and gives t = 1 
# (need to pre-filter) over time period you want
summ_over_time_horizon <- function(df){

  df <- df %>%
    group_by(scenario, ID, drawID,# t,
             age_lower, age_upper, int_ID, pfpr, seasonality, PEV, PEVcov, PEVstrategy, PEVage, 
             PEVrounds, EPIbooster, EPIextra, massbooster_rep, MDA) %>%
    summarize(
      # get means of rates/proportions
      across(c(n:prop_n,
               prevalence_2_10, prevalence_0_100,
               clinical, severe, mortality), # this will give mean annual incidence over the time period specified NOT total incidence  
             mean),
      #get sums of count variables 
      across(c(sevcases:daly,
               starts_with('dose'), starts_with('n_pev')), # get sum of all the doses delivered over the time period 
             sum),
    ) %>%
    mutate(t = 1) # add t variable back in 
  message('summed over time horizon')
  return(df)
}
