# Helper function to process runs over whole simulation by age group 

#' @param aggregation overall or ageyr

process_runs <- function(df, 
                         aggregation,
                         age_scaling){
  
  # Pre-processing, filter out first 5 years where there is no vaccination and reset to 1-30 years
  # timesteps are in days 
  df <- df %>%
    drop_burnin(burnin = 5*365)
  
  if(aggregation == 'last15'){
    df <- df %>%
      drop_burnin(burnin = 15*365) # to get last 15 years of 30 year sim
  }
  
  # Don't run the processing over last 15 years if not none or AB (will save space)
  #' 1. set time_div based on aggregation level
  if(aggregation == 'overall' | aggregation == 'last15'){
    time_div = max(df$timestep)
  } else if(aggregation == 'ageyr'){
    time_div = 182.5# this is half of a year because timesteps are in days # old: timesteps are by month, so dividing by 6 not 365 to get half year timesteps (but will still be 1, 2, 3, etc.)
  } 
  
  #' 2. add doses per timestep
  df <- df %>%
    add_doses()
  
  #' 3. count doses per time division 
  doses_agg <- get_doses(df, 
                         time_div = time_div)
  
  #' 4. get prevalence first by timestep, then aggregate to specified aggregation level 
  prevalence <- get_prev(df,
                         aggregation = aggregation) 
  
  #' 5. get cases per time and age 
  # cases <- get_cases(df,
  #                    time_div = time_div)
  
  #' 6. get rates per timestep then aggregate up
  rates <- get_rates1(df,
                      aggregation) %>%
    mutate(age_scaling = age_scaling)
  
  #' 7. get probabilities of infection and clinical malaria - maybe will need to add this but not sure 
  # probabilities <- get_probabilities(df, 
  #                                    time_div = time_div)
  
  #' 8. join prevalence, cases, rates, doses 
  rates <- rates %>%
    left_join(doses_agg, by = c('halfyear' = 't')) %>%
    left_join(prevalence, by = c('drawID', 'int_ID', 'halfyear', 'time'))
  
  # Fill rates out to single year age groups (code from Lydia)
  # rates2 <- rates %>%
  #   group_by(t) %>%
  #   tidyr::complete(age_lower = c(seq(1,50, by = 0.5), seq(50, 99, by = 1))) %>%
  #   mutate(agediff = age_upper - age_lower) %>%
  #   mutate(age_upper = ifelse((is.na(age_upper) | agediff == 5) & (age_lower > 20), age_lower + 1, age_upper)) %>%
  #   ungroup() %>%
  #   tidyr::fill(clinical, severe, mortality,
  #               pfpr, seasonality, PEV, PEVstrategy, PEVcov, PEVage, PEVrounds, EPIbooster, EPIextra, massbooster_rep, MDA, int_ID,
  #               drawID, EIR, ID, sim_length, time_div, starts_with('dose'), starts_with('n_pev'),
  #               prevalence_2_10, prevalence_0_100) 
  # 
  # # GEt estimates for single year age groups using beers package mrc-ide/beers
  # n <- rates2$n[rates2$age_lower %in% seq(50, 100, by = 5)] # ns to subdivide
  # t <- rates2$t[rates2$age_lower %in% seq(50, 100, by = 5)] # years for the ns to be subdivided
  # age_lower <- rates2$age_lower[rates2$age_lower %in% seq(50, 100, by = 5)]
  # new_ns <- beers_sub_modified(n) # subdividing the ns
  # # bind all of these values together and add in extra age groups 
  # ns <- as.data.frame(cbind(n, age_lower, t)) %>%
  #   group_by(t) %>%
  #   tidyr::complete(age_lower = seq(50,99.5, by = 1)) 
  # ns <- cbind(ns, new_ns) %>% select(-n) 
  # colnames(ns)[3] <- 'new_ns'
  # 
  # # join to the rates df
  # rates3 <- rates2 %>% left_join(ns) %>% 
  #   select(new_ns, everything()) %>%
  #   mutate(n = ifelse(!is.na(.data$new_ns), new_ns, n)) %>%
  #   select(-new_ns, -agediff) 
  
  # get cases and deaths
  processed <- rates %>%
    # add cases calculation 
    mutate(cases = clinical * person_days,
           sevcases = severe * person_days,
           deaths = mortality * person_days) %>%
    # remove yll and dalys because these aren't working with half-year age groups (because of 1 year age bands in life table)
    select(-yll, -dalys)
  
  # Add in n value - we want average n over time per age group, not sum (which is what person_days is)
  # to get this we can just divide the summed value by the number of timesteps over which it was summed
  processed$n <- processed$person_days / time_div
  
  #' 9. add severe outcomes
  # processed <- processed %>%
  #   mortality_rate() |> 
  #   daly_components() # dalys aren't working because we have half year age groups and life expectancy table is in yearly age group 
  
  processed$age_scaling <- age_scaling
  
  #' 10. save processed run depending on which  
  if(aggregation == 'overall'){
    saveRDS(processed, "processed_run_overall.rds")
  } else if(aggregation == 'ageyr'){
    processed <- processed %>%
      filter(age_lower < 50)
    saveRDS(processed, "processed_run_ageyr.rds")
  } else if(aggregation == 'last15'){
    saveRDS(processed, "processed_run_last15.rds")
  }
}
