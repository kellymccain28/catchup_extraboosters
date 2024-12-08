# Helper function to process runs over whole simulation by age group 

#' @param aggregation overall or ageyr

process_runs <- function(df, 
                         aggregation){
  
  # Pre-processing, filter out first 5 years where there is no vaccination and reset to 1-30 years
  df <- df %>%
    drop_burnin(burnin = 5*12)
  
  if(aggregation == 'last15'){
    df <- df %>%
      drop_burnin(burnin = 5*15) # to get last 15 years of 30 year sim
  }
  
  # Don't run the processing over last 15 years if not none or AB (will save space)
  #' 1. set time_div based on aggregation level
  if(aggregation == 'overall' | aggregation == 'last15'){
    time_div = max(df$timestep)
  } else if(aggregation == 'ageyr'){
    time_div = 6 # timesteps are by month, so dividing by 6 not 365 to get half year timesteps (but will still be 1, 2, 3, etc.)
  } 
  
  #' 2. add doses
  df <- df %>%
    add_doses()
  
  #' 3. count doses per time division 
  doses_agg <- get_doses(df, 
                         time_div = time_div)
  
  #' 4. get prevalence
  prevalence <- get_prev(df, 
                         time_div = time_div)
  
  #' 5. get cases per time and age 
  # cases <- get_cases(df,
  #                    time_div = time_div)
  
  #' 6. get rates 
  rates <- get_rates1(df, 
                      time_div = time_div)
  
  #' 7. get probabilities of infection and clinical malaria - maybe will need to add this but not sure 
  # probabilities <- get_probabilities(df, 
  #                                    time_div = time_div)
  
  #' 8. join prevalence, cases, rates, doses + probabilities
  rates <- rates %>%
    left_join(doses_agg, by = 't') %>%
    left_join(prevalence, by = c('drawID', 'int_ID', 't'))
  
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
  
  # get cases
  processed <- rates %>%
    # add cases calculation 
    mutate(cases = clinical * n,
           sevcases = severe * n)
  
  #' 9. add severe outcomes
  processed <- processed %>%
    mortality_rate() |>
    daly_components()
  
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
