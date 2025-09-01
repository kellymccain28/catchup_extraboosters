
get_icers <- function(df){
  # Filter out hybrid and SV because are not going to actually compare them in the paper 
  df1 <- df %>%
    filter(PEVstrategy != 'SV' & PEVstrategy !='hybrid') %>%
    filter(age_lower == 0 & age_upper == 100) %>%
    filter(PEV != 'none' & !(PEVage == '-' & EPIextra == '-')) %>%
    mutate(idstrategy = paste0(PEVstrategy, "  ", PEVage, ", ", EPIextra))
  
  pfprvals <- c(0.01,0.03,0.05,0.25,0.45,0.65)
  seasonalities <- c('perennial','seasonal')
  combos <- expand.grid(pfprvals, seasonalities) 
  colnames(combos) <- c('pfprval','seas')
  
  icers_clinical_all <- pmap(combos, 
                             function(pfprval, seas){
                               d <- df1 %>% 
                                 filter(pfpr == pfprval & seasonality == seas)
                               
                               params <- data.frame(
                                 placeholder = seq(1,50, 1) # the function to convert to the PSA object requires parameters
                               )
                               
                               effects <- d %>% 
                                 mutate(cases_averted_routine_perpop = cases_averted_routine / n * 1000) %>%
                                 select(drawID, idstrategy, cases_averted_routine_perpop) %>%
                                 pivot_wider(names_from = idstrategy, 
                                             values_from = cases_averted_routine_perpop) %>%
                                 select(-drawID)
                               
                               dosenums <- d %>%
                                 mutate(additional_doses_perpop = additional_doses/ n) %>%#) %>%# 
                                 select(drawID, idstrategy, additional_doses_perpop) %>%
                                 pivot_wider(names_from = idstrategy, 
                                             values_from = additional_doses_perpop) %>% 
                                 select(-drawID)
                               
                               psa_obj <- make_psa_obj(cost = dosenums,
                                                       effectiveness = effects,
                                                       parameters = params,
                                                       strategies = unique(d$idstrategy),
                                                       currency = "doses per person")
                               
                               psa_sum <- summary(psa_obj,
                                                  calc_sds = TRUE)
                               
                               icers <- calculate_icers(cost = psa_sum$meanCost,
                                                        effect = psa_sum$meanEffect,
                                                        strategies = psa_sum$Strategy)
                               
                               icers$pfpr <- pfprval
                               icers$seasonality <- seas
                               
                               return(icers)
                               
                             })
  
  icers_severe_all <- pmap(combos, 
                           function(pfprval, seas){
                             d <- df1 %>% 
                               filter(pfpr == pfprval & seasonality == seas)
                             
                             params <- data.frame(
                               placeholder = seq(1,50, 1) # the function to convert to the PSA object requires parameters
                             )
                             
                             effects <- d %>% 
                               mutate(severe_averted_routine_perpop = severe_averted_routine / n * 1000) %>%
                               select(drawID, idstrategy, severe_averted_routine_perpop) %>%
                               pivot_wider(names_from = idstrategy, 
                                           values_from = severe_averted_routine_perpop) %>%
                               select(-drawID)
                             
                             dosenums <- d %>%
                               mutate(additional_doses_perpop = additional_doses/ n) %>%#) %>%# 
                               select(drawID, idstrategy, additional_doses_perpop) %>%
                               pivot_wider(names_from = idstrategy, 
                                           values_from = additional_doses_perpop) %>% 
                               select(-drawID)
                             
                             psa_obj <- make_psa_obj(cost = dosenums,
                                                     effectiveness = effects,
                                                     parameters = params,
                                                     strategies = unique(d$idstrategy),
                                                     currency = "doses per person")
                             
                             psa_sum <- summary(psa_obj,
                                                calc_sds = TRUE)
                             
                             icers <- calculate_icers(cost = psa_sum$meanCost,
                                                      effect = psa_sum$meanEffect,
                                                      strategies = psa_sum$Strategy)
                             
                             icers$pfpr <- pfprval
                             icers$seasonality <- seas
                             
                             return(icers)
                             
                           })
  
  icers_clinical_all <- bind_rows(icers_clinical_all) %>%
    mutate(type = 'clinical')
  icers_severe_all <- bind_rows(icers_severe_all) %>%
    mutate(type = 'severe')
  
  icers_all <- rbind(icers_clinical_all, icers_severe_all) %>%
    filter(Status != 'D') %>%
    filter(pfpr %in% c(0.05,0.25,0.45)) %>%
    separate(Strategy,
             into = c("PEVstrategy", "PEVage", "EPIextra"),
             sep = "\\.\\.",
             remove = FALSE) %>%
    mutate(across(c(PEVstrategy, PEVage),
                  .fns = ~ str_replace(.x, '\\.', '-'))) %>%
    mutate(across(c(EPIextra), ~ .x |>
                    str_replace("^\\.", "") |>        # remove leading .
                    str_replace("^\\.$", "") |>       # replace "." alone with empty
                    str_replace_all("\\.", "+")       # convert remaining . to -
    )) %>%
    mutate(category = ifelse(EPIextra != '' & PEVage == '', 'Extra booster(s)', 
                             ifelse(PEVage != '' & EPIextra == '', 'Catch-up', 'Combined')),
           EPIextra = factor(EPIextra, levels = c('2y','5y','10y','2y+5y','2y+10y','5y+10y','2y+5y+10y','')),
           PEVage = ifelse(PEVage == '5-9','5-9y', ifelse(PEVage == '5-14','5-14y', PEVage)),
           PEVage = factor(as.factor(PEVage), levels = c('6m-2y','6m-4y','6m-9y','6m-14y','5-9y','5-14y',''))) %>%
    filter(category !='Routine age-based')
  
  return(icers_all)
  }

