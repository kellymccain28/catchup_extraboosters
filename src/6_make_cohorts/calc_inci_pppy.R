calc_inci_pppy <- function(df){
  
  vaccov <- df %>%
    filter(t == 1) %>%
    filter(age_grp == '0-0' | age_grp == '0-1') %>%
    filter(PEVstrategy != 'none') %>%
    select(c(age_grp, pfpr, seasonality, PEVstrategy,
             PEVage, EPIbooster, EPIextra, drawID,strategy,
             int_ID, n)) %>%
    #find n who were 0.5-1 who received mass vaccine using set vaccine coverage (0.5 was rounded down to 0)
    mutate(nvax0.5_1 = ifelse(age_grp == '0-1', n * 0.8, 0), # coverage was 80% for primary series (can't use mass dose 3 bc will count everyone given dose at that time, not just 0-1yos)
           # find n who were 0-0.5 who did not receive vaccine
           n0_0.5 = ifelse(age_grp == '0-0', n, NA) 
    ) %>%
    group_by(pfpr, seasonality, PEVstrategy,
             PEVage, EPIbooster, EPIextra, drawID,strategy,
             int_ID) %>%
    fill(n0_0.5, .direction = 'downup') %>%
    rowwise() %>%
    mutate(n0_1 = ifelse(age_grp == '0-1', sum(n, n0_0.5, na.rm = TRUE), NA)) %>%
    # find vaccine coverage among 0-1 year olds for each CU and AB strategy
    mutate(vac_cov_0_1 = nvax0.5_1 / n0_1) %>%
    filter(age_grp == '0-1') %>% select(-age_grp, -n)
  
  df2 <- df %>% distinct() %>%
    left_join(vaccov, by = join_by(pfpr, seasonality, PEVstrategy, PEVage, EPIbooster, EPIextra, drawID,
                                   int_ID, strategy)) %>%
    ungroup() 
    
    # Find number of cases in age-based scenario with no extra boosters for people at each age group
    mutate(baselineABcases = ifelse(PEVstrategy == 'AB' & EPIextra == '-', cases, NA),
           baseline_cases = ifelse(PEVstrategy == 'none', cases, NA),
           ABcases = ifelse(PEVstrategy == 'AB', cases, NA),
           CUcases = ifelse(PEVstrategy == 'catch-up', cases, NA)) %>%
    group_by(pfpr, seasonality, age_grp, drawID) %>%
    fill(baselineABcases, .direction = 'downup') %>%
    fill(baseline_cases, .direction = 'downup') %>%
    # Find catch-up cases averted per population per year - per pop vaccinated? 
    mutate(CUcases_averted_perpop = ifelse((t = 2 & age_grp == '1-2') | (t = 3 & age_grp == '2-3') | (t = 4 & age_grp =='3-4') | (t = 5 & age_grp =='4-5') |
                                              (t = 6 & age_grp =='5-6') |(t = 7 & age_grp =='6-7') | (t = 8 & age_grp =='7-8') |(t = 9 & age_grp =='8-9') |
                                              (t = 10 & age_grp =='9-10') |(t = 11 & age_grp =='10-11') |(t = 12 & age_grp =='11-12') | (t = 13 & age_grp =='12-13') |
                                              (t = 14 & age_grp =='13-14') |(t = 15 & age_grp =='14-15') |(t = 16 & age_grp =='15-16') |(t = 17 & age_grp =='16-17') |
                                              (t = 18 & age_grp =='17-18') |(t = 19 & age_grp =='18-19') | (t = 20 & age_grp =='19-20') | (t = 21 & age_grp =='20-21') |
                                              (t = 22 & age_grp =='21-22') |(t = 23 & age_grp =='22-23') |(t = 24 & age_grp =='23-24') |(t = 25 & age_grp =='24-25') |
                                              (t = 26 & age_grp =='25-26'), 
                                            (baselineABcases - CUcases) / (n * vac_cov_0_1), NA),
           ABcases_perpop = ifelse(PEVstrategy == 'AB' & (t = 2 & age_grp == '1-2') | (t = 3 & age_grp == '2-3') | (t = 4 & age_grp =='3-4') | (t = 5 & age_grp =='4-5') |
                                     (t = 6 & age_grp =='5-6') |(t = 7 & age_grp =='6-7') | (t = 8 & age_grp =='7-8') |(t = 9 & age_grp =='8-9') |
                                     (t = 10 & age_grp =='9-10') |(t = 11 & age_grp =='10-11') |(t = 12 & age_grp =='11-12') | (t = 13 & age_grp =='12-13') |
                                     (t = 14 & age_grp =='13-14') |(t = 15 & age_grp =='14-15') |(t = 16 & age_grp =='15-16') |(t = 17 & age_grp =='16-17') |
                                     (t = 18 & age_grp =='17-18') |(t = 19 & age_grp =='18-19') | (t = 20 & age_grp =='19-20') | (t = 21 & age_grp =='20-21') |
                                     (t = 22 & age_grp =='21-22') |(t = 23 & age_grp =='22-23') |(t = 24 & age_grp =='23-24') |(t = 25 & age_grp =='24-25') |
                                     (t = 26 & age_grp =='25-26'), ABcases / (n * vac_cov_0_1), NA),
           ABcases_averted_perpop = ifelse(PEVstrategy == 'AB' & (t = 2 & age_grp == '1-2') | (t = 3 & age_grp == '2-3') | (t = 4 & age_grp =='3-4') | (t = 5 & age_grp =='4-5') |
                                             (t = 6 & age_grp =='5-6') |(t = 7 & age_grp =='6-7') | (t = 8 & age_grp =='7-8') |(t = 9 & age_grp =='8-9') |
                                             (t = 10 & age_grp =='9-10') |(t = 11 & age_grp =='10-11') |(t = 12 & age_grp =='11-12') | (t = 13 & age_grp =='12-13') |
                                             (t = 14 & age_grp =='13-14') |(t = 15 & age_grp =='14-15') |(t = 16 & age_grp =='15-16') |(t = 17 & age_grp =='16-17') |
                                             (t = 18 & age_grp =='17-18') |(t = 19 & age_grp =='18-19') | (t = 20 & age_grp =='19-20') | (t = 21 & age_grp =='20-21') |
                                             (t = 22 & age_grp =='21-22') |(t = 23 & age_grp =='22-23') |(t = 24 & age_grp =='23-24') |(t = 25 & age_grp =='24-25') |
                                             (t = 26 & age_grp =='25-26'), baseline_cases - ABcases / (n * vac_cov), NA)) %>%
    group_by(pfpr, seasonality) %>%
    fill(ABcases_perpop, .direction = 'downup') %>%
    # Find the incidence per person per year in CU 
    mutate(CUcases_perpop = ABcases_perpop - CUcases_averted_perpop,
           CUcases = CUcases_perpop * n) %>%
    # filter out unvaccinated 0-6m group
    filter(age_grp != '0-0')
  
  return(df)
}

# p <- df %>% ungroup() %>%filter(age_lower < 4 & PEVstrategy!='none'&drawID == 40) %>%
#   select(age_grp,drawID:EPIextra, n, nvaxCU0.5_1, nvaxAB0.5_1, nCU0.5_1, nAB0.5_1, n0_0.5,
#          vac_cov, vac_cov,
#          cases, ABcases, CUcases,
#          CU_cases_averted_perpop, ABcases_perpop, CU_cases_perpop, CU_cases)

