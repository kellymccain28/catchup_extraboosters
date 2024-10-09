# Function to output a table of cumulative cases averted per relevant dose (similar to plot_cumul_CA.R)

table_CA_perreldose <- function(df_summ,
                                seas_name = 'perennial'){
  
  dftbl <- df_summ %>%
    filter(age_grp == '0-100') %>%
    filter(pfpr %in% c(0.01, 0.05, 0.25, 0.45, 0.65)) %>%
    filter(seasonality == seas_name) %>%
    mutate(pfpr = as.factor(pfpr)) %>%
    select(c(seasonality, pfpr, labels, PEVstrategy, EPIextra, contains('averted'))) %>%
    select(-contains('daly'), -contains('death'), -contains('clin'), -contains('CU'), -contains('AB', ignore.case = FALSE),
           -cases_averted, -cases_averted_lower, -cases_averted_upper, 
           -severe_averted, -severe_averted_lower, -severe_averted_upper) %>%
    # round all columns 
    mutate(across(where(is.numeric), \(x) round(x, 0))) %>%
    # format the lower, median, and upper values 
    mutate(CA_perpop = paste0(cases_averted_perpop, ' (', cases_averted_perpop_lower, ', ', cases_averted_perpop_upper, ')'),
           SA_perpop = paste0(severe_averted_perpop, ' (', severe_averted_perpop_lower, ', ', severe_averted_perpop_upper, ')'),
           CA_perdose = paste0(cases_averted_perdose, ' (', cases_averted_perdose_lower, ', ', cases_averted_perdose_upper, ')'),
           SA_perdose = paste0(severe_averted_perdose, ' (', severe_averted_perdose_lower, ', ', severe_averted_perdose_upper, ')'),
           CA_perFVC = paste0(cases_averted_perFVC, ' (', cases_averted_perFVC_lower, ', ', cases_averted_perFVC_upper, ')'),
           SA_perFVC = paste0(severe_averted_perFVC, ' (', severe_averted_perFVC_lower, ', ', severe_averted_perFVC_upper, ')')
    ) 
  
  dftbl1 <- dftbl %>%
    filter((EPIextra == '-'  & PEVstrategy == 'catch-up') | (PEVstrategy == 'AB')) %>%
    group_by(pfpr) %>%
    arrange(desc(cases_averted_perdose), .by_group = TRUE) %>%
    select(-c(cases_averted_perdose, cases_averted_perdose_lower, cases_averted_perdose_upper, 
              severe_averted_perdose, severe_averted_perdose_lower, severe_averted_perdose_upper,
              cases_averted_perpop, cases_averted_perpop_lower, cases_averted_perpop_upper,
              severe_averted_perpop, severe_averted_perpop_lower, severe_averted_perpop_upper,
              cases_averted_perFVC, cases_averted_perFVC_lower, cases_averted_perFVC_upper,
              severe_averted_perFVC, severe_averted_perFVC_lower, severe_averted_perFVC_upper,
              seasonality, PEVstrategy, EPIextra)) %>%
    rename(`PfPR2-10` = pfpr,
           Strategy = labels, 
           # `Cases averted` = CA,
           # `Severe cases averted` = SA,
           `Cases averted\nper 1,000 pop` = CA_perpop,
           `Severe cases averted\nper 1,000 pop` = SA_perpop,
           `Cases averted\nper 1,000 doses` = CA_perdose,
           `Severe cases averted\nper 1,000 doses` = SA_perdose,
           `Cases averted\nper 1,000 FVC` = CA_perFVC,
           `Severe cases averted\nper 1,000 FVC` = SA_perFVC)
  
  write.csv(dftbl1, paste0('plots/outcomes_averted_CUorAB_', seas_name, '.csv'), row.names = FALSE)
  
  dftbl2 <- dftbl %>%
    filter(EPIextra != '-'  & PEVstrategy == 'catch-up') %>%
    group_by(pfpr) %>%
    arrange(desc(cases_averted_perdose), .by_group = TRUE) %>%
    select(-c(cases_averted_perdose, cases_averted_perdose_lower, cases_averted_perdose_upper, 
              severe_averted_perdose, severe_averted_perdose_lower, severe_averted_perdose_upper,
              cases_averted_perpop, cases_averted_perpop_lower, cases_averted_perpop_upper,
              severe_averted_perpop, severe_averted_perpop_lower, severe_averted_perpop_upper,
              cases_averted_perFVC, cases_averted_perFVC_lower, cases_averted_perFVC_upper,
              severe_averted_perFVC, severe_averted_perFVC_lower, severe_averted_perFVC_upper,
              seasonality, PEVstrategy, EPIextra)) %>%
    rename(`PfPR2-10` = pfpr,
           Strategy = labels, 
           # `Cases averted` = CA,
           # `Severe cases averted` = SA,
           `Cases averted\nper 1,000 pop` = CA_perpop,
           `Severe cases averted\nper 1,000 pop` = SA_perpop,
           `Cases averted\nper 1,000 doses` = CA_perdose,
           `Severe cases averted\nper 1,000 doses` = SA_perdose,
           `Cases averted\nper 1,000 FVC` = CA_perFVC,
           `Severe cases averted\nper 1,000 FVC` = SA_perFVC)
  
  write.csv(dftbl2, paste0('plots/outcomes_averted_combinedstrategies_', seas_name, '.csv'), row.names = FALSE)
  
  # Get full csv file with all the strategies ranked 
  dftbl3 <- df_summ %>%
    filter(age_grp == '0-100') %>%
    filter(pfpr %in% c(0.01, 0.05, 0.25, 0.45, 0.65)) %>%
    mutate(pfpr = as.factor(pfpr)) %>%
    select(c(seasonality, pfpr, labels, PEVstrategy, EPIextra, contains('averted'))) %>%
    select(-contains('daly'), -contains('death'), -contains('clin'), -contains('CU'), -contains('AB', ignore.case = FALSE),
           -cases_averted, -cases_averted_lower, -cases_averted_upper, 
           -severe_averted, -severe_averted_lower, -severe_averted_upper) %>%
    # round all columns 
    mutate(across(where(is.numeric), \(x) round(x, 0))) %>%
    # format the lower, median, and upper values 
    mutate(CA_perpop = paste0(cases_averted_perpop, ' (', cases_averted_perpop_lower, ', ', cases_averted_perpop_upper, ')'),
           SA_perpop = paste0(severe_averted_perpop, ' (', severe_averted_perpop_lower, ', ', severe_averted_perpop_upper, ')'),
           CA_perdose = paste0(cases_averted_perdose, ' (', cases_averted_perdose_lower, ', ', cases_averted_perdose_upper, ')'),
           SA_perdose = paste0(severe_averted_perdose, ' (', severe_averted_perdose_lower, ', ', severe_averted_perdose_upper, ')'),
           CA_perFVC = paste0(cases_averted_perFVC, ' (', cases_averted_perFVC_lower, ', ', cases_averted_perFVC_upper, ')'),
           SA_perFVC = paste0(severe_averted_perFVC, ' (', severe_averted_perFVC_lower, ', ', severe_averted_perFVC_upper, ')')
    ) %>%
    filter(EPIextra != '-'  & PEVstrategy == 'catch-up') %>%
    arrange(desc(cases_averted_perdose), .by_group = TRUE) %>%
    select(-c(cases_averted_perdose, cases_averted_perdose_lower, cases_averted_perdose_upper, 
              severe_averted_perdose, severe_averted_perdose_lower, severe_averted_perdose_upper,
              cases_averted_perpop, cases_averted_perpop_lower, cases_averted_perpop_upper,
              severe_averted_perpop, severe_averted_perpop_lower, severe_averted_perpop_upper,
              cases_averted_perFVC, cases_averted_perFVC_lower, cases_averted_perFVC_upper,
              severe_averted_perFVC, severe_averted_perFVC_lower, severe_averted_perFVC_upper,
              seasonality, PEVstrategy, EPIextra)) %>%
    rename(`PfPR2-10` = pfpr,
           Strategy = labels, 
           `Cases averted\nper 1,000 pop` = CA_perpop,
           `Severe cases averted\nper 1,000 pop` = SA_perpop,
           `Cases averted\nper 1,000 doses` = CA_perdose,
           `Severe cases averted\nper 1,000 doses` = SA_perdose,
           `Cases averted\nper 1,000 FVC` = CA_perFVC,
           `Severe cases averted\nper 1,000 FVC` = SA_perFVC)
  write.csv(dftbl3, 'plots/outcomes_averted_combinedstrategies_perandseas_sorted.csv', row.names = FALSE)
  

}
