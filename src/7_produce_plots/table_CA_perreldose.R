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
              seasonality, PEVstrategy, EPIextra)) 
  

  dftbl2 <- dftbl1 %>%
    filter(pfpr != 0.01 & pfpr != 0.65) 
  dftbl2[,2] <- lapply(dftbl2[, 2], function(col) {
    gsub(", ", "; ", col)  # Replace commas with semicolons
  })
  dftbl2 <- dftbl2 %>%
    mutate(setting = case_when(
      pfpr == 0.05 & seas_name == 'perennial' ~ "Perennial low transmission: \\textit{Pf}PR_{2-10} = 5\\%",
      pfpr == 0.25 & seas_name == 'perennial' ~ "Perennial moderate transmission: \\textit{Pf}PR_{2-10} = 25\\%",
      pfpr == 0.45 & seas_name == 'perennial' ~ "Perennial moderately high transmission: \\textit{Pf}PR_{2-10} = 45\\%",
      pfpr == 0.65 & seas_name == 'perennial' ~ "Perennial moderate transmission: \\textit{Pf}PR_{2-10} = 65\\%",
      pfpr == 0.05 & seas_name == 'seasonal' ~ "Seasonal low transmission: \\textit{Pf}PR_{2-10} = 5\\%",
      pfpr == 0.25 & seas_name == 'seasonal' ~ "Seasonal moderate transmission: \\textit{Pf}PR_{2-10} = 25\\%",
      pfpr == 0.45 & seas_name == 'seasonal' ~ "Seasonal moderately high transmission: \\textit{Pf}PR_{2-10} = 45\\%",
      pfpr == 0.65 & seas_name == 'seasonal' ~ "Seasonal moderate transmission: \\textit{Pf}PR_{2-10} = 65\\%"
    )) %>% ungroup() %>%
    select(-pfpr) %>%
    insert_blank_rows_latex('setting') #%>%
    # mutate(mergerow = ifelse(grepl('transmission', labels), TRUE, FALSE)) 
    
  
  dftbl2[,2:7] <- lapply(dftbl2[, 2:7], function(col) {
    if (is.character(col)) {
      gsub(", ", "-", col)  # Replace commas with dashes
    } else {
      col  # Leave non-character columns unchanged
    }
  })
  
  dftbl2 <- dftbl2 %>%
    rename(Strategy = labels, 
           `Cases averted per 1000 pop` = CA_perpop,
           `Severe cases averted per 1000 pop` = SA_perpop,
           `Cases averted per 1000 doses` = CA_perdose,
           `Severe cases averted per 1000 doses` = SA_perdose,
           `Cases averted per 1000 FVC` = CA_perFVC,
           `Severe cases averted per 1000 FVC` = SA_perFVC)
  # dftbl2[nrow(dftbl2)+1,] <- rep('',8)#colnames(dftbl2)
  # emptyrow <- dftbl2[46,]
  
  # dftbl22 <- rbind(emptyrow, dftbl2[-46,])
  
  write.csv(dftbl1 %>%
              rename(`PfPR2-10` = pfpr,
                     Strategy = labels, 
                     `Cases averted\nper 1000 pop` = CA_perpop,
                     `Severe cases averted\nper 1000 pop` = SA_perpop,
                     `Cases averted\nper 1000 doses` = CA_perdose,
                     `Severe cases averted\nper 1000 doses` = SA_perdose,
                     `Cases averted\nper 1000 FVC` = CA_perFVC,
                     `Severe cases averted\nper 1000 FVC` = SA_perFVC), 
            paste0('plots/outcomes_averted_CUorAB_', seas_name, '.csv'), row.names = FALSE)
  write.table(dftbl2, file = paste0('plots/outcomes_averted_CUorAB_', seas_name, 'forlatex.csv'), 
              sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE) # Table 2 (perennial version)
  
  
  # Combined strategies only 
  dftbl3 <- dftbl %>%
    filter(EPIextra != '-'  & PEVstrategy == 'catch-up') %>%
    group_by(pfpr) %>%
    arrange(desc(cases_averted_perdose), .by_group = TRUE) %>%
    select(-c(cases_averted_perdose, cases_averted_perdose_lower, cases_averted_perdose_upper, 
              severe_averted_perdose, severe_averted_perdose_lower, severe_averted_perdose_upper,
              cases_averted_perpop, cases_averted_perpop_lower, cases_averted_perpop_upper,
              severe_averted_perpop, severe_averted_perpop_lower, severe_averted_perpop_upper,
              cases_averted_perFVC, cases_averted_perFVC_lower, cases_averted_perFVC_upper,
              severe_averted_perFVC, severe_averted_perFVC_lower, severe_averted_perFVC_upper,
              seasonality, PEVstrategy, EPIextra)) 
  
  dftbl4 <- dftbl3 %>%
    filter(pfpr != 0.01 & pfpr != 0.65) %>%
    mutate(setting = case_when(
      pfpr == 0.05 & seas_name == 'perennial' ~ "Perennial low transmission: \textit{Pf}PR_{2-10} = 5%",
      pfpr == 0.25 & seas_name == 'perennial' ~ "Perennial moderate transmission: \textit{Pf}PR_{2-10} = 25%",
      pfpr == 0.45 & seas_name == 'perennial' ~ "Perennial moderately high transmission: \textit{Pf}PR_{2-10} = 45%",
      pfpr == 0.65 & seas_name == 'perennial' ~ "Perennial moderate transmission: \textit{Pf}PR_{2-10} = 65%",
      pfpr == 0.05 & seas_name == 'seasonal' ~ "Seasonal low transmission: \textit{Pf}PR_{2-10} = 5%",
      pfpr == 0.25 & seas_name == 'seasonal' ~ "Seasonal moderate transmission: \textit{Pf}PR_{2-10} = 25%",
      pfpr == 0.45 & seas_name == 'seasonal' ~ "Seasonal moderately high transmission: \textit{Pf}PR_{2-10} = 45%",
      pfpr == 0.65 & seas_name == 'seasonal' ~ "Seasonal moderate transmission: \textit{Pf}PR_{2-10} = 65%"
    )) %>% ungroup() %>%
    select(-pfpr) %>%
    insert_blank_rows_latex('setting')
  
  write.csv(dftbl3 %>%
              rename(`PfPR2-10` = pfpr,
                     Strategy = labels,
                     `Cases averted\nper 1,000 pop` = CA_perpop,
                     `Severe cases averted\nper 1,000 pop` = SA_perpop,
                     `Cases averted\nper 1,000 doses` = CA_perdose,
                     `Severe cases averted\nper 1,000 doses` = SA_perdose,
                     `Cases averted\nper 1,000 FVC` = CA_perFVC,
                     `Severe cases averted\nper 1,000 FVC` = SA_perFVC), 
            file = paste0('plots/outcomes_averted_combinedstrategies_', seas_name, '.csv'), row.names = FALSE)
  write.table(dftbl4, file = paste0('plots/outcomes_averted_combinedstrategies_', seas_name, 'forlatex.csv'), 
              sep = ",", row.names = FALSE, col.names = FALSE) # Table S4
  
  # Get full csv file with all the strategies ranked 
  dftbl5 <- df_summ %>%
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
              seasonality, PEVstrategy, EPIextra)) 
  
  dftbl6 <- dftbl5 %>%
    filter(pfpr != 0.01 & pfpr != 0.65) %>%
    mutate(setting = case_when(
      pfpr == 0.05 & seas_name == 'perennial' ~ "Perennial low transmission: \textit{Pf}PR_{2-10} = 5%",
      pfpr == 0.25 & seas_name == 'perennial' ~ "Perennial moderate transmission: \textit{Pf}PR_{2-10} = 25%",
      pfpr == 0.45 & seas_name == 'perennial' ~ "Perennial moderately high transmission: \textit{Pf}PR_{2-10} = 45%",
      pfpr == 0.65 & seas_name == 'perennial' ~ "Perennial moderate transmission: \textit{Pf}PR_{2-10} = 65%",
      pfpr == 0.05 & seas_name == 'seasonal' ~ "Seasonal low transmission: \textit{Pf}PR_{2-10} = 5%",
      pfpr == 0.25 & seas_name == 'seasonal' ~ "Seasonal moderate transmission: \textit{Pf}PR_{2-10} = 25%",
      pfpr == 0.45 & seas_name == 'seasonal' ~ "Seasonal moderately high transmission: \textit{Pf}PR_{2-10} = 45%",
      pfpr == 0.65 & seas_name == 'seasonal' ~ "Seasonal moderate transmission: \textit{Pf}PR_{2-10} = 65%"
    )) %>% ungroup() %>%
    select(-pfpr) %>%
    insert_blank_rows_latex('setting')
  
  write.csv(dftbl5 %>%
              rename(`PfPR2-10` = pfpr,
                     Strategy = labels, 
                     `Cases averted\nper 1,000 pop` = CA_perpop,
                     `Severe cases averted\nper 1,000 pop` = SA_perpop,
                     `Cases averted\nper 1,000 doses` = CA_perdose,
                     `Severe cases averted\nper 1,000 doses` = SA_perdose,
                     `Cases averted\nper 1,000 FVC` = CA_perFVC,
                     `Severe cases averted\nper 1,000 FVC` = SA_perFVC), 'plots/outcomes_averted_combinedstrategies_perandseas_sorted.csv', row.names = FALSE)
  write.table(dftbl6, file = paste0('plots/outcomes_averted_combinedstrategies__perandseas_sorted_forlatex.csv'), 
              sep = ",", row.names = FALSE, col.names = FALSE) # Table S4

}
