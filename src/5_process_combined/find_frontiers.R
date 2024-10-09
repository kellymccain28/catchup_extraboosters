# Function to find efficiency frontier values 

find_frontiers <- function(df){
  
  # Filter out hybrid and SV because are not going to actually compare them in the paper 
  df1 <- df %>%
    filter(PEVstrategy != 'SV' & PEVstrategy !='hybrid') %>%
    # filter(age_grp == '0-100') %>%
    # Filter to strategy type
    # filter(strategytype == 'Catch-up' | strategytype == 'Routine') %>%
    group_by(pfpr, seasonality, age_grp) %>%
    arrange(totaldoses, .by_group = TRUE) %>% 
    mutate(mincases = cummin(cases),
           minsev = cummin(sevcases),
           maxCA = cummax(cases_averted),
           maxSA = cummax(severe_averted)) %>% ungroup() %>%
    mutate(mincases = ifelse(cases == mincases, 1, 0),
           minsev = ifelse(sevcases == minsev, 1, 0),
           maxCA = ifelse(cases_averted == maxCA, 1, 0),
           maxSA = ifelse(severe_averted == maxSA, 1, 0)) %>%
    # Only want this variable for the 0-100 age group
    mutate(across(c(mincases, minsev, maxCA, maxSA), ~ ifelse(age_grp != '0-100', NA, .x)))
  
  # Get only SV and hybrid  
  df2 <- df %>%
    filter(PEVstrategy == 'SV' | PEVstrategy =='hybrid') %>%
    mutate(mincases = 0, 
           minsev = 0,
           maxCA = 0, 
           maxSA = 0)%>%
    # Only want this variable for the 0-100 age group
    mutate(across(c(mincases, minsev, maxCA, maxSA), ~ ifelse(age_grp != '0-100', NA, .x)))
       
  # add back in the SV and hybrid 
  df <- rbind(df1, df2)
  
  return(df)
}