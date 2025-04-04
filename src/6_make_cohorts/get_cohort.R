#' function to get outcomes for a cohort for per year
#' 
#' @param df dataset - should be ageyr with draws 
#' @param time1 first year of vaccination (starts at year 1)  = cohort name
#' @param minage minimum age of the cohort at vaccination
#' @param maxage maximum age of the cohort at vaccination
#' @param strategy ??
get_cohort <- function(df, time1, minage, maxage, strategy = ''){
  
  cohort_cases_over_time <- NULL
  min_age <- minage
  max_age <- maxage
  
  for(i in time1:max(df$halfyear)){
  # i=1# for CU, only get cohort that is vaccinated at time 1 
    p <- df %>%
      filter(age_lower >= min_age & age_upper <= max_age &
             halfyear == i)
    
    cohort_cases_over_time <- bind_rows(cohort_cases_over_time, p)
    
    # Increment the starting year and age group for the next iteration 
    # (time is in integers but technically are half years (i.e. 1 and 2 are boht in year 1), and age is in half years)
    i <- i + 1
    min_age <- min_age + 0.5
    max_age <- max_age + 0.5
    
  }
  
  # cohort_cases_over_time$cohort_age <- paste(minage, maxage, sep = '-')
  cohort_cases_over_time$cohort_name <- time1
  
  #Get age of first vaccination 
  cohort_cases_over_time <- cohort_cases_over_time %>%
    mutate(ageatvax = ifelse(halfyear == 1, paste0(age_lower,'-', age_upper), 
                             paste0(age_lower - (halfyear*0.5-0.5),'-', age_upper - (halfyear*0.5-0.5)))) %>%
    mutate(ageatvax = ifelse(PEVstrategy == 'AB', '0.5-1', ifelse(PEVstrategy == 'none', 'no vax', ageatvax)))

  return(cohort_cases_over_time)
}
