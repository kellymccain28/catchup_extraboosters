# Function to calculate prevalence per time period 

get_prev <- function(df, aggregation){#, time_div){
  # Get prevalence by different age groups 
  prevalence <- get_prevalence(df,
                               diagnostic = 'lm',
                               baseline_year = 1,
                               ages_as_years = TRUE) 
                               # time_divisor = time_div,
                               # baseline_t = 0,
                               # age_divisor = 365) |>
  
  
  # Get half years
  prevalence$halfyear <- ifelse(prevalence$month <=6, (prevalence$year - 0.5)*2, 
                                ifelse(prevalence$month > 6, (prevalence$year)*2, NA))
  
  # Make new column for overall simulation summarization 
  if(aggregation == 'overall' | aggregation == 'last15'){
    prevalence$halfyear = 1
  }
  
  prev_agg <- postie:::prevalence_aggregate(prevalence, 
                                            "halfyear") |>
    mutate(drawID = df$drawID[1],
           int_ID = df$int_ID[1]
    )
  
  return(prev_agg)
}
