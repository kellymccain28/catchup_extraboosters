# Function to calculate prevalence per time period 

get_prev <- function(df, time_div){
  # Get prevalence by different age groups 
  prevalence <- get_prevalence(df,
                               time_divisor = time_div, 
                               baseline_t = 0,
                               age_divisor = 365) |>
    mutate(drawID = df$drawID[1],
           int_ID = df$int_ID[1]
    )
  
  return(prevalence)
}
