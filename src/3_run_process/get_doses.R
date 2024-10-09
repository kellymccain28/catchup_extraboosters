# Function to get number of doses per level of aggregation 

#' @param df name of dataset
#' @param time_div time divisor - i.e. runs are monthly, so to get yearly aggregation, time_div = 12
get_doses <- function(df, time_div){
  
  df_agg <- df |>
    mutate(t = as.integer(ceiling(.data$timestep / time_div) + 0)) %>%
    group_by(t) %>%
    # sum the doses over time 
    summarise_at(vars(starts_with('dose'), starts_with('n_pev')), sum, na.rm = TRUE) %>% distinct()#
  
  return(df_agg)
}
