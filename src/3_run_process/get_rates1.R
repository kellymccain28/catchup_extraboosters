# Helper function to get rates from model run 

get_rates1 <- function(df, aggregation){#, time_div){
  # Get rates and join prevalence and doses
  rates <- get_rates(df %>% ungroup(),
                     baseline_year = 1) 
  
  # Get half years
  rates$halfyear <- ifelse(rates$month <=6, (rates$year - 0.5)*2, 
                           ifelse(rates$month > 6, (rates$year)*2, NA))
  
  
  # Make new column for overall simulation summarization 
  if(aggregation == 'overall' | aggregation == 'last15'){
    rates$halfyear = 1
  }
  
  rates_agg <- postie:::rates_aggregate(rates,
                                        c("age_lower", "age_upper", "halfyear")) %>%
    mutate(#time_div = time_div, 
      ID = df$ID[1],
      sim_length = df$sim_length[1],
      scenario = df$scenario[1],
      drawID = df$drawID[1],
      EIR = df$EIR[1],
      pfpr = df$pfpr[1],
      seasonality = df$seasonality[1],
      PEV = df$PEV[1],
      PEVstrategy = df$PEVstrategy[1],
      PEVcov = df$PEVcov[1],
      PEVage = df$PEVage[1],
      PEVrounds = df$PEVrounds[1],
      EPIbooster = df$EPIbooster[1],
      EPIextra = df$EPIextra[1],
      massbooster_rep = df$massbooster_rep[1],
      MDA = df$MDA[1],
      int_ID = df$int_ID[1]
    )
  
  return(rates_agg)
}
