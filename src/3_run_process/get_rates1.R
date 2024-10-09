# Helper function to get rates from model run 

get_rates1 <- function(df, time_div){
  # Get rates and join prevalence and doses
  rates <- get_rates(df %>% ungroup(),
                     time_divisor = time_div,
                     baseline_t = 0, # default
                     age_divisor = 365,
                     scaler = 0.215,
                     treatment_scaler = 0.5,
                     baseline_treatment = 0.45,
                     aggregate_age = FALSE)  |>
    mutate(time_div = time_div, 
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
  
  return(rates)
}