# Function to run simulation 
runsim <- function(x){
  year <- 365
  month <- year / 12
  
  # read in selected scenario
  data <- param_df[x,]
  
  # read in collated EIR estimates  --- this is from task 2 where we match EIR and PfPR, this allows skipping task 2 
  match <- readRDS("EIRestimates.rds") |> select(-scenarioID)
  
  # EIR / prev match from "PfPR_EIR_match.R"
  data <- data |> left_join(match, by = c("drawID", "ID"))
  
  # Set EIR equilibrium ----------
  params <- set_equilibrium(unlist(data$params, recursive = F), 
                            as.numeric(data$starting_EIR))
  
  # run simulation ----------
  set.seed(123)
  
  output <- run_simulation(
    timesteps = data$warmup + data$sim_length,
    # correlations = correlations,
    parameters = params) |>
    # add vars to output
    mutate(ID = data$ID,
           scenario = data$scenarioID,
           drawID = data$drawID,
           EIR = data$starting_EIR,
           warmup = data$warmup,
           sim_length = data$sim_length,
           population = data$population,
           pfpr = data$pfpr,
           timestep = timestep - data$warmup,
           seasonality = data$seas_name,
           speciesprop = paste(data$speciesprop, sep = ",", collapse = ""),
           treatment = data$treatment,
           SMC = data$SMC,
           PEV = data$PEV,
           PEVstrategy = data$PEVstrategy,
           PEVcov = data$PEVcov,
           PEVage = data$PEVage,
           PEVrounds = data$PEVrounds,
           EPIbooster = data$EPIbooster,
           EPIextra = data$EPIextra,
           massbooster_rep = data$massbooster_rep,
           MDA = data$MDA) |>
    ungroup() |>
    
    filter(timestep > 0) |> # remove warmup period
    
    mutate(year = ceiling(timestep/365),
           month = ceiling(timestep/(365/12))) |>
    
    # keep only necessary variables
    dplyr::select(ID, scenario, drawID, EIR, warmup, sim_length, population, pfpr, month, year, seasonality, speciesprop,
                  treatment, SMC, PEV, PEVstrategy, PEVcov, PEVage, PEVrounds, EPIbooster, EPIextra, massbooster_rep, MDA, 
                  infectivity,
                  timestep, ft,
                  starts_with("n_pev"),
                  starts_with("n_inc"), -starts_with("p_inc"),
                  starts_with("n_inc_severe"), -starts_with("p_inc_severe"),
                  starts_with("n_detect"), -starts_with("p_detect"),
                  starts_with("n_"), -starts_with("n_age"), 
                  -n_bitten, n_treated, n_infections) |>
    
    # add in an intervention ID
    dplyr::mutate(int_ID = paste(pfpr, seasonality, PEV, PEVstrategy, PEVage, PEVrounds, EPIbooster, EPIextra, massbooster_rep, MDA, sep = '_')) |>
    
    # group by month and scenarios 
    group_by(ID, scenario, drawID, EIR, warmup, sim_length, population, pfpr, month, year, seasonality, speciesprop,
             treatment, SMC, PEV, PEVstrategy, PEVcov, PEVage, PEVrounds, EPIbooster, EPIextra, massbooster_rep, MDA) |>
    
    # get mean of population and doses by month
    mutate_at(vars(n_0_182.5:n_3650_5475, 
                   n_730_3650, n_0_36500,
                   n_detect_lm_730_3650, n_detect_lm_0_36500,
                   n_detect_pcr_730_3650, n_detect_pcr_0_36500,
                   infectivity), mean, na.rm = TRUE) |>
    
    # get sums of cases by month
    mutate_at(vars(starts_with('n_pev'), 
                   n_inc_clinical_0_182.5:n_inc_severe_3650_5475,
                   n_treated, n_infections), sum, na.rm = TRUE) |> 
    select(-c(timestep)) |> # removing timestep that is per day 
    distinct() |>
    mutate(timestep = month)  # this is so that postie can use the timestep variable - 
                              # need to adjust processing functions to account for timestep not being a day 
  
  return(output)
}

