# pfpr and eir matching 

# calibration to average annual pfpr value for last 2 years of simulation 
annual_pfpr_summary <- function(x){
  # warmup <- 4380 
  # x <- x[x$timestep > warmup,]
  x$year <- ceiling(x$timestep / 365)
  x <- x[x$year == max(x$year) | x$year == max(x$year)-1,]
  pfpr <- x$n_detect_730_3650 / x$n_730_3650
  year <- x$year
  tapply(pfpr, year, mean)
}

# test summary function 
# x <- 25
# data <- readRDS(paste0(HPCpath, "03_output/baseline_parameters.rds"))[x,]
# params <- unlist(data$params, recursive = FALSE)
# params$timesteps <- 365*4#data$sim_length + data$warmup
# out <- run_simulation(params$timesteps, params)
# annual_pfpr_summary(out)

pr_match <- function(x, y){
  
  data <- baseline_parameters[x,]
  params <- unlist(data$params, recursive = FALSE)
  params$timesteps <- data$sim_length + data$warmup
  
  # defining target as pfpr value in last 2 years of simulation 
  target <- rep(data$pfpr, 2)
  
  set.seed(1234)
  out <- cali::calibrate(parameters = params,
                         target = target,
                         summary_function = annual_pfpr_summary,
                         tolerance = 0.001,
                         low = 0.1,
                         high = 1500)
  
  # store init_EIR results as an .rds file to be read in later
  PR <- data.frame(scenarioID = x,  drawID = y)
  PR$starting_EIR <- out
  PR$ID <- data$ID
  
  print(paste0('Finished scenario ',x))
  saveRDS(PR, paste0('PrEIR/PRmatch_draws_', data$ID, '.rds'))
}



