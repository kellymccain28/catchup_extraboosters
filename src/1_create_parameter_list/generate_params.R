# Generate parameters for malariasimulation ------------------------------------

generate_params <- function(inputpath,   # path to input scenarios
                            outputpath,  # path where output file will be stored
                            age_scaling
                            ){  # value for scaling of age-based efficacy (1 or 0.64)
  #setwd(path)
  # read in dataframe of all scenario combinations
  scenarios <- readRDS(inputpath)
  
  # generate parameters
  generate_params2 <- function(x){ # x = scenario number
    
    # pull one scenario at a time
    data <- scenarios[x, ]
    
    # assign values
    population = data$population
    seasonality = data$seasonality
    seas_name = data$seas_name
    pfpr = data$pfpr
    warmup = data$warmup
    sim_length = data$sim_length
    speciesprop = data$speciesprop
    treatment = data$treatment
    SMC = data$SMC
    PEV = data$PEV
    PEVstrategy = data$PEVstrategy
    PEVcov = data$PEVcov
    PEVage = data$PEVage
    PEVrounds = data$PEVrounds
    EPIbooster = data$EPIbooster
    EPIextra = data$EPIextra
    massbooster_rep = data$massbooster_rep
    MDA = data$MDA
    ID = data$ID
    drawID = data$drawID
    
    year <- 365
    month <- year / 12
    
    # starting parameters ----------
    params <- get_parameters(list(
      human_population = population,
      model_seasonality = TRUE,
      # rainfall fourier parameters
      g0 = unlist(seasonality)[1],
      g = unlist(seasonality)[2:4],
      h = unlist(seasonality)[5:7],
      individual_mosquitoes = FALSE))
    
    # parameter draws  ----------
    # choose a parameter draw
    if(drawID > 0){
      params <- set_parameter_draw(params, drawID)
    }
    
    # outcome definitions ----------
    render_min_ages = c(c(0, 0.5, seq(1, 46.5, by = 0.5))*year, 47 * year, 0, 0, 5*year, 10*year)#seq(50, 95, by = 5)*year, 
    render_max_ages = c(c(0.5, seq(1, 47, by = 0.5))*year, 100*year, 100 * year, 5*year, 10*year, 15*year) #seq(55, 100, by = 5)*year,
    
    # Set clinical incidence rendering 
    params$clinical_incidence_rendering_min_ages = render_min_ages
    params$clinical_incidence_rendering_max_ages = render_max_ages
    
    # Set severe incidence rendering 
    params$severe_incidence_rendering_min_ages = render_min_ages
    params$severe_incidence_rendering_max_ages = render_max_ages
    
    # Set age group rendering 
    params$age_group_rendering_min_ages = render_min_ages
    params$age_group_rendering_max_ages = render_max_ages
    
    # prevalence 2-10 year olds
    params$prevalence_rendering_min_ages = c(2 * year, 0 * year)
    params$prevalence_rendering_max_ages = c(10 * year, 100 * year)
    
    # demography ----------
    # flat_demog <- read.csv(paste0(path,'/01_data/ssa_demography_2021.csv')) # from mlgts
    # ages <- round(flat_demog$V3 * year) # top of age bracket
    # deathrates <- flat_demog$V5 / 365   # age-specific death rates
    # params <- set_demography(
    #   params,
    #   agegroups = ages,
    #   timesteps = 0,
    #   deathrates = matrix(deathrates, nrow = 1)
    # )
    rescale_prob <- function(p, interval_in, interval_out){
      1 - (1 - p) ^ (interval_out / interval_in)
    }
    
    demog <- read.csv('ssa_demography_2021.csv')
    # Age group upper
    ages <- round(demog$age_upper * 365)
    # Rescale the deathrates to be on the daily timestep
    deathrates <- rescale_prob(demog$mortality_rate, 365, 1)
    # Create matrix of death rates
    deathrates_matrix <- matrix(deathrates, nrow = length(1), byrow = TRUE)
    
    params <- set_demography(
      parameters = params,
      agegroups = ages,
      timesteps = 0,
      deathrates = deathrates_matrix
    )
    
    # vectors ----------
    params <- set_species(
      parameters = params,
      species = list(arab_params, fun_params, gamb_params),
      proportions = unlist(speciesprop))

    # proportion of bites taken in bed for each species
    # find values in S.I. of 10.1038/s41467-018-07357-w Table 3
    params$phi_bednets <- c(0.9, 0.9, 0.89) # Hogan et al. 2020
    # proportion of bites taken indoors for each species
    params$phi_indoors <- c(0.96, 0.98, 0.97) # Hogan et al. 2020
    
    
    # # treatment ----------
    if (treatment > 0) {
      params <- set_drugs(
        parameters = params,
        drugs = list(AL_params, SP_AQ_params))
      
      # AL default, SP: https://doi.org/10.1016/S2214-109X(22)00416-8 supplement
      params$drug_prophylaxis_scale <- c(10.6, 39.34)
      params$drug_prophylaxis_shape <- c(11.3, 3.40)
      
      params <- set_clinical_treatment(
        parameters = params,
        drug = 1,
        timesteps = c(1),
        coverages = c(treatment)
      )  
    }
    
    # Add in vaccine to match random numbers 
    params$pev <- TRUE
    params$pev_epi_coverages <- 0
    params$pev_epi_booster_coverage <- 0
    params$mass_pev_coverages <- 0
    params$mass_pev_booster_coverage <- 0
    
    # Vaccination with a PEV ----
    if (PEV == 'R21'){
    # here, should add if ages_scaling is true or false and what that would mean
      # R21 ----------
      # median of these parameters is variable -- chose to take median from Table 1 in R21 paper https://ssrn.com/abstract=4597985
      # r21_efficacy <- read.csv('efficacy_parameters.csv') %>%
      #   summarise(v_max = median(v_max),
      #             alpha = median(alpha),
      #             beta = median(beta))
      r21_params <- read.csv('r21_malarisimulation_parameters.csv') %>% #Best fit parameter values for antibody parameters
        # grouping and summarizing for when we used the _draws dataset
        # group_by(par) %>%
        # summarize(median_mu = median(mu),
        #           median_sd = median(sd)) %>% 
        data.table::transpose() %>% 
        row_to_names(row_number = 1)
      
      if (PEVcov >0){
          # r21_profile <- rtss_profile
          # these parameters are from median value in Table 1 of R21 paper as explained above
          # Fitted parameters
          # r21_profile$vmax <- .87 #r21_efficacy$v_max
          # r21_profile$alpha <- 0.91 #r21_efficacy$alpha
          # r21_profile$beta <- 471 #r21_efficacy$beta
          # r21_profile$cs <- as.numeric(r21_params$r21_cs) #c(9.32, 0.839)
          # r21_profile$rho <- as.numeric(r21_params$r21_rho) #c(0.791, 0.607)
          # r21_profile$ds <- as.numeric(r21_params$r21_ds) #c(3.79, 0.165)
          # r21_profile$dl <- as.numeric(r21_params$r21_dl) #c(6.28, 0.433)
          # 
          # r21_booster_profile <- r21_profile
          # r21_booster_profile$rho <- as.numeric(r21_params$r21_rho_boost) 
          # r21_booster_profile$cs <- as.numeric(r21_params$r21_cs_boost) 
          
          # Make profile for second booster dose
          r21_booster_profile2 <- create_pev_profile(
            vmax = r21_booster_profile$vmax,
            alpha = r21_booster_profile$alpha,
            beta = r21_booster_profile$beta,
            cs = as.numeric(r21_params$r21_cs_boost2),
            rho = r21_booster_profile$rho,
            ds = r21_booster_profile$ds,
            dl = r21_booster_profile$dl
          )
          # r21_booster_profile2 <- r21_booster_profile
          # r21_booster_profile2$cs <- as.numeric(r21_params$r21_cs_boost2) 
          
          # Scaled vaccine efficacy for older children (0.64 * cs) according to Bojang et al 2005
          r21_older_profile <- create_pev_profile(
            vmax = r21_profile$vmax,
            alpha = r21_profile$alpha,
            beta = r21_profile$beta,
            cs = log(exp(r21_profile$cs) * 0.64),
            rho = r21_profile$rho,
            ds = r21_profile$ds,
            dl = r21_profile$dl
          )
          # r21_older_profile <- r21_profile 
          # r21_older_profile$cs <- log(exp(r21_older_profile$cs) * 0.64)
          
          r21_older_booster_profile <- create_pev_profile(
            vmax = r21_booster_profile$vmax,
            alpha = r21_booster_profile$alpha,
            beta = r21_booster_profile$beta,
            cs = log(exp(r21_booster_profile$cs) * 0.64),
            rho = r21_booster_profile$rho,
            ds = r21_booster_profile$ds,
            dl = r21_booster_profile$dl
          )
          # r21_older_booster_profile <- r21_booster_profile
          # r21_older_booster_profile$cs <- log(exp(r21_older_booster_profile$cs) * 0.64)
          
          r21_older_booster_profile2 <- create_pev_profile(
            vmax = r21_booster_profile2$vmax,
            alpha = r21_booster_profile2$alpha,
            beta = r21_booster_profile2$beta,
            cs = log(exp(r21_booster_profile2$cs) * 0.64),
            rho = r21_booster_profile2$rho,
            ds = r21_booster_profile2$ds,
            dl = r21_booster_profile2$dl
          )
          # r21_older_booster_profile2 <- r21_booster_profile2
          # r21_older_booster_profile2$cs <- log(exp(r21_older_booster_profile2$cs) * 0.64)
          
          program_start <- 5 * year
          
          if (PEVage == '5-9'){
            min_ages = 5 * year
            max_ages = 10 * year
          } else if (PEVage == '5-14'){
            min_ages = 5 * year
            max_ages = 15 * year
          # } else if (PEVage == '5-100'){ 
          #   min_ages = 5 * year
          #   max_ages = 100 * year
          } else if (PEVage == '6m-2y'){  # all under 3s
            min_ages = round(6 * month)
            max_ages = 3 * year
          } else if (PEVage == '6m-4y'){ # all under 5s
            min_ages = round(6 * month)
            max_ages = 5 * year
          } else if (PEVage == '6m-9y'){ # aka under 10s
            # two different min and max ages because of different efficacy profiles for each age 
            min_ages = round(6 * month)
            max_ages = 10 * year
            # min_ages_y = round(6 * month)
            # max_ages_y = 5 * year
            # min_ages_o = 5 * year + 1
            # max_ages_o = 10 * year
          } else if (PEVage == '6m-14y'){ #under 15s
            min_ages = round(6 * month)
            max_ages = 15 * year
            # min_ages_y = round(6 * month)
            # max_ages_y = 5 * year
            # min_ages_o = 5 * year + 1
            # max_ages_o = 15 * year
          } 
          
          # AB
          if (PEVstrategy == "AB" & EPIextra == '-') {
            params$pev_doses <- round(c(0, 1 * month, 2 * month)) # monthly spacing from phase III R21 trial
            
            epiboosters <- round(12 * month)
            
            boost_cov <- matrix(0.8)
            
            pevtimesteps <- warmup + program_start # starting 5 years after warmup ends
            
            params <- set_pev_epi(
              parameters = params,
              profile = r21_profile,
              timesteps = pevtimesteps,
              coverages = PEVcov,
              age = round(6 * month),
              min_wait = 0,
              booster_spacing = epiboosters,
              booster_coverage = boost_cov,
              booster_profile = list(r21_booster_profile),
              seasonal_boosters = FALSE
            )
            
            print(paste0('EPI timesteps: ', pev_epi_timesteps <- params$pev_epi_timesteps - warmup))
            print(paste0('EPI booster: ', params$pev_epi_booster_timestep))
            
          }
          
          if (PEVstrategy == "AB" & EPIextra != '-') {
            params$pev_doses <- round(c(0, 1 * month, 2 * month)) # monthly spacing from phase III R21 trial
            
            if (EPIextra == '5y') {
              epiboosters <- round(c(12 * month, 5 * year))
            } else if (EPIextra == '10y') {
              epiboosters <- round(c(12 * month, 10 * year))
            } else if (EPIextra == '-') {
              epiboosters <- round(12 * month)
            } else if (EPIextra == '5y+10y'){
              epiboosters <- round(c(12 * month, 5 * year, 10 * year))
            } else if (EPIextra == '2y'){
              epiboosters <- round(c(12 * month, 2 * year))
            } else if (EPIextra == '2y+5y+10y'){
              epiboosters <- round(c(12 * month, 2 * year, 5 * year, 10 * year))
            } else if(EPIextra == '2y+5y'){
              epiboosters <- round(c(12 * month, 2 * year, 5 * year))
            } else if(EPIextra == '2y+10y'){
              epiboosters <- round(c(12 * month, 2 * year, 10 * year))
            }
            
            epiboosterprofiles <- if (length(epiboosters) == 1){ 
              list(r21_booster_profile) 
            } else if (length(epiboosters) == 2){
              list(r21_booster_profile, r21_booster_profile2)
            } else if (length(epiboosters) == 3){
              list(r21_booster_profile, r21_booster_profile2, r21_booster_profile2)
            } else if (length(epiboosters) == 4){
              list(r21_booster_profile, r21_booster_profile2, r21_booster_profile2, r21_booster_profile2)
            }
            
            pevtimesteps <- warmup + program_start # starting 5 years after warmup ends
            
            epiboost_cov <- matrix(0.8, rep(1, length(epiboosters)-1), nrow = length(pevtimesteps), ncol = length(epiboosters))
            
            params <- set_pev_epi(
              parameters = params,
              profile = r21_profile,
              timesteps = pevtimesteps,
              coverages = PEVcov,
              age = round(6 * month),
              min_wait = 0,
              booster_spacing = epiboosters,
              booster_coverage = epiboost_cov,
              booster_profile = epiboosterprofiles,
              seasonal_boosters = FALSE
            )
            
            print(paste0('EPI timesteps: ', pev_epi_timesteps <- params$pev_epi_timesteps - warmup))
            print(paste0('EPI booster: ', params$pev_epi_booster_timestep))
          }
          
          # hybrid ----------
          if (PEVstrategy == "hybrid") {
            params$pev_doses <- round(c(0, 1 * month, 2 * month)) # spacing from phase iii trial R21
            
            # peak <- peak_season_offset(params)
            peak <- 304  # see note in CU section
            
            hybridbooster <- round(c(peak - 3.5 * month), 0)
            
            boost_cov <- matrix(0.8)  # coverage from 10.1016/S2214-109X(22)00416-8 
            pevtimesteps <- warmup + program_start # starting 5 years after warmup ends
            
            params <- set_pev_epi(
              parameters = params,
              profile = r21_profile,
              timesteps = pevtimesteps,
              coverages = PEVcov,
              age = round(6 * month),
              min_wait = round(6 * month),
              booster_spacing = hybridbooster,
              booster_coverage = boost_cov,
              booster_profile = list(r21_booster_profile),
              seasonal_boosters = TRUE
            )
            
            print(paste0('EPI timesteps: ', pev_epi_timesteps <- params$pev_epi_timesteps - warmup))
            print(paste0('EPI booster: ', params$pev_epi_booster_timestep))
            print('hybrid parameterized)')
          }
          
          # SV ----
          if (PEVstrategy == "SV") {
            # params$pev_doses <- round(c(0, 1.5 * month, 3 * month)) # spacing from the RTSS work 
            params$pev_doses <- round(c(0, 1 * month, 2 * month)) # spacing from phase iii trial R21
            
            # peak <- peak_season_offset(params)
            peak <- 304 # see note in CU section
            
            first <- round(warmup + program_start + (peak - month * 5.5), 0)
            SVtimesteps <- c(first, first + seq(year, sim_length, by = 365)) 
            
            SVbooster <- round(12 * month, 0)
            
            boost_cov <- matrix(0.8, nrow = length(SVtimesteps))  # coverage from 10.1016/S2214-109X(22)00416-8 
            
            params <- set_mass_pev(
              parameters = params,
              profile = r21_profile,
              timesteps = SVtimesteps, 
              coverages = rep(PEVcov, length(SVtimesteps)), 
              min_ages = round(6 * month),
              max_ages = round(17 * month),
              min_wait = 0,
              booster_spacing = SVbooster, # timesteps following initial vaccination 
              booster_profile = list(r21_booster_profile),
              booster_coverage = boost_cov # prop of vaccinated pop who will receive booster vaccine
              ) 
          }
          
          # mass ----------
          if (PEVstrategy == 'mass'){
            min_wait <- 20 * year
            
            params$pev_doses <- round(c(0, 1 * month, 2 * month)) # monthly spacing from phase III R21 trial
            
            # First set the EPI strategy
            EPIboosters <- round(c(12 * month))  # from phase III R21 trial
            pevtimesteps <- warmup + program_start # starting when warmup ends + program start
            EPIboost_cov <- matrix(0.8)
            
            params <- set_pev_epi(
              parameters = params,
              profile = r21_profile,
              timesteps = pevtimesteps,
              coverages = PEVcov,
              age = round(6 * month),
              min_wait = min_wait,
              booster_spacing = EPIboosters,
              booster_coverage = EPIboost_cov,
              booster_profile = list(r21_booster_profile),
              seasonal_boosters = FALSE)
            
            # Get timing for mass vaccination rounds
            peak <- peak_season_offset(params)
            
            if (massbooster_rep == '-') {
              massboosters <- round(12 * month) # 1 year after 3rd dose 
            } else if (massbooster_rep == '4 annual') {
              massboosters <- round(seq(12, 48, by = 12) * month) # 4 annual boosters after 3rd dose
            } else if (massbooster_rep == 'annual'){
              massboosters <- round(seq(12, (sim_length - program_start)/month, by = 12) * month)
            }
            
            massbooster_cov <- matrix(0.8, nrow = length(pevtimesteps), ncol = length(massboosters))# coverage from 10.1016/S2214-109X(22)00416-8
            
            if (length(massboosters) == 1){ 
              massboosterprofiles <- list(r21_booster_profile) 
            } else if (length(massboosters) >=4){
              massboosterprofiles <- append(replicate(length(massboosters)-1, r21_booster_profile2, simplify = FALSE), list(r21_booster_profile), after = 0)
            } 
            
            if(seas_name == 'seasonal'){
              first <- round(warmup + program_start + (peak - month * 3.5), 0) # after program start, 3.5 months prior to peak for seasonal (R21-CE repo)
            } else if(seas_name == 'perennial'){
              first <- round(warmup + program_start) # non seasonally distributed mass campaign
            }
            
            if (PEVrounds == 'single'){
              pevtimesteps <- c(first)
            } else if (PEVrounds == '1yr'){
              pevtimesteps <- c(first, first + seq(1 * year, sim_length, 1 * year))
            } else if (PEVrounds == '3yrs'){
              pevtimesteps <- c(first, first + seq(3 * year, sim_length, 3 * year))
            } else if (PEVrounds == '5yrs'){
              pevtimesteps <- c(first, first + seq(5 * year, sim_length, 5 * year))
            }
            
            # Set mass vaccination
            proppop_notpregnant <- 1 - 0.078/2 # from DHS data - see Get_pregnancy_rate.R
            
            # add mass vaccination taking into account pregnant women
            params <- set_mass_pev(
              parameters = params,
              profile = r21_profile,
              timesteps = pevtimesteps,
              coverages = matrix(PEVcov * proppop_notpregnant, length(pevtimesteps)),
              min_ages = min_ages,
              max_ages = max_ages,
              min_wait = min_wait,
              booster_spacing = massboosters, # timesteps following initial vaccination
              booster_profile = massboosterprofiles,#list(r21_booster_profile, rep(r21_booster_profile2, length(massboosters) -1)),
              booster_coverage = massbooster_cov # prop of vaccinated pop who will receive booster vaccine
            )
            
            # var for outputting to check RTS,S timings are correct
            print(paste0("Mass timesteps: ", mass_pev_timesteps <- params$mass_pev_timesteps - warmup))
            print(paste0('EPI timesteps: ', pev_epi_timesteps <- params$pev_epi_timesteps - warmup))
            print(paste0('EPI booster: ', params$pev_epi_booster_timestep))
            print(paste0('Mass booster: ', params$mass_pev_booster_timestep))
          }
          
          # catchup ----------
          if (PEVstrategy == 'catch-up'){
            
            pevtimesteps <- warmup + program_start # starting 5 years after warmup ends
            
            if(seas_name == 'seasonal'){
              # peak <- peak_season_offset(params) # this does not work because it is based on rainfall & epi params are lagged
              peak <- 304 # calculated using baseline value of inci 0-100 (0.5_get_peak_incidence/workflow.R) -- so campaign will be slightly after age-based vaccination
                
              first <- round(warmup + program_start + (peak - month * 5.5), 0)
              CUtimesteps <- first 
            } else if (seas_name == 'perennial'){
              CUtimesteps <- pevtimesteps
            }
            
            # Get timing for EPI boosters in catch-up campaigns
            params$pev_doses <- round(c(0, 1 * month, 2 * month)) # monthly spacing from phase III R21 trial
            
            if (EPIextra == '5y') {
              epiboosters <- round(c(12 * month, 5 * year))
            } else if (EPIextra == '10y') {
              epiboosters <- round(c(12 * month, 10 * year))
            } else if (EPIextra == '-') {
              epiboosters <- round(12 * month)
            } else if (EPIextra == '5y+10y'){
              epiboosters <- round(c(12 * month, 5 * year, 10 * year))
            } else if (EPIextra == '2y'){
              epiboosters <- round(c(12 * month, 2 * year))
            } else if (EPIextra == '2y+5y+10y'){
              epiboosters <- round(c(12 * month, 2 * year, 5 * year, 10 * year))
            } else if(EPIextra == '2y+5y'){
              epiboosters <- round(c(12 * month, 2 * year, 5 * year))
            } else if(EPIextra == '2y+10y'){
              epiboosters <- round(c(12 * month, 2 * year, 10 * year))
            }
            
            epiboost_cov <- matrix(0.8, nrow = length(pevtimesteps), ncol = length(epiboosters))
            
            massboosters <- round(c(1 * year)) # 1 year after 3rd dose
            
            massboost_cov <- matrix(0.8)
            
            epiboosterprofiles <- if (length(epiboosters) == 1){ 
              list(r21_booster_profile) 
            } else if (length(epiboosters) == 2){
              list(r21_booster_profile, r21_booster_profile2)
            } else if (length(epiboosters) == 3){
              list(r21_booster_profile, r21_booster_profile2, r21_booster_profile2)
            } else if (length(epiboosters) == 4){
              list(r21_booster_profile, r21_booster_profile2, r21_booster_profile2, r21_booster_profile2)
            }
            
            # Set EPI strategy for young children
            params <- set_pev_epi(
              parameters = params,
              profile = r21_profile,
              timesteps = pevtimesteps,
              coverages = PEVcov,
              age = round(6 * month),
              min_wait = 20 * year,
              booster_spacing = epiboosters,
              booster_coverage = epiboost_cov,
              booster_profile = epiboosterprofiles, # first booster is one thing, then any others are different
              seasonal_boosters = FALSE
            )
            
            # Set catch-up mass campaigns
            params <- set_mass_pev(
              parameters = params,
              profile = r21_profile,
              timesteps = CUtimesteps+1,
              coverages = PEVcov,
              min_ages = min_ages,
              max_ages = max_ages,
              min_wait = 20 * year,
              booster_spacing = massboosters,
              booster_coverage = massboost_cov,
              booster_profile = list(r21_booster_profile)
            )
            
            print(paste0("Mass timesteps: ", mass_pev_timesteps <- params$mass_pev_timesteps - warmup))
            print(paste0('EPI timesteps: ', pev_epi_timesteps <- params$pev_epi_timesteps - warmup))
            print(paste0('EPI booster: ', params$pev_epi_booster_timestep))
            print(paste0('Mass booster: ', params$mass_pev_booster_timestep))
          }
      }
    }
    
    # MDA  ----------
    if (MDA > 0) { # this will only run if there is MDA, which will only be the case if there is mass vaccination 
      MDAcov <- 0.8
      program_start <- 5 * year
      
      if(PEV !='none'){
        mdatimesteps <- pevtimesteps # round of MDA occurs at same time as first dose
      } else if(PEV == 'none'){
        mdatimesteps <- warmup + program_start
      }
      
      params <- set_drugs(
        parameters = params, 
        list(AL_params, SP_AQ_params, DHA_PQP_params)
      )
      
      proppop_notpregnant <- 1 - 0.078/2 # from DHS data - see Get_pregnancy_rate.R
      
      # https://www.who.int/publications/i/item/9789241513104 - for drug, min_ages, and coverage (pregnancy)
      params <- set_mda(
        parameters = params,
        drug = 1, # AL for MDA 
        timesteps = mdatimesteps,
        coverages = rep(MDAcov * proppop_notpregnant, length(mdatimesteps)), # excluding pregnant women
        min_ages = rep(6 * month, length(mdatimesteps)), # starting from 6 months of age 
        max_ages = rep(100 * 365, length(mdatimesteps))
      ) 
    }
    
    # synergy SMC & RTS,S ----------
    # if (SMC > 0 & RTSS %in% c("EPI", "mass", "hybrid")) {
    #   
    #   params$rtss_beta <- 70.9
    #   params$rtss_alpha <- 0.868
    #   params$rtss_vmax <- 0.843
    #   params$rtss_cs_boost <- c(6.37008, 0.35)
    #   
    #   params$drug_prophylaxis_scale <- c(10.6, 45.76)
    #   params$drug_prophylaxis_shape <- c(11.3, 2.87)
    #   
    # }
  

    # correlate interventions  ----------
    # correlations <- get_correlation_parameters(params)
    
    # if (RTSScov == 0.77 & pfpr == 0.40) {
    #   correlations$inter_intervention_rho('rtss', 'bednets', 0.04)
    # }
    #
    # if (RTSScov == 0.72 & pfpr == 0.18) {
    #   correlations$inter_intervention_rho('rtss', 'bednets', 0.07)
    # }
    
    # save as data.frame
    data$params <- list(params)
    data$scenarioID <- x
    data$age_scaling <- age_scaling
    
    # print count
    print(paste(x,'pfpr=',data$pfpr))
    
    return(data)
}
  
  # loop through function to generate parameters one by one
  output <- map_dfr(1:nrow(scenarios), generate_params2)
  
  # save output ----------
  saveRDS(output, outputpath)
  
}
