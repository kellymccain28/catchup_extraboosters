# Calculations of deaths and DALYs
# Code from here: https://github.com/mrc-ide/malariavax2G/blob/main/02_code/Functions/deaths_dalys.R

# input: data frame
# process: reads in raw HPC output, adds RTS,S doses, condenses output over simulation length
# output: data frame with additional variables for deaths and DALYs


# DALYs = Years of life lost (YLL) + Years of life with disease (YLD)
# YLL = Deaths * remaining years of life
# YLD = cases and severe cases * disability weighting * episode_length
# CE = $ per event (case, death DALY) averted

# reference code: https://github.com/mrc-ide/gf/blob/69910e798a2ddce240c238d291bc36ea40661b90/R/epi.R
# weights from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4772264/ {Gunda _et al_, 2016}


# mortality --------------------------------------------------------------------

mortality_rate <- function(x,
                           scaler = 0.215,           # severe case to death scaler
                           treatment_scaler = 0.5) { # treatment modifier
  x |>
    
    # mortality rate
    # dplyr::mutate(mortality_rate = (1 - (treatment_scaler * .data$treatment)) * scaler * .data$sev) |>
    
    # mortality rate alternative (consistent with old ICL analysis)
    # this mortality rate is already included in postie processing functions so is not needed 
    # dplyr::mutate(mortality_rate = scaler * .data$severe) |> 
    
    dplyr::mutate(deaths = .data$mortality * .data$person_days)  # deaths
}

# DALYs ------------------------------------------------------------------------

daly_components <- function(x,
                            lifespan = 64.49,                   # average life expectancy
                            episode_length = 0.01375,        # average length of clinical episode
                            severe_episode_length = 0.04795, # average length of severe episode
                            weight1 = 0.211,      # disability weight age group 1
                            weight2 = 0.195,      # disability weight age group 2
                            weight3 = 0.172,      # disability weight age group 3
                            severe_weight = 0.6){ # disability weight severe malaria
  output <- x |>
    dplyr::mutate(yll = .data$deaths * (lifespan - ((.data$age_lower + .data$age_upper) / 2)),
                  # yll_lower = .data$deaths_lower * (lifespan - ((.data$age_lower + .data$age_upper) / 2)),
                  # yll_upper = .data$deaths_upper * (lifespan - ((.data$age_lower + .data$age_upper) / 2)),
                  
                  yll = ifelse(yll < 0, 0, yll),                    # should be no negative yll from older age groups
                  # yll_lower = ifelse(yll_lower < 0, 0, yll_lower),  # should be no negative yll from older age groups
                  # yll_upper =  ifelse(yll_upper < 0, 0, yll_upper), # should be no negative yll from older age groups
                  
                  yld = dplyr::case_when(.data$age_upper <= 5 ~ .data$cases * episode_length * weight1 + .data$sevcases * severe_episode_length * severe_weight,
                                         .data$age_upper > 5 & .data$age_upper <= 15 ~ .data$clinical * episode_length * weight2 + .data$sevcases * severe_episode_length * severe_weight,
                                         .data$age_upper > 15 ~ .data$cases * episode_length * weight3 + .data$sevcases * severe_episode_length * severe_weight),
                  
                  # yld_lower = dplyr::case_when(.data$age_upper <= 5 ~ .data$cases_lower * episode_length * weight1 + .data$sevcases * severe_episode_length * severe_weight,
                  #                              .data$age_upper > 5 & .data$age_upper <= 15 ~ .data$cases_lower * episode_length * weight2 + .data$sevcases * severe_episode_length * severe_weight,
                  #                              .data$age_upper > 15 ~ .data$cases_lower * episode_length * weight3 + .data$sevcases * severe_episode_length * severe_weight),
                  # 
                  # yld_upper = dplyr::case_when(.data$age_upper <= 5 ~ .data$cases_upper * episode_length * weight1 + .data$sevcases * severe_episode_length * severe_weight,
                  #                              .data$age_upper > 5 & .data$age_upper <= 15 ~ .data$cases_upper * episode_length * weight2 + .data$sevcases * severe_episode_length * severe_weight,
                  #                              .data$age_upper > 15 ~ .data$cases_upper * episode_length * weight3 + .data$sevcases * severe_episode_length * severe_weight)
    ) |>
    
    dplyr::mutate(daly = yll + yld
                  # daly_upper = yll_lower + yld_lower,
                  # daly_lower = yll_upper + yld_upper
    )
  return(output)
  
}