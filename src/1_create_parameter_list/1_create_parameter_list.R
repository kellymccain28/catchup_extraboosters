# Task to create set of parameters for model runs (Catch up and age-based and no vaccination scenarios)

# Set up task  ------------------------------------------------------------
library(dplyr)
library(orderly2)
library(data.table)
library(janitor)
library(purrr)
library(tidyr)
library(malariasimulation)

orderly_strict_mode()

# Set parameters for task 
orderly_parameters(analysis = NULL,# either catch-up or mass
                   age_scaling = NULL) 

# OUtputs for task 
orderly_artefact(
  description = 'Files of scenarios and associated malariasimulation parameters as rds files',
  files = c(
    "scenarios_torun_R21.rds",
    "parameters_torun_R21.rds"
    )
)

# Set resources
orderly_resource(
  c(
    "ssa_demography_2021.csv",
    "efficacy_parameters.csv",
    "efficacy_parameters_medians.csv",
    "r21_malarisimulation_parameter_draws.csv",
    "r21_malarisimulation_parameters.csv",
    "generate_params.R"
  )
)

set.seed(1234)
source("generate_params.R")


# Task --------------------------------------------------------------------

## First, create scenario df ----------------------------------------------
# year
year <- 365

# population
population <- 300000 

# run time
warmup <- 15 * year        
sim_length <- 35*year  # now will be able to discard first 15 years after vaccination and then evaluate the impact at 15-30 years

# number of parameter draws
# 0 = use mean values, 1 to 50 = draws
drawID <- c(942,  40, 541, 497, 877, 697, 400, 450, 806, #0, 
            600, 670, 363, 838, 478, 403, 375, 335, 598, 142,
            919, 444, 986, 659,  71, 457, 891, 188, 432, 975, 
            488, 867, 538, 912, 534, 215, 540, 866, 613, 973, 
            917, 937, 931, 296, 835, 328, 147, 701, 889, 708, 888)# c(0, sample(1:1000, 50))#c(0, 1:50)

# SITE set-up ----
# parasite prevalence 2-10 year olds
pfpr <- c(0.01, 0.03, 0.05, 0.25, 0.45, 0.65)

# seasonal profiles: c(g0, g[1], g[2], g[3], h[1], h[2], h[3])
# drawn from mlgts: https://github.com/mrc-ide/mlgts/tree/master/data
# g0 = a0, a = g, b = h
seas_name <- 'highly seasonal'
seasonality <- list(c(0.284596,-0.317878,-0.0017527,0.116455,-0.331361,0.293128,-0.0617547))
s1 <- tibble(seasonality, seas_name)

seas_name <- 'seasonal'
seasonality <- list(c(0.285505,-0.325352,-0.0109352,0.0779865,-0.132815,0.104675,-0.013919))
s2 <- tibble(seasonality, seas_name)

seas_name <- 'perennial'
seasonality <- list(c(0.2852770,-0.0248801,-0.0529426,-0.0168910,-0.0216681,-0.0242904,-0.0073646))
s3 <- tibble(seasonality, seas_name)

stable <- bind_rows( s2, s3)#s1,

# vectors
# list(arab_params, fun_params, gamb_params)
speciesprop <- data.frame(speciesprop = rbind(list(c(0.25, 0.25, 0.5))),
                          row.names = NULL)

# INTERVENTIONS ----

# treatment coverage (baseline)
treatment <- c(0.45) 

# MDA
MDA <- c(0, 1)

# SMC: 0, 1
SMC <- c(0) 

# Vaccine 
PEV <- c('none','R21')

# PEV strategy
PEVstrategy <- c('none', 'AB', 'hybrid', 'SV','catch-up', 'mass')

# PEV coverage
PEVcov <- c(0, 0.8)

# PEV age group
PEVage <- c('-', '5-9', '5-14', '5-100', '6m-2y', '6m-4y', '6m-9y', '6m-14y')

# Rounds of PEV mass vaccination
PEVrounds <- c('-', 'single', '3yrs') 

# EPI booster timing 
EPIbooster <- c('12m', '12m boost', '18m', 'seasonal', '-')

# Extra boosters for EPI 
EPIextra <- c('5y', '10y', '5y+10y', '2y', '2y+5y','2y+10y','2y+5y+10y', '-')#, '2y')

# adding vaccine boosters:  - is only 4th dose,  annual (every year), 4 annual (every year for 4 years)
massbooster_rep <- c('-', '4 annual', 'annual')

interventions <- crossing(treatment, SMC, PEV, PEVstrategy, PEVcov, PEVage, PEVrounds, EPIbooster, EPIextra, massbooster_rep, MDA)

interventions <- interventions %>%
  filter(!(PEV == 'none' & PEVstrategy %in% c('AB', 'hybrid','catch-up', 'mass', 'SV'))) |>
  filter(!(PEV %in% c('R21') & PEVstrategy == 'none')) |> 
  filter(!(PEV == 'R21' & EPIbooster %in% c('12m boost', '18m'))) |> # only 12 month booster
  filter(!(PEVstrategy == 'none' & EPIbooster %in% c('12m', '12m boost', '18m', 'seasonal'))) |> # If no vaccination, then no EPI boosters
  filter(!(PEVstrategy %in% c('none', 'mass') & EPIextra %in% c('5y', '10y', '5y+10y', '2y', '2y+5y','2y+10y','2y+5y+10y'))) |> #no extra epi boosters if no vax or if only mass
  filter(!(PEVstrategy == 'none' & PEVrounds %in% c('3yrs','single'))) |> # no rounds of PEVs if no vaccination
  filter(!(PEVstrategy == 'none' & PEVage %in% c('5-14', '5-9', '5-100', '6m-4y','6m-9y', '6m-14y', '6m-2y'))) |> # if no vaccine, then age groups are irrelevant
  filter(!(PEVstrategy %in% c('AB','hybrid', 'SV') & PEVage %in% c('5-14', '5-9', '5-100', '6m-4y','6m-9y', '6m-14y', '6m-2y'))) |> # with hybrid or SV vax, no age groups specified
  filter(!(PEVstrategy == 'catch-up' & PEVage %in% c('-', '5-100'))) |> # catch-up vax to only 5-19 or 5-15, or 6m-5y, 6m-15y, 6m-9y
  filter(!(PEVstrategy == 'mass' & PEVage %in% c('5-14', '5-9', '-', '6m-4y','6m-9y', '6m-14y', '6m-2y'))) |> # mass vax only to 5-100
  filter(!(PEVstrategy %in% c('AB', 'hybrid', 'catch-up', 'SV') & PEVrounds %in% c('single','3yrs', '5yrs'))) |> # AB and hybrid would not have repeated mass rounds 
  filter(!(PEVstrategy == 'AB' & EPIbooster %in% c('seasonal','-'))) |> # no seasonal boosters for AB
  filter(!((PEV == 'R21' & PEVstrategy == 'catch-up') & EPIbooster %in% c('-', '12m boost', '18m','seasonal'))) |> # always has an EPI booster timing of 12 m boost
  filter(!(PEVstrategy == 'hybrid' & EPIbooster %in% c('12m', '12m boost', '18m','-'))) |> # hybrid vaccination will not have these boosters
  filter(!(PEVstrategy == 'mass' & EPIbooster %in% c('12m', '12m boost', '18m','seasonal'))) |> # mass vaccination doesn't have epi boosters
  filter(!(PEVstrategy == 'SV' & EPIbooster %in% c('12m', '12m boost', '18m','-'))) |> # seasonal vaccination will not have these boosters
  filter(!(PEVstrategy %in% c('hybrid','mass', 'SV','none') & EPIextra %in% c('5y','10y','5y+10y', '2y', '2y+5y','2y+10y','2y+5y+10y'))) |> # only catch-up vax and routine get extra boosters
  filter(!(PEVstrategy %in% c('AB', 'hybrid','catch-up', 'SV') & MDA == 1)) |># MDA is only to PEVstrategy == none and PEVstrategy ==mass 
  filter(!(PEVstrategy %in% c('AB', 'hybrid','catch-up', 'mass', 'SV') & PEVcov == 0)) |> # if vaccination, then coverage is not 0
  filter(!(PEVstrategy == 'none' & PEVcov == 0.8)) |>
  filter(!(PEVstrategy == 'mass' & PEVrounds == '-')) |> # this means the same thing as single 
  filter(!(PEVstrategy %in% c('none', 'catch-up', 'hybrid', 'AB', 'SV') & massbooster_rep %in% c('4 annual', 'annual'))) # only mass booster repetition in mass scenarios 

# create combination of all runs 
combo <- crossing(population, pfpr, stable, warmup, sim_length, speciesprop, interventions, drawID) |>
  mutate(ID = paste(pfpr, seas_name, "0", drawID, sep = "_")) 

intpre2y <- combo %>% 
  filter(!(EPIextra %in% c('2y', '2y+5y','2y+10y','2y+5y+10y')))
intpost2y <- combo %>% 
  filter(EPIextra == '2y')
intpost2ymore <- combo %>%
  filter( ((EPIextra == '2y+5y'| EPIextra == '2y+10y'| EPIextra == '2y+5y+10y') & PEVage != '-') |
            ((EPIextra == '2y+5y'| EPIextra == '2y+10y'| EPIextra == '2y+5y+10y') & PEVage == '-'))

combo <- rbind(intpre2y, intpost2y, intpost2ymore)


# remove non-applicable scenarios 
combo <- combo |>
  filter(!(seas_name == 'perennial' & PEVstrategy == 'hybrid')) |> # no hybrid vaccination in perennial settings 
  filter(!(seas_name == 'perennial' & PEVstrategy == 'SV'))  # no seasonal vaccination in perennial settings 

# put variables into the same order as function arguments
combo <- combo |> 
  select(population,        # simulation population
         seasonality,       # seasonal profile
         seas_name,         # name of seasonal profile
         pfpr,              # corresponding PfPR
         warmup,            # warm-up period
         sim_length,        # length of simulation run
         speciesprop,       # proportion of each vector species
         treatment,         # treatment coverage
         SMC,               # SMC coverage
         PEV,               # Which PEV
         PEVstrategy,       # PEV strategy
         PEVcov,            # PEV coverage
         PEVage,            # PEV age groups
         PEVrounds,         # PEV rounds of mass vax
         EPIbooster,        # Timing of EPI booster 
         EPIextra,          # Extra EPI boosters for catch-up campaigns 
         massbooster_rep,   # how many mass boosters
         MDA,               # MDA coverage
         ID,                # name of output file
         drawID             # parameter draw no.
  ) |> as.data.frame() 

# Filter based on analysis defined in orderly.R
if(analysis == 'catch-up'){
  combo <- combo |>
    filter(PEVstrategy %in% c('none','AB','SV','hybrid','catch-up')) |>
    filter(!(PEVstrategy == 'none' & MDA ==1 )) 
} else if (analysis == 'mass'){
  combo <- combo |>
    filter(PEVstrategy == 'mass' | PEVstrategy == 'none')
}

# Check that it looks right 
check <- combo %>% 
  group_by(PEV, PEVstrategy, PEVage, EPIextra, EPIbooster, massbooster_rep, PEVrounds, MDA) %>% 
  summarize(n = n())

# Save scenarios 
saveRDS(combo, 'scenarios_torun_R21.rds')

# Second, make and export parameter list --------------------------------------------------------------
generate_params('scenarios_torun_R21.rds', # file path to pull
                "parameters_torun_R21.rds",
                age_scaling)      # file path to push


###############################################
##  Make baseline parameter list ##############
###############################################
# Create baseline parameter set 
# year
year <- 365

# population
population <- 50000

# run time
warmup <- 12 * year       # needs to be multiple of 3 for ITN distribution
sim_length <- 12 * year   # value > 0 

# number of parameter draws
# 0 = use mean values, 1 to 50 = draws
drawID <- c(942,  40, 541, 497, 877, 697, 400, 450, 806, #0, 
            600, 670, 363, 838, 478, 403, 375, 335, 598, 142,
            919, 444, 986, 659,  71, 457, 891, 188, 432, 975, 
            488, 867, 538, 912, 534, 215, 540, 866, 613, 973, 
            917, 937, 931, 296, 835, 328, 147, 701, 889, 708, 888)# c(0, 1:50)

# SITE set-up ----
# parasite prevalence 2-10 year olds
pfpr <- c(0.01, 0.03, 0.05, 0.15, 0.25, 0.35, 0.45, 0.55, 0.65) # same profiles as Penny et al. 

# seasonal profiles: c(g0, g[1], g[2], g[3], h[1], h[2], h[3])
# drawn from mlgts: https://github.com/mrc-ide/mlgts/tree/master/data
# g0 = a0, a = g, b = h
seas_name <- 'highly seasonal'
seasonality <- list(c(0.284596,-0.317878,-0.0017527,0.116455,-0.331361,0.293128,-0.0617547))
s1 <- tibble(seasonality, seas_name)

seas_name <- 'seasonal'
seasonality <- list(c(0.285505,-0.325352,-0.0109352,0.0779865,-0.132815,0.104675,-0.013919))
s2 <- tibble(seasonality, seas_name)

seas_name <- 'perennial'
seasonality <- list(c(0.2852770,-0.0248801,-0.0529426,-0.0168910,-0.0216681,-0.0242904,-0.0073646))
s3 <- tibble(seasonality, seas_name)

stable <- bind_rows(s2, s3)#s1, 

# vectors
# list(arab_params, fun_params, gamb_params)
speciesprop <- data.frame(speciesprop = rbind(list(c(0.25, 0.25, 0.5))),
                          row.names = NULL)

# INTERVENTIONS ----
# treatment coverage
treatment <- c(0.45) 

# SMC: 0, 1
SMC <- c(0) 

# MDA y/n
MDA <- c(0)

# Which vaccine
PEV <- c('none') 

# Vaccination strategy
PEVstrategy <- c('none')

# Age group
PEVage <- c('-')

# PEV coverage
PEVcov <- c(0) 

# Rounds of PEV mass vaccination
PEVrounds <- c('-')

# EPI booster timing 
EPIbooster  <- c('-')  

# Extra boosters for EPI 
EPIextra <- c('-')

# adding vaccine boosters: 0 - no fifth dose, fifth (2 booster doses), annual (every year), 6mo (every 6 months), 2yrs (every 2 years)
massbooster_rep <- c('-')

interventions <- crossing(treatment, SMC, PEV, PEVstrategy, PEVcov, PEVage, PEVrounds, EPIbooster, EPIextra, massbooster_rep, MDA)

# create combination of all runs 
combo <- crossing(population, pfpr, stable, warmup, sim_length, speciesprop, interventions, drawID) |>
  mutate(ID = paste(pfpr, seas_name, "0", drawID, sep = "_")) #, RTSSage

# put variables into the same order as function arguments
combo <- combo |> 
  select(population,        # simulation population
         seasonality,       # seasonal profile
         seas_name,         # name of seasonal profile
         pfpr,              # corresponding PfPR
         warmup,            # warm-up period
         sim_length,        # length of simulation run
         speciesprop,       # proportion of each vector species
         treatment,         # treatment coverage
         SMC,               # SMC coverage
         PEV,               # Which PEV
         PEVstrategy,       # PEV strategy
         PEVcov,            # PEV coverage
         PEVage,            # PEV age groups
         PEVrounds,         # PEV rounds of mass vax
         EPIbooster,        # Timing of EPI booster 
         EPIextra,          # Extra EPI boosters for catch-up campaigns 
         massbooster_rep,   # how many mass boosters
         MDA,               # MDA coverage
         ID,                # name of output file
         drawID             # parameter draw no.
  ) |> as.data.frame()

# save baseline parameters 
saveRDS(combo, 'baseline_scenarios.rds')

generate_params('baseline_scenarios.rds', # file path to pull
                "baseline_parameters.rds", # file path to push 
                age_scaling)
