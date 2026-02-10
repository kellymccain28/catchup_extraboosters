# Task to process raw model runs (over simulation by age group + by year and age)

# Set up task  ------------------------------------------------------------
library(dplyr)
library(orderly)
library(data.table)
library(janitor)
library(purrr)
library(malariasimulation)
library(malariaEquilibrium)
library(postie)
library(tidyr)
library(retry)
library(zoo)
# library(beers) #devtools::install_github("mrc-ide/beers")

orderly_strict_mode()
orderly::orderly_description('Process raw malariasimulation runs overall and by half year/age')

# Set parameters for task 
orderlyparams <- orderly_parameters(analysis = NULL, # parameter to say it is a catch-up or mass vaccination parameter set (catch-up or mass)
                   drawID = NULL, 
                   pfpr = NULL,
                   seas_name = NULL,
                   PEV = NULL,
                   PEVstrategy = NULL, 
                   PEVage = NULL,
                   PEVrounds = NULL,
                   EPIbooster = NULL, 
                   EPIextra = NULL, 
                   massbooster_rep = NULL,
                   MDA = NULL,
                   par_index = NULL,
                   age_scaling = NULL)
analysis <- orderlyparams$analysis 
drawID <- orderlyparams$drawID
pfpr <- orderlyparams$pfpr
seas_name <- orderlyparams$seas_name
PEV <- orderlyparams$PEV
PEVstrategy <- orderlyparams$PEVstrategy
PEVage <- orderlyparams$PEVage
PEVrounds <- orderlyparams$PEVrounds
EPIbooster <- orderlyparams$EPIbooster
EPIextra <- orderlyparams$EPIextra
massbooster_rep <- orderlyparams$massbooster_rep
MDA <- orderlyparams$MDA
par_index <- orderlyparams$par_index
age_scaling <- orderlyparams$age_scaling

# Set dependencies
# orderly::orderly_dependency("1_create_parameter_list",
#                              "latest(parameter:analysis == this:analysis
#                              && parameter:age_scaling == this:age_scaling)",
#                              c(parameters_torun_R21.rds = "parameters_torun_R21.rds"))
param_df <- readRDS('R:/Kelly/catchup_extraboosters/archive/1_create_parameter_list/20260130-112115-4cdd8b1f/parameters_torun_R21.rds') # for 40% age scaling of over 5s
# readRDS('parameters_torun_R21.rds')

# Outputs for task 
orderly_artefact(
  description = 'Produces a raw model run then a processed run for each row in the parameter dataset - summarized overall by age or by age/year',
  files = c(#"raw_modelrun.rds",
    "processed_run_overall.rds",
    "processed_run_ageyr.rds")
)

if(PEVstrategy == 'none' | PEVstrategy == 'AB'){
  orderly_artefact(
    description = 'Produces a processed run for each row in the parameter dataset - summarized over last 15 years only if AB or no vaccine',
    files = c("processed_run_last15.rds")
  )
}

# Set resources
orderly_resource(
  c("EIRestimates.rds", ####### once I've set up the calibration task, this will no longer be a resource but will be a dependency 
    "run_simulation.R",
    "process_runs.R",
    "get_rates1.R",
    "get_prev.R",
    "get_doses.R",
    "add_doses.R")#,
    # "deaths_dalys.R")
)

# Functions to source
source("run_simulation.R")
source("process_runs.R")
source("get_rates1.R")
source("get_prev.R")
source("get_doses.R")
source("add_doses.R")
# source("deaths_dalys.R")


# Running the model for specified scenario
message('Starting to run model')
df <- runsim(par_index)
message('Finished running model')

# save output ----------
# message('save model run')
# saveRDS(df, "raw_modelrun.rds")


# Process runs overall
message('starting processing of raw model run')

process_runs(df,
             aggregation = 'overall',
             age_scaling = age_scaling)
message('processed overall')

# Process runs by age and year
process_runs(df,
             aggregation = 'ageyr',
             age_scaling = age_scaling)
message('processed age year')

# Process runs by last 15 years
if(PEVstrategy == 'none' | PEVstrategy == 'AB' | PEVstrategy == 'hybrid' | PEVstrategy == 'seasonal'){
  process_runs(df,
               aggregation = 'last15',
               age_scaling = age_scaling)
  message('processed last 15')
}

message('finished')
