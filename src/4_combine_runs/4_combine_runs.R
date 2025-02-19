# Task to combine processed runs 

# Set up task  ------------------------------------------------------------
library(dplyr)
library(orderly2)
library(data.table)
library(janitor)
library(purrr)
library(tidyr)

orderly_strict_mode()
orderly2::orderly_description('Combine processed malariasimulation runs by groups of 500')

# Set parameters for task 
orderly_parameters(analysis = NULL,
                   n500 = NULL,
                   to = NULL,
                   age_scaling = NULL)

start <- paste0(Sys.time(), ' start')
message(start)

orderly2::orderly_resource('completed_reports.R')
source('completed_reports.R')

# Set dependencies -- all of the processed runs, looped over the parameter list 
orderly2::orderly_dependency("1_create_parameter_list",
                             "latest(parameter:analysis == this:analysis)",
                             c(scenarios_torun_R21.rds = "scenarios_torun_R21.rds"))
pars <- readRDS('scenarios_torun_R21.rds')

# Get list of dfs taht have been completed recently 


# Initialize the dataframes
num_tostart = n500
num_toend = to

overallruns <- list()
last15runs <- list()
ageyrruns <- list()

for (i in num_tostart:num_toend){
  message(i)
  
  par <- pars[i,]

  message(i)

  drawID = par$drawID
  pfpr = par$pfpr
  seas_name = par$seas_name
  PEV = par$PEV
  PEVage = par$PEVage
  PEVstrategy = par$PEVstrategy
  PEVrounds = par$PEVrounds
  EPIbooster = par$EPIbooster
  EPIextra = par$EPIextra
  massbooster_rep = par$massbooster_rep
  MDA = par$MDA
  par_index = i

  orderly2::orderly_dependency("3_run_process",
                               "latest(parameter:analysis == this:analysis
                             && parameter:drawID == environment:drawID
                             && parameter:pfpr == environment:pfpr
                             && parameter:seas_name == environment:seas_name
                             && parameter:PEV == environment:PEV
                             && parameter:PEVage == environment:PEVage
                             && parameter:PEVstrategy == environment:PEVstrategy
                             && parameter:PEVrounds == environment:PEVrounds
                             && parameter:EPIbooster == environment:EPIbooster
                             && parameter:EPIextra == environment:EPIextra
                             && parameter:massbooster_rep == environment:massbooster_rep
                             && parameter:MDA == environment:MDA
                             && parameter:par_index == environment:par_index
                             && parameter:age_scaling == this:age_scaling)",
                             c("data/overall/processed_run_overall${par_index}.rds" = "processed_run_overall.rds",
                               "data/ageyr/processed_run_ageyr${par_index}.rds" = 'processed_run_ageyr.rds'))

  overallruns[[i]] <- readRDS(paste0('data/overall/processed_run_overall', par_index,'.rds'))
  ageyrruns[[i]] <- readRDS(paste0('data/ageyr/processed_run_ageyr', par_index,'.rds'))

  message(paste0(Sys.time(), ' got dependencies'))
# 
  if(PEVstrategy == 'none' | PEVstrategy == 'AB'){
    # last15runs[[i]] <- readRDS(paste0("R:/Kelly/catchupR21/archive/4_process_runs/", p$directory_name, '/processed_run_last15.rds'))
    orderly2::orderly_dependency("3_run_process","latest(parameter:analysis == this:analysis
                             && parameter:drawID == environment:drawID
                             && parameter:pfpr == environment:pfpr
                             && parameter:seas_name == environment:seas_name
                             && parameter:PEV == environment:PEV
                             && parameter:PEVage == environment:PEVage
                             && parameter:PEVstrategy == environment:PEVstrategy
                             && parameter:PEVrounds == environment:PEVrounds
                             && parameter:EPIbooster == environment:EPIbooster
                             && parameter:EPIextra == environment:EPIextra
                             && parameter:massbooster_rep == environment:massbooster_rep
                             && parameter:MDA == environment:MDA
                             && parameter:par_index == environment:par_index
                             && parameter:age_scaling == this:age_scaling)",
                                 c("data/last15/processed_run_last15${par_index}.rds" = 'processed_run_last15.rds'))
    last15runs[[i]] <- readRDS(paste0('data/last15/processed_run_last15', par_index,'.rds'))
    message(paste0(Sys.time(), ' got AB or none dependencies'))

  }
}

output_overall <- rbindlist(overallruns, use.names=TRUE)
output_ageyr <- rbindlist(ageyrruns, use.names=TRUE)
output_last15 <- rbindlist(last15runs, use.names=TRUE)

# Outputs for task 
orderly_artefact(
  'Produces three combined datasets (overall and ageyr and last 15 years) with 500 scenarios - depending on parameter n500, there will be 36 in total',
  c("summarized_draws_500.rds",
    "summarized_ageyr_draws_500.rds",
    "summarized_last15_draws_500.rds")
)

saveRDS(output_overall, 'summarized_draws_500.rds')
saveRDS(output_ageyr, 'summarized_ageyr_draws_500.rds')
saveRDS(output_last15, 'summarized_last15_draws_500.rds')

