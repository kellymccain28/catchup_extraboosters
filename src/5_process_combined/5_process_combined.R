# Task to combine the batched combination and then process them (get outcomes averted and summarize by scenario) 5.5

# Set up task  ------------------------------------------------------------
library(dplyr)
library(orderly2)
library(data.table)
library(janitor)
library(purrr)
library(tidyr)
library(stringr)

orderly_strict_mode()
orderly2::orderly_description('Combine processed malariasimulation runs by groups of 500')

# Set parameters for task 
orderly_parameters(analysis = NULL, 
                   age_scaling = NULL)

# Set dependencies 
orderly2::orderly_dependency("1_create_parameter_list",
                             "latest(parameter:analysis == this:analysis
                             && parameter:age_scaling == this:age_scaling)",
                             c(parameters_torun_R21.rds = "parameters_torun_R21.rds"))

pars <- readRDS("parameters_torun_R21.rds")


output_overall <- data.frame()
# output_ageyr <- data.frame()
output_ageyrto50 <- data.frame()
output_last15 <- data.frame()

# using files that started 
# output_overall <- readRDS('R:/Kelly/catchup_extraboosters/archive/5_process_combined/20241018-170856-c8607bb1/output_overall_intermediate.rds')
# output_ageyr <- readRDS('R:/Kelly/catchup_extraboosters/draft/5_process_combined/20241018-083359-49de9d8f/output_ageyr_intermediate.rds')
# output_ageyrto50 <- readRDS('R:/Kelly/catchup_extraboosters/archive/5_process_combined/20241018-170856-c8607bb1/output_ageyr_toage50_intermediate.rds') 
# output_last15 <- readRDS('R:/Kelly/catchup_extraboosters/archive/5_process_combined/20241018-170856-c8607bb1/output_last15_intermediate.rds')
# 
# saveRDS(output_overall, 'output_overall_intermediate.rds')
# saveRDS(output_ageyr, 'output_ageyr_intermediate.rds')
# saveRDS(output_ageyrto50, 'output_ageyr_toage50_intermediate.rds')
# saveRDS(output_last15, 'output_last15_intermediate.rds')

# output_ageyrto50 <- output_ageyr %>%
#   filter(age_lower < 50 & # keep only 0.5 year age groups up to age 50
#            !(age_lower == 0 & age_upper == 5) &
#            !(age_lower == 0 & age_upper == 100) &
#            !(age_lower == 5 & age_upper == 10) &
#            !(age_lower == 10 & age_upper == 15))


# Add each set of processed runs to dataset
for(i in as.integer(c(seq(1, max(pars$scenarioID)-500, by = 500), 34501))){ #seq(0.5, 18, by = 0.5), (removing this bc just adding to orignals for now)
  message(i)
  orderly2::orderly_dependency("4_combine_runs",
                               "latest(parameter:analysis == this:analysis
                             && parameter:n500 == environment:i)",
                               c("data/summarized_draws_${i}.rds" = "summarized_draws_500.rds",
                                 "data/summarized_ageyr_draws_${i}.rds" = 'summarized_ageyr_draws_500.rds',
                                 "data/summarized_last15_draws_${i}.rds" = 'summarized_last15_draws_500.rds'))

  ageyr <- readRDS(paste0('data/summarized_ageyr_draws_',i,'.rds'))
  message('ageyr read in')

  output_overall <- rbind(output_overall, readRDS(paste0('data/summarized_draws_',i,'.rds')))
  message('overall read in')

  # output_ageyr <- rbind(output_ageyr, ageyr)
  # message('ageyr rbinded')

  ageyrto50 <- ageyr %>%
    filter(age_lower < 50 & # keep only 0.5 year age groups up to age 50
             !(age_lower == 0 & age_upper == 5) &
             !(age_lower == 0 & age_upper == 100) &
             !(age_lower == 5 & age_upper == 10) &
             !(age_lower == 10 & age_upper == 15))
  output_ageyrto50 <- rbind(output_ageyrto50, ageyrto50, fill = TRUE  )
  message('ageyr to 50 rbinded')


  output_last15 <- rbind(output_last15, readRDS(paste0('data/summarized_last15_draws_',i,'.rds')), fill = TRUE)
  message(paste0(Sys.time(), ' bound last15'))

  saveRDS(output_overall, 'output_overall_intermediate.rds')
  message(paste0(Sys.time(), ' saved overall'))
  # saveRDS(output_ageyr, 'output_ageyr_intermediate.rds')
  # message(paste0(Sys.time(), ' saved ageyr'))
  saveRDS(output_ageyrto50, 'output_ageyr_toage50_intermediate.rds')
  message(paste0(Sys.time(), ' saved ageyr to 50'))
  saveRDS(output_last15, 'output_last15_intermediate.rds')
  message(paste0(Sys.time(), ' saved last15'))

  message(paste0(Sys.time(), ' saved datasets'))
}


# Outputs for task 
orderly_artefact(
  'Produces processed datasets, summarized by age over whole sim, last 15 yrs, and first 5 years,
  and summarized by age and year, all with and without draws',
  c(#'output_ageyr_combined_raw.rds', # combined dataset with no further processing 
    # Overall (30 years) - age only
    "summarized_overall.rds",
    "summarized_overall_draws.rds",
    # Last 15 years - age only
    "summarized_last15_draws.rds",
    "summarized_last15.rds",
    # Intermediate dfs
    'output_overall_intermediate.rds',
    # 'output_ageyr_intermediate.rds',
    'output_ageyr_toage50_intermediate.rds',
    'output_last15_intermediate.rds'
  )
)

# Set resources
orderly_resource(
  c('collapse_by_scenario.R',
    'add_labels.R',
    'outcomes_averted.R',
    'find_frontiers.R',
    'process_combined.R'
    ))

# Functions to source
source('collapse_by_scenario.R')
source('add_labels.R')
source('outcomes_averted.R')
source('find_frontiers.R')
source('process_combined.R')


# Processing all different runs 
process_combined(output_overall)
process_combined(output_last15)
# process_combined(output_ageyr)
