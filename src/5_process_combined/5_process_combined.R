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
output_ageyrto50 <- data.frame()
output_last15 <- data.frame()


# done4 <- completed_reports(report_name = '4_combine_runs')
# donerecent <- done4 %>% filter(date_time > 20250311000000)
# donerecent <- donerecent %>% arrange(n500, date_time) %>%
#   group_by(n500, age_scaling) %>% slice_max(date_time)
# # saveRDS(donerecent, 'pars_folders.rds')

# report_name = '4_combine_runs'
# meta <- orderly2::orderly_metadata_extract(name = report_name, extract = c('time', 'parameters'), 
#                                            options = orderly2::orderly_search_options(allow_remote = TRUE))
# 
# meta2 <- meta
# meta_orig <- meta
# meta2$age_scaling <- sapply(meta2$parameters, function(x) {
#   if(is.null(x$age_scaling)) NA else x$age_scaling
# })
# meta2$n500 <- sapply(meta2$parameters, function(x) {
#   if(is.null(x$n500)) NA else x$n500
# })
# meta2$to <- sapply(meta2$parameters, function(x) {
#   if(is.null(x$to)) NA else x$to
# })
# 
# meta2<- meta2 |>
#   mutate(directory_name = id) |>
#   tidyr::separate(col = id, into = c('date', 'time'), sep = '-')|>
#   mutate(date= as.numeric(date)) |>
#   mutate(date_time = as.numeric(paste0(date, time))) %>%
#   filter(!is.na(age_scaling))
# 
# meta21 <- meta2 %>%
#   group_by(age_scaling, n500, to) %>%
#   mutate(keep = ifelse(date_time == max(date_time, na.rm = TRUE),1,0)) %>%
#   filter(keep == 1) %>% ungroup()
# 
# table(meta21$age_scaling)
# saveRDS(meta21, 'pars_folders.rds')
orderly_resource('pars_folders.rds')
pars_foldersraw <- readRDS('pars_folders.rds')

pars_folders <- pars_foldersraw %>%
  filter(age_scaling_par == age_scaling)

# Add each set of processed runs to dataset
for(i in as.integer(c(seq(1, max(pars$scenarioID)-500, by = 500), 34501))){ #seq(0.5, 18, by = 0.5), (removing this bc just adding to orignals for now)
  message(i)

  folder <- pars_folders[pars_folders$n500 == i,]$directory_name
  # orderly2::orderly_dependency("4_combine_runs",
  #                              "latest(parameter:analysis == this:analysis
  #                            && parameter:n500 == environment:i && parameter_age_scaling == this:age_scaling)",
  #                              c("data/summarized_draws_${i}.rds" = "summarized_draws_500.rds",
  #                                "data/summarized_ageyr_draws_${i}.rds" = 'summarized_ageyr_draws_500.rds',
  #                                "data/summarized_last15_draws_${i}.rds" = 'summarized_last15_draws_500.rds'))
  # ageyr <- readRDS(paste0('data/summarized_ageyr_draws_',i,'.rds'))
  ageyr <- readRDS(paste0("R:/Kelly/catchup_extraboosters/archive/4_combine_runs/", folder, '/summarized_ageyr_draws_500.rds'))
  message('ageyr read in')

  # output_overall <- rbind(output_overall, readRDS(paste0('data/summarized_draws_',i,'.rds')))
  output_overall <- rbind(output_overall, readRDS(paste0("R:/Kelly/catchup_extraboosters/archive/4_combine_runs/", folder, '/summarized_draws_500.rds')))
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


  # output_last15 <- rbind(output_last15, readRDS(paste0('data/summarized_last15_draws_',i,'.rds')), fill = TRUE)
  output_last15 <- rbind(output_last15, readRDS(paste0("R:/Kelly/catchup_extraboosters/archive/4_combine_runs/", folder, '/summarized_last15_draws_500.rds')), fill = TRUE)
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
# # for normal assumptions age_scaling = 1
# if (age_scaling == 1){
#   output_overall <- readRDS("R:/Kelly/catchup_extraboosters/archive/5_process_combined/20250225-111021-3d02a447/output_overall_intermediate.rds")
#   output_last15 <- readRDS("R:/Kelly/catchup_extraboosters/archive/5_process_combined/20250225-111021-3d02a447/output_last15_intermediate.rds")
#   output_ageyr_toage50_intermediate <- readRDS("R:/Kelly/catchup_extraboosters/archive/5_process_combined/20250225-111021-3d02a447/output_ageyr_toage50_intermediate.rds")
# } else if (age_scaling == 0.64){
#   # for 0.64 assumption
#   output_overall <- readRDS("R:/Kelly/catchup_extraboosters/archive/5_process_combined/20250324-203431-4d4ab63a/output_overall_intermediate.rds")
#   output_last15 <- readRDS("R:/Kelly/catchup_extraboosters/archive/5_process_combined/20250324-203431-4d4ab63a/output_last15_intermediate.rds")
#   output_ageyr_toage50_intermediate <- readRDS("R:/Kelly/catchup_extraboosters/archive/5_process_combined/20250324-203431-4d4ab63a/output_ageyr_toage50_intermediate.rds")
# }

# saveRDS(output_overall, 'output_overall_intermediate.rds')
# saveRDS(output_ageyr_toage50_intermediate, 'output_ageyr_toage50_intermediate.rds')
# saveRDS(output_last15, 'output_last15_intermediate.rds')

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
    'add_agegrps.R',
    'outcomes_averted.R',
    'find_frontiers.R',
    'process_combined.R'
    ))

# Functions to source
source('collapse_by_scenario.R')
source('add_agegrps.R')
source('outcomes_averted.R')
source('find_frontiers.R')
source('process_combined.R')


# Processing all different runs 
process_combined(output_overall)
process_combined(output_last15)
# process_combined(output_ageyr)
