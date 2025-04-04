# Script to run all orderly reports 
library(hipercow)
library(orderly2)
# source("C:/Users/kem22/OneDrive - Imperial College London/R work/Useful functions/completed_reports.R")

# Task 1 - generate parameters
orderly2::orderly_run(name = '1_create_parameter_list',
                      parameters = list(analysis = 'catch-up',
                                        age_scaling = 1)) # 1 or 0.64


# Task 2 - calibration of EIR to PfPR  
# ONly need to run this once to get EIR estimate (same can be used regardless of vaccination scaling)
task_create_expr(orderly2::orderly_run(name = '2_calibration_to_PfPR'))


# Task 3 - run and process
source('src/3_run_process/workflow.R')
pars <- readRDS("R:/Kelly/catchup_extraboosters/archive/1_create_parameter_list/20250210-102645-d865230a/parameters_torun_R21.rds")
pars_sml <- pars %>% select(-params) %>% mutate(par_index = scenarioID)
bundle3b <- send_report3(pars,
                         x = c(50, 100, 150, 200, 250, 500, 600, 650, 700, 750, 800),
                         queue_hold = '1m',
                         age_scaling = 1)#872
xvals <- seq(1, 2000)
xvals <- xvals[!(xvals %in% c(50, 100, 150,200,250,500,600, 650, 700, 750, 800))]
xvals <- seq(4000, 6000)
xvals <- seq(6001, 15000)
bundle3c <- send_report3(pars, x = xvals, queue_hold = 'tonight', age_scaling = 1)
xvals <- seq(15001, 25000)
bundle3d <- send_report3(pars, x = xvals, queue_hold = 'tonight', age_scaling = 1)
xvals <- seq(25001, 34800)
bundle3e <- send_report3(pars, x = xvals, queue_hold = 'tonight', age_scaling = 1)
table(hipercow_bundle_status(bundle3c))
table(hipercow_bundle_status(bundle3d))
table(hipercow_bundle_status(bundle3e))
source("C:/Users/kem22/OneDrive - Imperial College London/R work/Useful functions/completed_reports.R")
done3 <- completed_reports(report_name = '3_run_process')
# donerecent <- done3 %>% filter(date_time > 20250211000000)
donerecent <- done3 %>% arrange(par_index, date_time) %>%
  group_by(par_index) %>% slice_max(date_time)
pars_folders <- left_join(pars_sml, donerecent %>% select(par_index, directory_name, age_scaling), by = c('par_index','age_scaling'))
saveRDS(pars_folders, 'pars_folders.rds')
saveRDS(donerecent, 'completed_report3s.rds')


# Task 4 - combine runs 
source('src/4_combine_runs/workflow.R')
bundle4 <- send_report4(queue_hold = '1m', age_scaling = 1)
# bundle4 <- hipercow_bundle_load(name = 'chordal_hypacrosaurus' )
hipercow_bundle_status(bundle4)
task_log_show(bundle4$ids[1])
task_status(bundle4)

# done4 <- completed_reports(report_name = '4_combine_runs')

# Task 5 - process + bind togethe
task5 <- task_create_expr(orderly2::orderly_run(name = '5_process_combined',
                                                 parameters = list(analysis = 'catch-up',
                                                                   age_scaling = 1)))
# task_wait(task5)
task_status(task5)
task_log_show(task5)

# Task 6 - make cohorts 
task6 <- hipercow::task_create_expr(orderly2::orderly_run(name = '6_make_cohorts',
                                                parameters = list(analysis = 'catch-up',
                                                                  age_scaling = 1)))
task_wait(task6)
task_status(task6)
task_log_show(task6)

# # Task 7 - make plots 
task7 <- task_create_expr(orderly2::orderly_run(name = '7_produce_plots',
                                                parameters = list(age_scaling = 1)))
# task_wait(task7)
task_log_show(task7)
task_status(task7)


#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
# Age-scaled runs ----
source("C:/Users/kem22/OneDrive - Imperial College London/R work/Useful functions/completed_reports.R")

# Task 1 - generate parameters
orderly2::orderly_run(name = '1_create_parameter_list',
                      parameters = list(analysis = 'catch-up',
                                        age_scaling = 0.64)) # 1 or 0.64

# skip report 2 because don't need to re-calibrate

# Task 3 - run and process
source('src/3_run_process/workflow.R')
pars64 <- readRDS("R:/Kelly/catchup_extraboosters/archive/1_create_parameter_list/20250220-144405-74dfee69/parameters_torun_R21.rds")
# pars05 <- readRDS('R:/Kelly/catchup_extraboosters/archive/1_create_parameter_list/20250224-135641-b35e9239/parameters_torun_R21.rds')
pars_sml <- pars64 %>% select(-params) %>% mutate(par_index = scenarioID)
bundle3b_scaled <- send_report3(pars64,
                         x = c(13651, 13701, 13801, 13951, 14201, 14401),#c(13551, 13601, 13651, 13701, 13801, 13951, 14201, 14401),
                         queue_hold = '1m',
                         age_scaling = 0.64)
table(hipercow_bundle_status(bundle3b_scaled))

done3 <- completed_reports(report_name = '3_run_process')
donerecent <- done3 %>% filter(date_time > 20250211000000)
donerecent <- done3 %>% arrange(par_index, date_time, age_scaling) %>%
  group_by(par_index, age_scaling) %>% slice_max(date_time)
pars_folders <- left_join(pars_sml, donerecent %>% select(par_index, directory_name, age_scaling), by = c('par_index','age_scaling'))
saveRDS(pars_folders, 'pars_folders.rds')
saveRDS(donerecent, 'completed_report3s.rds')

# Task 4 - combine runs 
source('src/4_combine_runs/workflow.R')
## before doing this iwll need to re-run completed reports to get the folder names and re-save in the folder for task 4
bundle4_scaled <- send_report4(queue_hold = '1m', age_scaling = 0.64)
bundle4 <- hipercow_bundle_load(name = 'chordal_hypacrosaurus' )
table(hipercow_bundle_status(bundle4_scaled))
hipercow_bundle_status(bundle4_scaled)
task_log_show(bundle4_scaled$ids[3])
task_status(bundle4)

done4 <- completed_reports(report_name = '4_combine_runs')
recentdone4 <- done4 %>% filter(date_time > 20250211000000)

# Task 5 - process + bind together
task5_scaled_withfolders <- task_create_expr(orderly2::orderly_run(name = '5_process_combined',
                                                parameters = list(analysis = 'catch-up',
                                                                  age_scaling = 0.64)))
# task_wait(task5_scaled)
task_status(task5_scaled_withfolders)
task_log_show(task5_scaled_withfolders)

# Task 6 - make cohorts 
task6_scaled <- task_create_expr(orderly2::orderly_run(name = '6_make_cohorts',
                                                parameters = list(analysis = 'catch-up',
                                                                  age_scaling = 0.64)))
task_wait(task6_scaled)
task_status(task6_scaled)
task_log_show(task6_scaled)

# # Task 7 - make plots 
task7_scaled <- task_create_expr(orderly2::orderly_run(name = '7_produce_plots',
                                                parameters = list(age_scaling = 0.64)))
# task_wait(task7)
task_log_show(task7_scaled)
task_status(task7_scaled)
