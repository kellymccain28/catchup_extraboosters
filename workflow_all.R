 # Script to run all orderly reports 
library(hipercow)
library(orderly2)

# Task 1 - generate parameters
# orderly2::orderly_run(name = '1_create_parameter_list',
#                       parameters = list(analysis = 'catch-up'))

# skip report 2 because don't need to re-calibrate


# Task 3 - run and process
source('src/3_run_process/workflow.R')
pars <- readRDS('R:/Kelly/catchup_extraboosters/archive/1_create_parameter_list/20240906-171402-faa8ade3/parameters_torun_R21.rds')
# bundle3a <- send_report3(pars, x = 1:10000, queue_hold = 'tonight')#872
# bundle3b <- send_report3(pars, x = 600:34800, queue_hold = 'weekend')#150
# bundle3c <- send_report3(pars, x = 20001:34800, queue_hold = 'weekend')
hipercow_bundle_wait(bundle3)
table(hipercow_bundle_status(bundle3b))
rerun <- which(hipercow_bundle_status(bundle3b) == 'failure' | hipercow_bundle_status(bundle3b) == 'submitted')
bundle3d <- send_report3(pars, x = rerun, queue_hold = 'tonight')
table(hipercow_bundle_status(bundle3d))

failed <- which(hipercow_bundle_status(bundle3d) == 'failure')
done3 <- completed_reports(report_name = '3_run_process')
done <- done3 %>% 
  filter(date >= 20240909)
table(hipercow_bundle_status(bundle3d))
nums <- 1:34800
missed <- nums[!(nums %in% done$par_index)]


bundle3e <- send_report3(pars, x = missed, queue_hold = 'tonight')
table(hipercow_bundle_status(bundle3e))
failed <- which(hipercow_bundle_status(bundle3e) == 'failure')

bundle3f <- send_report3(pars, x = failed, queue_hold = '1m')
table(hipercow_bundle_status(bundle3f))

# Task 4 - combine runs 
source('src/4_combine_runs/workflow.R')
bundle4 <- send_report4(queue_hold = '1m')
hipercow_bundle_wait(bundle4)
hipercow_bundle_status(bundle4)
done4 <- completed_reports(report_name = '4_combine_runs')
nums <- c(seq(1, 34800-499, by = 500), 34501)
nlist <- nlist[nlist$by %in% missed,]

# # Task 5 - process + bind together
task5 <- task_create_expr(orderly2::orderly_run(name = '5_process_combined',
                                                 parameters = list(analysis = 'catch-up')))
task_wait(task5)
# task_status(task5)
# task_log_show(task5)

# Task 6 - make cohorts 
task6 <- task_create_expr(orderly2::orderly_run(name = '6_make_cohorts',
                                                parameters = list(analysis = 'catch-up')))
task_wait(task6)
# task_log_show(task6)
# 
# # Task 7 - make plots 
task7 <- task_create_expr(orderly2::orderly_run(name = '7_produce_plots'))
# task_wait(task7)
task_log_show(task7)

