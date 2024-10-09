# Workflow to send report to cluster to make cohort files 
library(hipercow)

task6 <- task_create_expr(orderly2::orderly_run(name = '6_make_cohorts',
                                                 parameters = list(analysis = 'catch-up')))


task_wait(task6)
task_log_show(task6)
task_result(task6)


task6 <- task_create_expr(orderly2::orderly_run(name = '6_produce_plots'))
task_log_value(task6)
