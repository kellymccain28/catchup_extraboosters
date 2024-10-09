# Workflow for task 5.5 - combining batched runs and summarizing by scenario
library(hipercow)
task55 <- task_create_expr(orderly2::orderly_run(name = '5_process_combined',
                                       parameters = list(analysis = 'catch-up')))


# task_wait(task55)
task_log_value(task55)
task_result(task55)

task_log_value('9755ef87834bd2f90cdff273d6f31ecd')
