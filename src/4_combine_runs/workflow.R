# Workflow to pull and combine processed runs (task 4)
send_report4 <- function(queue_hold, age_scaling){

  # Read in parameter list 
  # pars <- readRDS('R:/Kelly/catchupR21/archive/1_create_parameter_list/20240503-125203-48a00fa0/parameters_torun_R21.rds')
  
  # n500list = data.frame(n500 = seq(0.5, max(pars$scenarioID)/1000, by = 0.5),
  # nums = paste0(seq(0.5, max(pars$scenarioID)/1000, by = 0.5)*1000-499, ' to ',
  #               seq(0.5, max(pars$scenarioID)/1000, by = 0.5)*1000))
  
  nlist = data.frame(by = as.integer(c(seq(1, 34800-499, by = 500), 34501)),#, seq(18001, max(pars$scenarioID)-99, by = 100)),
                     to = as.integer(c(seq(500, 34800, by = 500), 34800)))#, seq(18100, max(pars$scenarioID), by = 100)))
  
  res <- hipercow_resources(hold_until = queue_hold)
    
  # Send to cluster
  bundle4 <- task_create_bulk_expr(
    orderly2::orderly_run('4_combine_runs', parameters = list(analysis = 'catch-up',
                                                              n500 = by,
                                                              to = to, 
                                                              age_scaling = age_scaling)),
    nlist,
    resources = res
  )
  
  return(bundle4)
}
# hipercow_bundle_wait(bundle5)
# hipercow_bundle_log_value(bundle5)[1]
# hipercow_bundle_result(bundle4)
# table(hipercow_bundle_status(bundle4))
# fail <- which(hipercow_bundle_status(bundle4) == 'failure')
# fail  
# task_log_show(bundle4$ids[18])
