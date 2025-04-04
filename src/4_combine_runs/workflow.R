# Workflow to pull and combine processed runs (task 4)
send_report4 <- function(queue_hold, age_scaling){

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