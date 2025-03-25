# Workflow to send simulation/processing runs to the cluster 
send_report3 <- function(pars, x, queue_hold, age_scaling){ #x is the rows in pars df that we want to run 
  # Read in parameter list 
  # x <- 1:nrow(pars)
  # x <- c(8835:nrow(pars))
  # x <- 18001:nrow(pars)
  # x <- 8835:12000 #5a33dcf230501c60ce3c9cb5973d6f1c and a7154f08413676b3ba073b5c196504f6 failed 
  # x <- 12001:15000
  # x <- c(11289, 11343, 15000:22200)
  # x = 8834
  res <- hipercow_resources(hold_until = queue_hold)
  
  bundle3 <- task_create_bulk_expr(
    orderly2::orderly_run('3_run_process', parameters = list(analysis = 'catch-up',
                                                             drawID = drawID,
                                                             pfpr = pfpr,
                                                             seas_name = seas_name,
                                                             PEV = PEV,
                                                             PEVstrategy = PEVstrategy,
                                                             PEVage = PEVage,
                                                             PEVrounds = PEVrounds,
                                                             EPIbooster = EPIbooster,
                                                             EPIextra = EPIextra,
                                                             massbooster_rep = massbooster_rep,
                                                             MDA = MDA,
                                                             par_index = scenarioID,
                                                             age_scaling = age_scaling)),
    pars[x,],
    environment = 'malsimruns3',
    resources = res, 
    bundle_name = 'report3'
  )
  
  return(bundle3)
}
# hipercow_bundle_wait(bundle3)
# hipercow_bundle_log_value(bundle4)[1]
# hipercow_bundle_result(bundle3)
# hipercow_bundle_status(bundle3)
