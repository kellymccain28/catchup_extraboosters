# Workflow to send simulation/processing runs to the cluster 
send_report3 <- function(pars, x, queue_hold, age_scaling){ #x is a vector of the rows in pars df that we want to run 
  if(queue_hold !='none'){
    res <- hipercow_resources(hold_until = queue_hold)
  } else {
    res <- NULL
  }
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