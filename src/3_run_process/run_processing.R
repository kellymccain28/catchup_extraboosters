# Wrapper for run_report for 4_process_runs

run_processing <- function(x, y){
  
  index <- seq(x, y, 1)
  
  lapply(index, run_report, reportname = '4_process_runs')
  
  
}
