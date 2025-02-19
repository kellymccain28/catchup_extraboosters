#' Return a list of the reports that have already completed from orderly metadata
#' @param report_name  name of orderly report to extract metadata from
#' @export
completed_reports<- function(report_name, datefilter = 20250211000000){
  
  
  meta <- orderly2::orderly_metadata_extract(name = report_name, extract = c('time', 'parameters')#, 
                                             # options = orderly2::orderly_search_options(allow_remote = TRUE)
                                             )
  
  meta<- meta |>
    dplyr::mutate(directory_name = id) |>
    tidyr::separate(col = id, into = c('date', 'time'), sep = '-')|>
    dplyr::mutate(date= as.numeric(date)) |>
    dplyr::mutate(date_time = as.numeric(paste0(date, time)))
  
  # Get most recent runs 
  meta <- meta |>
    filter(date_time > datefilter)
  
  meta<- data.table(meta)
  meta<- meta[, index:= c(1:.N) ]
  
  
  unique(lapply(meta$parameters, names))
  nms <- names(meta$parameters[[1]])
  pars <- do.call("data.frame", stats::setNames(lapply(nms, function(nm) sapply(meta$parameters, function(x) x[[nm]])), nms))
  pars<- data.table(pars)
  pars<- pars[, index:= c(1:.N)]
  
  meta<- meta |>
    dplyr::select(directory_name, index, date_time)
  map<- merge(pars, meta, by = 'index')
  map<- map |>
    select(-index)
  
  return(map)
}
