# Function to process combined processed runs using other functions in report 5.5

#' @param df data frame - can be output_overall, output_ageyr, output_last15
process_combined <- function(df){
  library(stringr)
  
  dfname <- stringr::str_replace(deparse(substitute(df)), pattern = 'output_', replacement = '')
  
  draws <- df %>%
    outcomes_averted() %>%
    add_agegrps()
  message(paste0(Sys.time(), 'get draws ', dfname))
  if(dfname != 'ageyr'){ # save draws df only if not by ageyr (too big)
    saveRDS(draws, paste0('summarized_', dfname, '_draws.rds'))
  }
  
  d <- draws %>%
    collapse_by_scenario() %>%
    find_frontiers()
  message(paste0(Sys.time(), 'get summarized ', dfname))
  saveRDS(d, paste0('summarized_', dfname, '.rds'))

  if(dfname == 'ageyr'){
    drawsto50 <- draws %>%
      filter(age_lower < 50 & # keep only 0.5 year age groups up to age 50
               age_grp != '0-5' & 
               age_grp != '0-100' & age_grp != '5-10' & age_grp != '10-15') 
    saveRDS(drawsto50, paste0('summarized_toage50_', dfname, '_draws.rds'))
    
    dto50 <- d %>%
      filter(age_lower < 50 & # keep only 0.5 year age groups up to age 50
               age_grp != '0-5' & 
               age_grp != '0-100' & age_grp != '5-10' & age_grp != '10-15') 
    saveRDS(dto50, paste0('summarized_toage50_', dfname, '.rds'))
    
    rm(drawsto50)
    rm(dto50)
  }
  
  rm(df)
  rm(draws)
  rm(d)

  message('Removed summarized dfs to make space')
  message(paste0('finished ', dfname))
}
