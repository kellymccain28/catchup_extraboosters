insert_blank_rows_latex <- function(dataframe, column) {
  
  plain_labels <- unique(dataframe[[column]])
  tlabels      <- as.character(unique(dataframe[[column]]))
  
  df_split <- split(dataframe, dataframe[[column]])
  df_split <- df_split[as.character(plain_labels)]
  dataframe <- do.call(rbind, lapply(df_split, function(group) {
    na_row <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(group)))
    colnames(na_row) <- colnames(group)
    rbind(group, na_row)
  }))
  
  na_row <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(dataframe)))
  colnames(na_row) <- colnames(dataframe)
  
  dataframe <- rbind(na_row, dataframe)#add NAs for top row header
  
  dataframe <- dataframe[-nrow(dataframe),]#remove NAs at bottom
  dataframe[[column]] <- NULL#remove column
  inds                 <- which(!complete.cases(dataframe))
  dataframe[inds,1]    <- plain_labels
  # dataframe[[1]][inds] <- paste0("\\bfseries{", dataframe[[1]][inds], "}")
  dataframe            <- dataframe %>% mutate_all(~ ifelse(is.na(.), "", 
                                                            ifelse(. == 'NA (NA, NA)', 'Ref', .)))
  
  return(dataframe)
}
