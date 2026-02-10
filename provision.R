# Cran
install.packages('pacman')
pacman::p_load(dplyr,data.table, janitor,purrr,tidyr,retry,zoo,ggplot2,
               cowplot,ggrepel,stringr,tidyverse,viridis,png,grid,
               Rcpp, rlang, glue, fs, devtools, dampack, svglite, ggpattern)

install.packages(
  c("orderly"),#, "malariasimulation"
  repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))
install.packages(
  "orderly2",
  repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))
# Github 
devtools::install_github("kellymccain28/malariasimulation_agescaling") # adapted from JC's adult scaling work to include adolescent scaling (5-15)
  # devtools::install_github('mrc-ide/malariasimulation')
devtools::install_github("mrc-ide/postie")
# github::kellymccain28/postie 
devtools::install_github("mrc-ide/cali")
