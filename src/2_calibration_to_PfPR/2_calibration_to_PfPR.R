# Task to run calibration of the EIRs to PfPR2-10

library(cali)
library(malariasimulation)  
library(malariaEquilibrium)
library(dplyr)
library(orderly2)
# will output 50 runs per baseline scenario and then will combine them to produce EIRestimates.rds
# this output is used in the next step where we run the model to set the equilibrium

orderly_dependency(
  name = '1_create_parameter_list',
  "latest()",
  c(baseline_parameters.rds = "baseline_parameters.rds")
)

baseline_parameters <- readRDS('baseline_parameters.rds')

orderly_resource(
  'eir_prev_matching.R'
)

source('eir_prev_matching.R')

orderly_artefact(
  files = 'PrEIR/EIRestimates.rds'
)

xs <- 1:nrow(baseline_parameters)
ys <- baseline_parameters$drawID
index <- tibble(x = xs, y = ys)

# Run PrEIR matching for each setting and drawID -- this will take a long time 
for(x in xs){
  pr_match(index[x,]$x, index[x,]$y)
}

# read in results
files <- list.files(path = paste0("PrEIR"), pattern = "PRmatch_draws_", full.names = TRUE)
dat_list <- lapply(files, function (x) readRDS(x))

# concatenate all outputs together 
match <-  do.call("rbind", dat_list) |> as_tibble()

# summary(match$starting_EIR)

# take a look at failed jobs
# failed <- anti_join(combo, match, by = "ID") 

# save EIR estimates
saveRDS(match, "PrEIR/EIRestimates.rds")