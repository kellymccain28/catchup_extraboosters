# Task to produce all plots for final output 

# Set up task  ------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(orderly2)
library(data.table)
library(janitor)
library(purrr)
library(tidyr)
library(cowplot)
library(ggrepel)
library(stringr)
library(viridis)
library(scales)
library(grid)
library(png)

orderly_strict_mode()
orderly2::orderly_description('Make figures for manuscript')

# Set parameters for task 
orderly_parameters(age_scaling = NULL)

# Set dependencies 
orderly2::orderly_dependency("5_process_combined",
                             "latest(parameter:age_scaling == this:age_scaling)",
                             c(summarized_overall.rds = "summarized_overall.rds",
                               summarized_overall_draws.rds = "summarized_overall_draws.rds",
                               # summarized_ageyr.rds = "summarized_ageyr.rds",
                               summarized_last15.rds = "summarized_last15.rds"))

# Set resources
orderly_resource(
  c('plot_themes.R',
    'plot_cumul_CA.R',
    'plot_efficiency_frontier.R',
    'plot_age_dist.R',
    'plot_cohort.R',
    'table_CA_perreldose.R',
    'plot_perc_averted.R',
    'plots_mim_ammnet.R',
    'manuscript_figures.R',
    'get_perc_dominated.R',
    'get_perc_U5.R',
    'add_labels.R',
    'legend.png',
    'insert_blank_rows_latex.R'
  )
)
source('add_labels.R')

df_summ <- readRDS("summarized_overall.rds") %>% 
  add_labels()
df_summ_draws <- readRDS("summarized_overall_draws.rds")%>% 
  add_labels()
# df_ageyr <- readRDS("summarized_ageyr.rds")
df_last15 <- readRDS("summarized_last15.rds") %>% 
  add_labels()

orderly2::orderly_dependency("6_make_cohorts",
                             "latest(parameter:age_scaling == this:age_scaling)",
                             c(cohorts_byage.rds = "cohorts_byage.rds",
                               cohorts_ageatvaxandage.rds = "cohorts_ageatvaxandage.rds",
                               cohorts_ageatvax.rds = "cohorts_ageatvax.rds",
                               cohorts.rds = "cohorts.rds"))

cohorts_byage <- readRDS('cohorts_byage.rds')%>% 
  add_labels()
cohorts <- readRDS('cohorts.rds')%>% 
  add_labels()
cohorts_ageatvaxandage <- readRDS('cohorts_ageatvaxandage.rds')%>% 
  add_labels()
cohorts_ageatvax <- readRDS('cohorts_ageatvax.rds')%>% 
  add_labels()


# Functions to source
source('plot_themes.R')
source('plot_cumul_CA.R')
source('plot_efficiency_frontier.R')
source('plot_age_dist.R')
source('plot_cohort.R')
source('table_CA_perreldose.R')
source('plot_perc_averted.R')
source('plots_mim_ammnet.R')
source('insert_blank_rows_latex.R')

dir.create('plots/')
# Make the plots 
# Figure 1 and 4, S1 and S3, S4
plot_cumul_CA(df_summ = df_summ,
              df_last15 = df_last15,
              cohorts = cohorts)

# Figure 2
# plot_age_dist(df = cohorts_byage,
#               seas_type = 'seasonal')
# plot_age_dist(df = cohorts_byage,
#               seas_type = 'perennial')

# Plot percentage averted 
plot_perc_averted(df = df_summ)

# Figure 5 -- only use the age-based ones in the manuscript here
plot_cohort(df = cohorts_byage)
plot_cohort(df = cohorts_byage,
            seas = 'perennial')


# Figure 6
plot_efficiency_frontier(df = df_summ)
plot_efficiency_frontier(df = df_summ,
                         seas_type= 'perennial')

# Table 
table_CA_perreldose(df_summ = df_summ)
table_CA_perreldose(df_summ = df_summ, 
                    seas_name = 'seasonal')

# Make plots for MIM/AMMnet
makeplots()

# Get % dominated scenarios 
source('get_perc_dominated.R')

# Get % in U5s and school-aged
source('get_perc_U5.R')

# Move manuscript figures to a different folder
source('manuscript_figures.R')
