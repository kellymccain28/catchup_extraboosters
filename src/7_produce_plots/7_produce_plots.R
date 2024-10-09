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

orderly_strict_mode()
orderly2::orderly_description('Make figures for manuscript')

# Set parameters for task 
# no parameters

# Set dependencies 
orderly2::orderly_dependency("5_process_combined",
                             "latest()",
                             c(summarized_overall.rds = "summarized_overall.rds",
                               summarized_overall_draws.rds = "summarized_overall_draws.rds",
                               # summarized_ageyr.rds = "summarized_ageyr.rds",
                               summarized_last15.rds = "summarized_last15.rds"))

df_summ <- readRDS("summarized_overall.rds")
df_summ_draws <- readRDS("summarized_overall_draws.rds")
# df_ageyr <- readRDS("summarized_ageyr.rds")
df_last15 <- readRDS("summarized_last15.rds")

orderly2::orderly_dependency("6_make_cohorts",
                             "latest()",
                             c(cohorts_byage.rds = "cohorts_byage.rds",
                               cohorts_ageatvaxandage.rds = "cohorts_ageatvaxandage.rds",
                               cohorts_ageatvax.rds = "cohorts_ageatvax.rds",
                               cohorts.rds = "cohorts.rds"))

cohorts_byage <- readRDS('cohorts_byage.rds')
cohorts <- readRDS('cohorts.rds')
cohorts_ageatvaxandage <- readRDS('cohorts_ageatvaxandage.rds')
cohorts_ageatvax <- readRDS('cohorts_ageatvax.rds')

# Outputs for task 
orderly_artefact(
  'Produces a series of plots',
  c(# Cumulative cases averted (plot_cumul_CA.R)
    "plots/plot_cumulCA_catchupnobooster_none_seasonal.png", #
    "plots/plot_cumulCA_AB_none_seasonal.png", #
    "plots/plot_cumulCA_SVhybrid_none_seasonal.png",
    "plots/plot_cumulSA_catchupnobooster_none_seasonal.png",
    "plots/plot_cumulSA_AB_none_seasonal.png",
    "plots/plot_cumulSA_SVhybrid_none_seasonal.png",
    "plots/plot_cumulCASA_catchupnobooster_none_seasonal.png", 
    "plots/plot_cumulCASA_AB_none_seasonal.png", #
    "plots/plot_cumulCA_catchupnobooster_none_perennial.png", #
    "plots/plot_cumulCA_AB_none_perennial.png", #
    "plots/plot_cumulCA_SVhybrid_none_perennial.png",
    "plots/plot_cumulSA_catchupnobooster_none_perennial.png",
    "plots/plot_cumulSA_AB_none_perennial.png",
    "plots/plot_cumulSA_SVhybrid_none_perennial.png",
    "plots/plot_cumulCASA_catchupnobooster_none_perennial.png", 
    "plots/plot_cumulCASA_AB_none_perennial.png", #
    # Age distributions (plot_age_dist.R)
    # 'plots/Age_dist_casesperperson_CU_seasonal.png',
    # "plots/Age_dist_sevcasesperperson_CU_seasonal.png",
    # "plots/Age_dist_casesandsev_perperson_CU_seasonal.png",
    # 'plots/Age_dist_casesperperson_CU_perennial.png',
    # "plots/Age_dist_sevcasesperperson_CU_perennial.png",
    # "plots/Age_dist_casesandsev_perperson_CU_perennial.png",
    # Percent averted (plot_perc_averted.R)
    "plots/plot_perc_uncomplicated_averted.png",
    "plots/heatmap_perc_uncomplicated_averted.png",
    "plots/table_perc_outcomes_averted.csv",
    # Cohort plots 
    ## Efficiency frontiers
    "plots/CAbytotaldosesseasonal.png",
    "plots/SAbytotaldosesseasonal.png",
    "plots/casesbytotaldosesseasonal.png",
    "plots/sevcasesbytotaldosesseasonal.png",
    'plots/CASAbytotaldosesseasonal.png',
    
    "plots/CAbytotaldosesperennial.png",
    "plots/SAbytotaldosesperennial.png",
    "plots/casesbytotaldosesperennial.png",
    "plots/sevcasesbytotaldosesperennial.png",
    'plots/CASAbytotaldosesperennial.png',
    # Tables (table_CA_perreldose.R)
    'plots/outcomes_averted_CUorAB_perennial.csv',
    'plots/outcomes_averted_combinedstrategies_perennial.csv',
    # plots mim ammnet
    "plots/cohorts_CU_CAperpop_AMMnetMIM_seas.png", 
    # "plots/cohorts_CU_CAperdose_AMMnetMIM_seas.png",
    "plots/cohorts_CU_casesperpop_AMMnetMIM_seas.png",
    "plots/cohorts_AB_CAperpop_AMMnetMIM_seas.png", 
    # "plots/cohorts_AB_CAperdose_AMMnetMIM_seas.png",
    "plots/cohorts_AB_casesperpop_AMMnetMIM_seas.png",
    "plots/plot_cohorts_ageatvax_sevcases_seas.png",
    "plots/plot_cohorts_ageatvax_cases_seas.png",
    "plots/plot_cohorts_ageatvax_CA_seas.png",
    "plots/plot_cohorts_ageatvax_CAperpop_seas.png",
    
    "plots/cohorts_CU_CAperpop_AMMnetMIM_per.png", 
    # "plots/cohorts_CU_CAperdose_AMMnetMIM_per.png",
    "plots/cohorts_CU_casesperpop_AMMnetMIM_per.png",
    "plots/cohorts_AB_CAperpop_AMMnetMIM_per.png", 
    # "plots/cohorts_AB_CAperdose_AMMnetMIM_per.png",
    "plots/cohorts_AB_casesperpop_AMMnetMIM_per.png",
    "plots/plot_cohorts_ageatvax_sevcases_per.png",
    "plots/plot_cohorts_ageatvax_cases_per.png",
    "plots/plot_cohorts_ageatvax_CA_per.png",
    "plots/plot_cohorts_ageatvax_CAperpop_per.png"
    )
)

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
    'get_perc_U5.R'
    )
)

# Functions to source
source('plot_themes.R')
source('plot_cumul_CA.R')
source('plot_efficiency_frontier.R')
source('plot_age_dist.R')
source('plot_cohort.R')
source('table_CA_perreldose.R')
source('plot_perc_averted.R')
source('plots_mim_ammnet.R')

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
