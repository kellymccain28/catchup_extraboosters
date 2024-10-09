# Script to pull out manuscript figures from the saved plots and copy them to a new folder 
dir.create('manuscript_figures/')

# Figure 1: CA and SA for CU strategies 
file.copy(from = 'plots/plot_cumulCASA_CU_perennial.png',
          to = 'manuscript_figures/1_plot_cumulCASA_CU_perennial.png')

# Figure 2: Cases averted per 1000 people by age at vaccination and age (perennial) - from plots_mim_ammnet.R
# file.copy(from = 'plots/plot_cohorts_ageatvax_CAperpop_per.png', 
#           to = 'manuscript_figures/2_plot_cohorts_ageatvax_CAperpop_per.png')plot_cohorts_ageatvax_CAperpoptotals_per.png
file.copy(from = 'plots/plot_cohorts_ageatvax_CAperpoptotals_per.png', 
                    to = 'manuscript_figures/2_plot_cohorts_ageatvax_CAperpoptotals_per.png')

# Figure 3: CA and SA for AB strategies 
file.copy(from = 'plots/plot_cumulCASA_AB_perennial.png',
          to = 'manuscript_figures/3_plot_cumulCASA_AB_perennial.png')

# Table 2:
file.copy(from = 'plots/outcomes_averted_CUorAB_perennial.csv',
          to = 'manuscript_figures/Table2_outcomes_averted_CUorAB_perennial.csv')

# Table S4
file.copy(from = 'plots/outcomes_averted_combinedstrategies_perennial.csv',
          to = 'manuscript_figures/TableS4_outcomes_averted_combinedstrategies_perennial.csv')

# Figure 4: Cohort view of cases averted per 1000 people AB
file.copy(from = 'plots/cohorts_AB_CA_perpop_perennial.png',
          to = 'manuscript_figures/4_cohorts_AB_CA_perpop_perennial.png')

# Figure 5: Efficiency frontier
file.copy(from = 'plots/CASAbytotaldosesperennial.png',
          to = 'manuscript_figures/5_CASAbytotaldosesperennial.png')



# Values in text are from:
# get_perc_U5.R --> percentage of cases and cases averted in U5s
# get_perc_dominated.R --> % of non-dominated scenarios that are CU, AB, etc.

file.copy(from = 'plots/cohorts_sum_ageatvaxperennial.csv',
          to = 'manuscript_figures/cohorts_sum_ageatvax_perennial.csv')
file.copy(from = 'plots/cohorts_sum_ageatvaxseasonal.csv',
          to = 'manuscript_figures/cohorts_sum_ageatvax_seasonal.csv')

# text after figure 1 : 
file.copy(from = 'plots/cohorts_CAperdose_catch-up no boosterperennial.csv',
          to = 'manuscript_figures/cohorts_CAperdose_catch-up no boosterperennial.csv')
# have same file for a age-based
file.copy(from = 'plots/cohorts_CAperdose_age-basedperennial.csv',
          to = 'manuscript_figures/cohorts_CAperdose_age-basedperennial.csv')


# SI Figures
# S1
file.copy(from = 'plots/plot_cumulCA_SVhybrid_none_seasonal.png',
          to = 'manuscript_figures/S1_plot_cumulCA_SVhybrid_none_seasonal.png')

# S2
file.copy(from = 'plots/plot_cumulCASAperpop_CU_perennial.png',
          to = 'manuscript_figures/S2_plot_cumulCASAperpop_CU_perennial.png')

# S3
file.copy(from = 'plots/plot_cumulCASA_CU_seasonal.png',
          to = 'manuscript_figures/S3_plot_cumulCASA_CU_seasonal.png')

# S4 
file.copy(from = 'plots/plot_cumulCASAperpop_AB_perennial.png',
          to = 'manuscript_figures/S4_plot_cumulCASAperpop_AB_perennial.png')

# S5
file.copy(from = 'plots/plot_cumulCASA_AB_seasonal.png',
          to = 'manuscript_figures/S5_plot_cumulCASA_AB_seasonal.png')

# S6
file.copy(from = 'plots/cohorts_AB_SA_perpop_perennial.png',
          to = 'manuscript_figures/S6_cohorts_AB_SA_perpop_perennial.png')

# S7
file.copy(from = 'plots/CASAbytotaldosesseasonal.png',
          to = 'manuscript_figures/5_CASAbytotaldosesseasonal.png')

# Table S5
file.copy(from = 'plots/percent_cases.csv',
          to = 'manuscript_figures/TableS5_percent_cases.csv')

# Table s6
file.copy(from = 'plots/table_perc_outcomes_averted.csv',
          to = 'manuscript_figures/TableS6_table_perc_outcomes_averted.csv')

