# Script to pull out manuscript figures from the saved plots and copy them to a new folder 
dir.create('manuscript_figures/')

# Figure 1: CA and SA for CU strategies 
file.copy(from = 'plots/plot_cumulCASA_CU_perennial.pdf',
          to = 'manuscript_figures/1_plot_cumulCASA_CU_perennial.pdf')

# Figure 2: Cases averted per 1000 people by age at vaccination and age (perennial) - from plots_mim_ammnet.R
# file.copy(from = 'plots/plot_cohorts_ageatvax_CAperpop_per.pdf', 
#           to = 'manuscript_figures/2_plot_cohorts_ageatvax_CAperpop_per.pdf')plot_cohorts_ageatvax_CAperpoptotals_per.pdf
file.copy(from = 'plots/plot_cohorts_ageatvax_CAperpoptotals_per.pdf', 
                    to = 'manuscript_figures/2_plot_cohorts_ageatvax_CAperpoptotals_per.pdf')

# Figure 3: CA and SA for AB strategies 
file.copy(from = 'plots/plot_cumulCASA_AB_perennial.pdf',
          to = 'manuscript_figures/3_plot_cumulCASA_AB_perennial.pdf')

# Table 2:
file.copy(from = 'plots/outcomes_averted_CUorAB_perennial.csv',
          to = 'manuscript_figures/Table2_outcomes_averted_CUorAB_perennial.csv')

# Table S4
file.copy(from = 'plots/outcomes_averted_combinedstrategies_perennial.csv',
          to = 'manuscript_figures/TableS4_outcomes_averted_combinedstrategies_perennial.csv')

# Figure 4: Cohort view of cases averted per 1000 people AB
file.copy(from = 'plots/cohorts_AB_CA_perpop_perennial.pdf',
          to = 'manuscript_figures/4_cohorts_AB_CA_perpop_perennial.pdf')

# Figure 5: Efficiency frontier
file.copy(from = 'plots/CASAbytotaldosesperennial.pdf',
          to = 'manuscript_figures/5_CASAbytotaldosesperennial.pdf')



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
file.copy(from = 'plots/plot_cumulCA_SVhybrid_none_seasonal.pdf',
          to = 'manuscript_figures/S1_plot_cumulCA_SVhybrid_none_seasonal.pdf')

# S2
file.copy(from = 'plots/plot_cumulCASAperpop_CU_perennial.pdf',
          to = 'manuscript_figures/S2_plot_cumulCASAperpop_CU_perennial.pdf')

# S3
file.copy(from = 'plots/plot_cumulCASA_CU_seasonal.pdf',
          to = 'manuscript_figures/S3_plot_cumulCASA_CU_seasonal.pdf')

# S4 
file.copy(from = 'plots/plot_cumulCASA_AB_seasonal.pdf',
          to = 'manuscript_figures/S4_plot_cumulCASA_AB_seasonal.pdf')

# S5
file.copy(from = 'plots/plot_cumulCASAperpop_AB_perennial.pdf',
          to = 'manuscript_figures/S5_plot_cumulCASAperpop_AB_perennial.pdf')

# S6
file.copy(from = 'plots/cohorts_AB_SA_perpop_perennial.pdf',
          to = 'manuscript_figures/S6_cohorts_AB_SA_perpop_perennial.pdf')

# S7
file.copy(from = 'plots/CASAbytotaldosesseasonal.pdf',
          to = 'manuscript_figures/S7_CASAbytotaldosesseasonal.pdf')

# Table S5
file.copy(from = 'plots/percent_cases.csv',
          to = 'manuscript_figures/TableS5_percent_cases.csv')

# Table s6
file.copy(from = 'plots/table_perc_outcomes_averted.csv',
          to = 'manuscript_figures/TableS6_table_perc_outcomes_averted.csv')

