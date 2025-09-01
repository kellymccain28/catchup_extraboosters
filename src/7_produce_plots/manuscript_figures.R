# Script to pull out manuscript figures from the saved plots and copy them to a new folder 
dir.create('manuscript_figures/')

# Figure 1: CA and SA for CU strategies 
file.copy(from = 'plots/plot_cumulCASA_CU_ADDDOSEperennial.tiff',
          to = str_glue('manuscript_figures/1_plot_cumulCASA_CU_additionaldoseperennial_', age_scaling, '.tiff'))

# Figure 2: Cases averted per 1000 people by age at vaccination and age (perennial) - from plots_mim_ammnet.R
# file.copy(from = 'plots/plot_cohorts_ageatvax_CAperpop_per.tiff', 
#           to = 'manuscript_figures/2_plot_cohorts_ageatvax_CAperpop_per.tiff')plot_cohorts_ageatvax_CAperpoptotals_per.tiff
file.copy(from = 'plots/plot_cohorts_ageatvax_CAperpoptotals_per.tiff', 
                    to = str_glue('manuscript_figures/2_plot_cohorts_ageatvax_CAperpoptotals_per_', age_scaling, '.tiff'))

# Figure 3: CA and SA for AB strategies 
file.copy(from = 'plots/plot_cumulCASA_AB_ADDDOSEperennial.tiff',
          to = str_glue('manuscript_figures/3_plot_cumulCASA_AB_additionaldoseperennial_', age_scaling, '.tiff'))

# Table 2:
file.copy(from = 'plots/outcomes_averted_CUorAB_perennial.csv',
          to = str_glue('manuscript_figures/Table2_outcomes_averted_CUorAB_perennial_', age_scaling, '.csv'))

# Table S5
file.copy(from = 'plots/outcomes_averted_combinedstrategies_perennial.csv',
          to = str_glue('manuscript_figures/TableS5_outcomes_averted_combinedstrategies_perennial_', age_scaling, '.csv'))

# Figure 4: Cohort view of cases averted per 1000 people AB
file.copy(from = 'plots/cohorts_AB_CA_perpop_perennial.tiff',
          to = str_glue('manuscript_figures/4_cohorts_AB_CA_perpop_perennial_', age_scaling, '.tiff'))

# Figure 5: Efficiency frontier
file.copy(from = 'plots/CASAbyadditionaldosesperennial.tiff',
          to = str_glue('manuscript_figures/5_CASAbyadditionaldosesperennial_', age_scaling, '.tiff'))



# Values in text are from:
# get_perc_U5.R --> percentage of cases and cases averted in U5s
# get_perc_dominated.R --> % of non-dominated scenarios that are CU, AB, etc.

file.copy(from = 'plots/cohorts_sum_ageatvaxperennial.csv',
          to = str_glue('manuscript_figures/cohorts_sum_ageatvax_perennial_', age_scaling, '.csv'))
file.copy(from = 'plots/cohorts_sum_ageatvaxseasonal.csv',
          to = str_glue('manuscript_figures/cohorts_sum_ageatvax_seasonal_', age_scaling, '.csv'))

# text after figure 1 : 
file.copy(from = 'plots/cohorts_CAperdose_catch-up no boosterperennial.csv',
          to = str_glue('manuscript_figures/cohorts_CAperdose_catch-up no boosterperennial_', age_scaling, '.csv'))
# have same file for a age-based
file.copy(from = 'plots/cohorts_CAperdose_age-basedperennial.csv',
          to = str_glue('manuscript_figures/cohorts_CAperdose_age-basedperennial_', age_scaling, '.csv'))


# SI Figures
# S1
file.copy(from = 'plots/plot_cumulCA_SVhybrid_none_seasonal.tiff',
          to = str_glue('manuscript_figures/S2_plot_cumulCA_SVhybrid_none_seasonal_', age_scaling, '.tiff'))

# S2
file.copy(from = 'plots/plot_cumulCASAperpop_CU_perennial.tiff',
          to = str_glue('manuscript_figures/S3_plot_cumulCASAperpop_CU_perennial_', age_scaling, '.tiff'))

# S3
file.copy(from = 'plots/plot_cumulCASA_CU_ADDDOSEseasonal.tiff',
          to = str_glue('manuscript_figures/S4_plot_cumulCASA_CU_additionaldoseseasonal_', age_scaling, '.tiff'))

# S4 
file.copy(from = 'plots/plot_cumulCASA_AB_ADDDOSEseasonal.tiff',
          to = str_glue('manuscript_figures/S5_plot_cumulCASA_AB_additionaldoseseasonal_', age_scaling, '.tiff'))

# S5
file.copy(from = 'plots/plot_cumulCASAperpop_AB_perennial.tiff',
          to = str_glue('manuscript_figures/S6_plot_cumulCASAperpop_AB_perennial_', age_scaling, '.tiff'))

# S6
file.copy(from = 'plots/cohorts_AB_SA_perpop_perennial.tiff',
          to = str_glue('manuscript_figures/S7_cohorts_AB_SA_perpop_perennial_', age_scaling, '.tiff'))

# S7
file.copy(from = 'plots/CASAbyadditionaldosesseasonal.tiff',
          to = str_glue('manuscript_figures/S8_CASAbyadditionaldosesseasonal_', age_scaling, '.tiff'))

# Table S4
file.copy(from = 'plots/percent_cases.csv',
          to = str_glue('manuscript_figures/TableS4_percent_cases_', age_scaling, '.csv'))

# Table s6
file.copy(from = 'plots/table_perc_outcomes_averted.csv',
          to = str_glue('manuscript_figures/TableS6_table_perc_outcomes_averted_', age_scaling, '.csv'))



# Now do same for pdfs #######################
# Figure 1: CA and SA for CU strategies 
file.copy(from = 'plots/plot_cumulCASA_CU_ADDDOSEperennial.pdf',
          to = str_glue('manuscript_figures/1_plot_cumulCASA_CU_additionaldoseperennial_', age_scaling, '.pdf'))

# Figure 2: Cases averted per 1000 people by age at vaccination and age (perennial) - from plots_mim_ammnet.R
# file.copy(from = 'plots/plot_cohorts_ageatvax_CAperpop_per.pdf', 
#           to = 'manuscript_figures/2_plot_cohorts_ageatvax_CAperpop_per.pdf')plot_cohorts_ageatvax_CAperpoptotals_per.pdf
file.copy(from = 'plots/plot_cohorts_ageatvax_CAperpoptotals_per.pdf', 
          to = str_glue('manuscript_figures/2_plot_cohorts_ageatvax_CAperpoptotals_per_', age_scaling, '.pdf'))

# Figure 3: CA and SA for AB strategies 
file.copy(from = 'plots/plot_cumulCASA_AB_ADDDOSEperennial.pdf',
          to = str_glue('manuscript_figures/3_plot_cumulCASA_AB_additionaldoseperennial_', age_scaling, '.pdf'))

# Figure 4: Cohort view of cases averted per 1000 people AB
file.copy(from = 'plots/cohorts_AB_CA_perpop_perennial.pdf',
          to = str_glue('manuscript_figures/4_cohorts_AB_CA_perpop_perennial_', age_scaling, '.pdf'))

# Figure 5: Efficiency frontier
file.copy(from = 'plots/CASAbyadditionaldosesperennial.pdf',
          to = str_glue('manuscript_figures/5_CASAbyadditionaldosesperennial_', age_scaling, '.pdf'))

# SI Figures
# S1
file.copy(from = 'plots/plot_cumulCA_SVhybrid_none_seasonal.pdf',
          to = str_glue('manuscript_figures/S2_plot_cumulCA_SVhybrid_none_seasonal_', age_scaling, '.pdf'))

# S2
file.copy(from = 'plots/plot_cumulCASAperpop_CU_perennial.pdf',
          to = str_glue('manuscript_figures/S3_plot_cumulCASAperpop_CU_perennial_', age_scaling, '.pdf'))

# S3
file.copy(from = 'plots/plot_cumulCASA_CU_ADDDOSEseasonal.pdf',
          to = str_glue('manuscript_figures/S4_plot_cumulCASA_CU_additionaldoseseasonal_', age_scaling, '.pdf'))

# S4 
file.copy(from = 'plots/plot_cumulCASA_AB_ADDDOSEseasonal.pdf',
          to = str_glue('manuscript_figures/S5_plot_cumulCASA_AB_additionaldoseseasonal_', age_scaling, '.pdf'))

# S5
file.copy(from = 'plots/plot_cumulCASAperpop_AB_perennial.pdf',
          to = str_glue('manuscript_figures/S6_plot_cumulCASAperpop_AB_perennial_', age_scaling, '.pdf'))

# S6
file.copy(from = 'plots/cohorts_AB_SA_perpop_perennial.pdf',
          to = str_glue('manuscript_figures/S7_cohorts_AB_SA_perpop_perennial_', age_scaling, '.pdf'))

# S7
file.copy(from = 'plots/CASAbyadditionaldosesseasonal.pdf',
          to = str_glue('manuscript_figures/S8_CASAbyadditionaldosesseasonal_', age_scaling, '.pdf'))
