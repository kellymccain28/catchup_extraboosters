**Modelling of catch-up vaccination and/or extra booster doses with R21/Matrix-M vaccine**

Code to  estimate the impact of expanding target age groups for vaccination with the R21/Matrix-M vaccine as seen in: McCain et al., Public health impact of catch-up vaccination or additional booster doses with pre-erythrocytic malaria vaccine R21/Matrix-M: a modelling study.

The workflow_all.R script will run each of the orderly2 tasks in the correct order. Each orderly task, with the exception of 2_calibration_to_PfPR, has parameters that indicate the scaling of the antibodies by age; the entire workflow was run twice - once with a scaling factor of 1, presented in the main results, and once with a scaling factor of 0.64, presented in the Supplementary Information. The provision.R script contains the packages required to run the analysis. Note that this analysis was run on the HPC in the Department of Infectious Disease Epidemiology at Imperial College. 

src/
    ├── 1_create_parameter_list/                           # Scripts to create a parameter set across all combinations of seasonality, PfPR, and interventions.
        ├── 1_create_parameter_list.R                      # Orderly task to produce the output 
        ├── efficacy_parameters_medians.csv                
        ├── efficacy_parameters.csv                        
        ├── generate_params.R                              # Script to generate malariasimulation parameters from scenarios
        ├── r21_malarisimulation_parameter_draws.csv
        ├── r21_malarisimulation_parameters.csv            # R21 parameters from Schmit, Topazian et al. 2024. 
        ├── scenarios_torun_R21.rds                        # Outputted scenarios 
        └── ssa_demography_2021.csv                        # Average SSA demography to age-scale population in simulation
    ├── 2_calibration_to_PfPR/                             # Scripts run once, to calibrate the model's starting EIR to desired PfPR
        ├── PrEIR/                    
            ├── EIRestimates.rds                           # Final EIR estimates for each of the baseline settings
        ├── 2_calibration_to_PfPR.R                        # Orderly task to calibrate the EIR to PfPR
        ├── baseline_parameters.rds                        # Malariasimulation parameters for the baseline settings
        └── eir_prev_matching.R                            # Function to match the EIR to PfPR
    ├── 3_run_process/                                     # Scripts to run and process individual malariasimulation runs
        ├── 3_run_process.R                                # Orderly task to run and process malariasimulation runs
        ├── add_doses.R                                    # Add dose variables for each run 
        ├── deaths_dalys.R                                 # Calculate deaths 
        ├── EIRestimates.rds                               # EIRestimates from task 2 used for runs
        ├── get_doses.R                                    # Summarize doses per time period 
        ├── get_prev.R                                     # Get prevalence per time period 
        ├── get_rates1.R                                   # Get clinical and severe incidence per time period
        ├── process_runs.R                                 # Wrapper function to do all processing
        ├── run_processing.R                               
        ├── run_simulation.R                               # Helper function to run the simulations
        └── workflow.R                                     # Helper function to send the runs to the HPC 
    ├── 4_combine_runs/                                    # Scripts to combine indvididual runs in groups of 500
        ├── 4_combine_runs.R                               # Orderly task to combine runs in groups of 500
        ├── completed_reports.R                            # Function to find locations of completed orderly reports 
        ├── parameters_torun_R21.rds                       # Parameters used in runs
        └── workflow.R                                     # Helper function to send tasks to HPC
    ├── 5_process_combined/                                # Scripts to pull in each of the combined runs and bind / process them
        ├── 5_process_combined.R                           # Orderly task to pull in each of the combined runs and bind / process them
        ├── add_agegrps.R                                  # Function to add age group definitions to data 
        ├── calc_cumul_cases.R                            
        ├── collapse_by_scenario.R                         # Function to get medians and 95% CrIs for each scenario
        ├── find_frontiers.R                               # Function to find scenarios on the efficiency frontier 
        ├── outcomes_averted.R                             # Function to calculate outcomes averted compared to baseline
        ├── process_combined.R                             # Wrapper function to do processing of combined runs
        ├── summ_over_time_horizon.R                
        └── workflow.R                                     # Helper function to send task to cluster
    ├── 6_make_cohorts/                                    # Scripts to create cohorts for each vaccination scenario
        ├── 6_make_cohorts.R                               # Orderly task to create cohorts
        ├── add_agegrps.R                                  # Add age groups to cohort dataset (same function as above)
        ├── calc_inci_pppy.R                                
        ├── cohorts_diagnostics.qmd      
        ├── collapse_by_scenario_cohorts.R                 # Function to get medians and 95% CrIs of each scenario in cohort data
        ├── get_cohort.R                                   # Function to filter datasets to cohort
        ├── outcomes_averted.R                             # Function to calculate outcomes averted compared to baseline
        └── workflow.R                                     # Helper function to send task to cluster
    └── 7_produce_plots/                                   # Scripts to produce plots for manuscript
        ├── 7_produce_plots.R                              # Orderly task to produce plots
        ├── add_labels.R                                   # Add plotting labels
        ├── get_eff_frontier_legend.R                      # Create legend for efficiency frontier plots 
        ├── get_perc_dominated.R                           # Function to get % of scenarios that are dominated 
        ├── get_perc_U5.R                                  # Function to get % of cases and severe cases in U5s
        ├── insert_blank_rows_latex.R                      # Function to create tables for LaTeX
        ├── legend.png                                     # Legend for efficiency frontier
        ├── manuscript_figures.R                           # Function to pull out only plots created for manuscript
        ├── plot_age_dist.R                                
        ├── plot_cohort.R                                  # Function to create cohort plots 
        ├── plot_cumul_CA.R                                # Function to create plots of cumulative cases 
        ├── plot_efficiency_frontier.R                     # Function to plot efficiency frontiers
        ├── plot_perc_averted.R                            # Function to plot percentage of cases averted
        ├── plot_themes.R                                  # Helper function to define plot themes
        ├── plots_mim_ammnet.R                             # Create plots of age at vaccination and cohorts
        └── table_CA_perreldose.R                          # Make tables 
provision.R                                                # Packages required for analysis 
workflow_all.R                                             # Workflow, running through each orderly task in order
