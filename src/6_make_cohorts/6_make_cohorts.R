# Script to make various cohorts

# Set up task  ------------------------------------------------------------
library(dplyr)
library(orderly2)
library(data.table)
library(janitor)
library(purrr)
library(tidyr)
library(stringr)

orderly_strict_mode()
orderly2::orderly_description('Create cohorts from processed ageyr dataset')

# Set parameters for task
orderly_parameters(analysis = NULL)

orderly2::orderly_dependency("5_process_combined",
                             "latest()",
                             c(output_ageyr_toage50_intermediate.rds = 'output_ageyr_toage50_intermediate.rds'#,
                               # summarized_ageyr_draws.rds = "summarized_ageyr_draws.rds"
                               ))

# Outputs for task
orderly_artefact(
  'Produces a dataset with cohorts for age-based, 6m-2y, 6m-14y, and none',
  c('cohorts_byage.rds',
    "cohorts_byage_draws.rds",
    # "cohorts_rawdraws.rds",
    'cohorts.rds',
    'cohorts_ageatvax_draws.rds',
    'cohorts_ageatvax.rds',
    'cohorts_ageatvaxandage_draws.rds',
    'cohorts_ageatvaxandage.rds'
  )
)

# Set resources
orderly_resource(
  c('get_cohort.R',
    'collapse_by_scenario_cohorts.R',
    'calc_inci_pppy.R',
    'outcomes_averted.R',
    'add_labels.R')
)


# Functions to source
source('get_cohort.R')
source('collapse_by_scenario_cohorts.R')
source('calc_inci_pppy.R')
source('outcomes_averted.R')
source('add_labels.R')


ageyr <- readRDS('output_ageyr_toage50_intermediate.rds') %>%
  mutate(totaldoses = rowSums(across(starts_with('dose'))),
         massdoses = rowSums(across(starts_with('n_pev_mass'))),
         EPIdoses = rowSums(across(starts_with('n_pev_epi'))))

# example <- ageyr %>%
#   filter((PEVage == '6m-4y' & EPIextra == '-') | PEVstrategy == 'none') %>%
#   filter(!(age_lower == 0 & age_upper == 5) & !(age_lower == 5 & age_upper == 10))
# saveRDS(example, 'ageyrto50_6m4y_ex.rds')

message('Got ageyr df')
message(names(ageyr))

#### Get cohorts
# years over which to loop
yrs <- seq(min(ageyr$t), max(ageyr$t))

# initialize empty data frame for age-based
ABcohorts <- data.frame()

for (i in yrs){
  ABcohort <- get_cohort(df = ageyr %>% filter(PEVstrategy == 'AB' | PEVstrategy == 'none') %>%
                           filter(!(age_lower == 0 & age_upper == 5) & !(age_lower == 5 & age_upper == 10)),
                         time1 = i,
                         minage = 0.5,
                         maxage = 1) %>%
    mutate(strategy = ifelse(PEVstrategy == 'AB' & EPIextra == '-', 'AB',
                             ifelse(PEVstrategy == 'AB' & EPIextra == '5y', 'AB - 5y',
                                    ifelse(PEVstrategy == 'AB' & EPIextra == '10y', 'AB - 10y',
                                           ifelse(PEVstrategy == 'AB' & EPIextra == '5y+10y', 'AB - 5y+10y','noneAB'))))) %>%
    outcomes_averted() %>%
    filter(PEVstrategy != 'none')
  ABcohorts <- rbind(ABcohorts, ABcohort)
  message(paste0('finished', i))
}
message('finished ABcohort')

CU6m2y <- get_cohort(df = ageyr %>% filter((PEVage == '6m-2y' & EPIextra == '-') | PEVstrategy == 'none') %>%
                       filter(!(age_lower == 0 & age_upper == 5) & !(age_lower == 5 & age_upper == 10)), # PEVstrategy == 'none'
                     time1 = 1, # CU campaigns will only be vaccinated at time = 1
                     minage = 0.5,
                     maxage = 3) %>%
  mutate(strategy = ifelse(PEVstrategy == 'none', 'none', 'CU 6m-2y')) %>%
  outcomes_averted() %>%
  filter(PEVstrategy != 'none')
message('finished CU 6m2y cohort')

CU6m4y <- get_cohort(df = ageyr %>% filter((PEVage == '6m-4y' & EPIextra == '-') | PEVstrategy == 'none') %>%
                       filter(!(age_lower == 0 & age_upper == 5) & !(age_lower == 5 & age_upper == 10)),
                     time1 = 1, # CU campaigns will only be vaccinated at time = 1
                     minage = 0.5,
                     maxage = 5) %>%
  mutate(strategy = ifelse(PEVstrategy == 'none', 'none', 'CU 6m-4y'))%>%
  outcomes_averted() %>%
  filter(PEVstrategy != 'none')
message('finished CU 6m4y cohort')

CU6m9y <- get_cohort(df = ageyr %>% filter((PEVage == '6m-9y' & EPIextra == '-') | PEVstrategy == 'none') %>%
                       filter(!(age_lower == 0 & age_upper == 5) & !(age_lower == 5 & age_upper == 10)),
                     time1 = 1, # CU campaigns will only be vaccinated at time = 1
                     minage = 0.5,
                     maxage = 10) %>%
  mutate(strategy = ifelse(PEVstrategy == 'none', 'none', 'CU 6m-9y'))%>%
  outcomes_averted() %>%
  filter(PEVstrategy != 'none')
message('finished CU 6m9y cohort')

CU6m14y <- get_cohort(df = ageyr %>% filter((PEVage == '6m-14y' & EPIextra == '-')  | PEVstrategy == 'none') %>%
                        filter(!(age_lower == 0 & age_upper == 5) & !(age_lower == 5 & age_upper == 10)),
                      time1 = 1, # CU campaigns will only be vaccinated at time = 1
                      minage = 0.5,
                      maxage = 15) %>%
  mutate(strategy = ifelse(PEVstrategy == 'none', 'none6m14y', 'CU 6m-14y'))%>%
  outcomes_averted()
# keeping the 'none' here because it'll cover all the other catch-up cohorts
message('finished CU 6m14y cohort')

CU5y9y <- get_cohort(df = ageyr %>% filter((PEVage == '5-9' & EPIextra == '-') | PEVstrategy == 'none') %>%
                       filter(!(age_lower == 0 & age_upper == 5) & !(age_lower == 5 & age_upper == 10)),
                     time1 = 1, # CU campaigns will only be vaccinated at time = 1
                     minage = 5,
                     maxage = 10) %>%
  mutate(strategy = ifelse(PEVstrategy == 'none', 'none', 'CU 5-9y'))%>%
  outcomes_averted() %>%
  filter(PEVstrategy != 'none')
message('finished CU 5-9y cohort')

CU5y14y <- get_cohort(df = ageyr %>% filter((PEVage == '5-14' & EPIextra == '-') | PEVstrategy == 'none') %>%
                        filter(!(age_lower == 0 & age_upper == 5) & !(age_lower == 5 & age_upper == 10)),
                      time1 = 1, # CU campaigns will only be vaccinated at time = 1
                      minage = 5,
                      maxage = 15) %>%
  mutate(strategy = ifelse(PEVstrategy == 'none', 'none', 'CU 5-14y'))%>%
  outcomes_averted() %>%
  filter(PEVstrategy != 'none')
message('finished CU 5-14y cohort')

cohorts_rawdraws <- rbind(CU6m14y, CU6m2y, CU6m4y, CU6m9y, CU5y9y, CU5y14y, ABcohorts) %>%
  # distinct just in case there are some duplicates of the 'none' group
  distinct() %>%
  select(-starts_with('p_'),
         -contains('diff'),-clinical, -severe, -mortality,
         -yll, -yld, -daly, -contains('pop'), - contains('FVC'),
         -prevalence_2_10, -prevalence_0_100, -prop_n,
         -ends_with('perdose')
         ) %>%
  add_labels()


saveRDS(cohorts_rawdraws, "cohorts_rawdraws.rds")
# cohorts_rawdraws <- readRDS('R:/Kelly/catchup_extraboosters/archive/6_make_cohorts/20241019-135953-3faba4fe/cohorts_rawdraws.rds')
# saveRDS(cohorts_rawdraws, 'cohorts_rawdraws.rds')
# function to get EPi doses for a cohort
# for each timestep (which is in half years)
get_epi <- function(df){
  df <- df %>%
    rowwise() %>%
    mutate(epiprimary = case_when(t == 1 & age_lower == 0.5 & age_upper == 1 & PEVage != '5-9' & PEVage != '5-14' ~
                                    sum(n_pev_epi_dose_1, n_pev_epi_dose_2, n_pev_epi_dose_3, na.rm = TRUE),
                                  t == 1 & age_lower == 5.0 & age_upper == 5.5 & (PEVage == '5-9' | PEVage == '5-14') ~
                                    sum(n_pev_epi_dose_1, n_pev_epi_dose_2, n_pev_epi_dose_3, na.rm = TRUE), # want to count the EPI doses for 5-9 and 5-14 cohorts in time 1
                                  TRUE ~ NA),
           epibooster = case_when(t == 3 & age_lower == 1.5 & age_upper == 2 & PEVage != '5-9' & PEVage != '5-14'~ n_pev_epi_booster_1, # all first doses for cohorts that don't have catch-ups to older kids
                                  t == 3 & age_lower == 6 & age_upper == 6.5 & (PEVage == '5-9' | PEVage == '5-14')~ n_pev_epi_booster_1, # first doses for cohorts with CU to oldre kids (ages are different)
                                  # Second boosters
                                  t == 5 & EPIextra == '2y'& age_lower == 2.5 & age_upper == 3 & PEVage == '-' ~ n_pev_epi_booster_2, 
                                  t == 11 & EPIextra == '5y' & age_lower == 5.5 & age_upper == 6 & PEVage == '-' ~ n_pev_epi_booster_2,
                                  t == 21 & EPIextra == '10y'& age_lower == 10.5 & age_upper == 11 & PEVage == '-' ~ n_pev_epi_booster_2,
                                  t == 5 & EPIextra == '2y+5y'& age_lower == 2.5 & age_upper == 3 & PEVage == '-' ~ n_pev_epi_booster_2, # 1st extra booster at 2 years
                                  t == 5 & EPIextra == '2y+10y'& age_lower == 2.5 & age_upper == 3 & PEVage == '-' ~ n_pev_epi_booster_2, # 1st extra booster at 2 years
                                  t == 11 & EPIextra == '5y+10y'& age_lower == 5.5 & age_upper == 6 & PEVage == '-' ~ n_pev_epi_booster_2, # 1st extra booster at 5 years
                                  t == 5 & EPIextra == '2y+5y+10y'& age_lower == 2.5 & age_upper == 3 & PEVage == '-' ~ n_pev_epi_booster_2, # first extra booster at 2 years
                                  # Third boosters
                                  t == 11 & EPIextra == '2y+5y' & age_lower == 5.5 & age_upper == 6 & PEVage == '-' ~ n_pev_epi_booster_3, # second extra booster at 5 years
                                  t == 21 & EPIextra == '2y+10y' & age_lower == 10.5 & age_upper == 11 & PEVage == '-' ~ n_pev_epi_booster_3, # second extra booster at 10 years
                                  t == 21 & EPIextra == '5y+10y' & age_lower == 10.5 & age_upper == 11 & PEVage == '-' ~ n_pev_epi_booster_3, # second extra booster at 10 years
                                  t == 11 & EPIextra == '2y+5y+10y' & age_lower == 5.5 & age_upper == 6 & PEVage == '-' ~ n_pev_epi_booster_3, # second extra booster at 5 years
                                  # Fourth boosters
                                  t == 21 & EPIextra == '2y+5y+10y' & age_lower == 10.5 & age_upper == 11 & PEVage == '-' ~ n_pev_epi_booster_4, # third extra booster at 10 years
                                  TRUE ~ NA)) %>% # there are no catch-up cohorts with extra boosters
    ungroup()
}

get_mass <- function(df){
  df <- df %>%
    rowwise() %>%
    mutate(mass = ifelse(t == 1 & age_lower == 0.5 & age_upper == 1 & !(PEVage %in% c('5-9', '5-14')),
                         sum(n_pev_mass_dose_1, n_pev_mass_dose_2, n_pev_mass_dose_3, n_pev_mass_booster_1, na.rm = TRUE),
                         ifelse(t == 1 & (PEVage == '5-9' | PEVage == '5-14') & age_lower == 5 & age_upper == 5.5,
                                sum(n_pev_mass_dose_1, n_pev_mass_dose_2, n_pev_mass_dose_3, n_pev_mass_booster_1, na.rm = TRUE), 0))) %>%
    ungroup()
}

# First need to condense 60 cohorts to 1
ab_condensed <- cohorts_rawdraws %>%
  filter(PEVstrategy %in% c('AB', 'none')) %>%
  # Calculate doses
  get_epi() %>%
  # First condense to 1 'cohort' - grouping by age and strategy and get median of everything across 60 cohorts
  group_by(age_lower, age_upper, ID, drawID, strategy,
           int_ID, PEV, PEVcov, PEVstrategy, PEVage, PEVrounds,
           EPIbooster, EPIextra, massbooster_rep, MDA, pfpr, seasonality,
           labels, label_int, strategytype, EPIextra_labels, scen_labels) %>%
  mutate_at(vars(cases, sevcases, deaths,
                    n, epiprimary, epibooster,
                    contains('averted')),
               median, na.rm = TRUE) %>%
  mutate(mass = 0) %>%
  distinct(age_lower, age_upper, ID, drawID, strategy,
           int_ID, PEV, PEVcov, PEVstrategy, PEVage, PEVrounds,
           EPIbooster, EPIextra, massbooster_rep, MDA, pfpr, seasonality,
           labels, label_int, strategytype, EPIextra_labels, scen_labels, .keep_all = TRUE)
saveRDS(ab_condensed, 'ab_condensed.rds')

# Now for CU cohorts
CU_only <- cohorts_rawdraws %>%
  get_mass() %>%
  get_epi() %>%
  # Filter to just CU
  filter(PEVstrategy == 'catch-up')

cohorts_rawdraws2 <- rbind(CU_only, ab_condensed)


# cohorts_rawdraws2 <- readRDS("R:/Kelly/catchupR21-lite2/archive/6_make_cohorts/20240714-165510-95d42754/cohorts_rawdraws2.rds")
# saveRDS(cohorts_rawdraws2, 'cohorts_rawdraws2.rds')
##########################################################
# get overall values for cohorts
allcohorts_draws <- cohorts_rawdraws2 %>%
  select(t, ID, drawID, strategy,
         int_ID, PEV, PEVcov, PEVstrategy, PEVage, PEVrounds,
         EPIbooster, EPIextra, massbooster_rep, MDA, pfpr, seasonality,
         labels, label_int, strategytype, EPIextra_labels, scen_labels,
         cases, sevcases, deaths, 
         epiprimary, epibooster, mass, n, totaldoses,
         cases_averted, deaths_averted, severe_averted) %>%
  # Get sum of n in each cohort over all ages in cohort (i.e. 6m-2y would have multiple age groups that we want to sum up)
  group_by(t, ID, drawID, strategy, 
           int_ID, PEV, PEVcov, PEVstrategy, PEVage, PEVrounds,
           EPIbooster, EPIextra, massbooster_rep, MDA, pfpr, seasonality,
           labels, label_int, strategytype, EPIextra_labels, scen_labels) %>%
  summarize_at(vars(cases, sevcases, deaths,
                 epiprimary, epibooster, mass, n,
                 contains('averted')), 
            sum, na.rm = TRUE) %>% ungroup() %>%
  # get overall values by grouping by strategy only
  group_by(ID, drawID, strategy,
           int_ID, PEV, PEVcov, PEVstrategy, PEVage, PEVrounds,
           EPIbooster, EPIextra, massbooster_rep, MDA, pfpr, seasonality,
           labels, label_int, strategytype, EPIextra_labels, scen_labels) %>%
  mutate_at(vars(cases, sevcases, deaths,
                 epiprimary, epibooster, mass,
                 contains('averted')),
            sum, na.rm = TRUE) %>%
  # get mean population over all time periods, grouped by strategy
  mutate(n = mean(n)) %>%
  rowwise() %>%
  mutate(t = 'overall',
         totaldoses = sum(epiprimary, epibooster, mass, na.rm = TRUE)) %>%
  mutate(cases_averted_perpop = cases_averted / n * 1000,
         cases_averted_perdose= cases_averted / totaldoses * 1000,
         severe_averted_perpop = severe_averted / n * 1000,
         severe_averted_perdose = severe_averted / totaldoses * 1000) %>%
  # Get cases per population
  mutate(cases_per1000pop = cases / n * 1000,
         sevcases_per1000pop = sevcases / n * 1000) %>%
  distinct()

saveRDS(allcohorts_draws, 'cohorts_draws.rds')

allcohorts <- allcohorts_draws %>%
  collapse_by_scenario_cohorts(by = 'overall')
saveRDS(allcohorts, "cohorts.rds")
##########################################################


##########################################################
# Summarize over cohorts by age and t -- used for age distribution plot; to account for the age/time dynamics associated with immunity
cohorts_byage_draws <- cohorts_rawdraws2 %>% 
  select(t, ID, drawID, strategy, age_grp, age_lower, age_upper,
         int_ID, PEV, PEVcov, PEVstrategy, PEVage, PEVrounds,
         EPIbooster, EPIextra, massbooster_rep, MDA, pfpr, seasonality,
         labels, label_int, strategytype, EPIextra_labels, scen_labels,
         cases, sevcases, deaths, n,
         cases_averted, deaths_averted, severe_averted) %>%
  mutate(age_lower_group = ceiling(age_lower)) %>%#ifelse(age_lower != '0.5', floor(age_lower), age_lower)) %>%
  group_by(age_lower_group) %>%
  mutate(age_grp = paste0(first(age_lower), '-', last(age_upper)),
         age_grp = factor(age_grp, levels = c('0.5-1.5', '1.5-2.5', '2.5-3.5', '3.5-4.5', '4.5-5.5', '5.5-6.5', '6.5-7.5', '7.5-8.5',
                                              '8.5-9.5', '9.5-10.5', '10.5-11.5', '11.5-12.5', '12.5-13.5', '13.5-14.5', '14.5-15.5',
                                              '15.5-16.5', '16.5-17.5', '17.5-18.5', '18.5-19.5', '19.5-20.5', '20.5-21.5', '21.5-22.5',
                                              '22.5-23.5', '23.5-24.5', '24.5-25.5', '25.5-26.5', '26.5-27.5', '27.5-28.5', '28.5-29.5',
                                              '29.5-30.5', '30.5-31.5', '31.5-32.5', '32.5-33.5', '33.5-34.5', '34.5-35.5', '35.5-36.5',
                                              '36.5-37.5', '37.5-38.5', '38.5-39.5', '39.5-40.5', '40.5-41.5', '41.5-42.5', '42.5-43.5',
                                              '43.5-44.5', '44.5-45.5', '45.5-46.5', '46.5-47.5', '47.5-48.5', '48.5-49.5', '49.5-50.5'))) %>%
  # group by 1 year timesteps
  mutate(t = ceiling(t/2)) %>%
  # First summarize by age group/scenario and time and half year age groups to get 1 mean value n and sum cases 
  # for each 1y t, 6month age group (age_lower and age_upper are half year still)
  group_by(t, ID, drawID, strategy, age_grp, age_lower, age_upper,
           int_ID, PEV, PEVcov, PEVstrategy, PEVage, PEVrounds,
           EPIbooster, EPIextra, massbooster_rep, MDA, pfpr, seasonality,
           labels, label_int, strategytype, EPIextra_labels, scen_labels) %>%
  mutate_at(vars(cases, sevcases, deaths,
                 contains('averted')),
            sum, na.rm = TRUE) %>%
  mutate(n = mean(n)) %>% 
  distinct() %>%
  # then group by 1 year age groups and 1 year timesteps to get sum over simulation
  group_by(t, ID, drawID, strategy, age_grp, 
           int_ID, PEV, PEVcov, PEVstrategy, PEVage, PEVrounds,
           EPIbooster, EPIextra, massbooster_rep, MDA, pfpr, seasonality,
           labels, label_int, strategytype, EPIextra_labels, scen_labels) %>%
  # sum of outcomes over simulation for each age group
  summarize_at(vars(cases, sevcases, deaths,
                    contains('averted'), n),
               sum, na.rm = TRUE) %>%
  # mutate(t = 'overall') %>%
  rowwise() %>%
  # Calculate outcomes averted per pop and per dose
  mutate(cases_averted_perpop = cases_averted / n * 1000,
         severe_averted_perpop = severe_averted / n * 1000) %>%
  # Get cases per population
  mutate(cases_per1000pop = cases / n * 1000,
         sevcases_per1000pop = sevcases / n * 1000) %>%
  mutate(age_lower = factor(str_split_fixed(age_grp, "-", 2)[, 1],
                            levels = c('0.5', '1.5', '2.5', '3.5', '4.5', '5.5', '6.5', '7.5', '8.5', '9.5',
                                       '10.5', '11.5', '12.5', '13.5', '14.5', '15.5', '16.5', '17.5', '18.5', '19.5',
                                       '20.5', '21.5', '22.5', '23.5', '24.5', '25.5', '26.5', '27.5', '28.5', '29.5',
                                       '30.5', '31.5', '32.5', '33.5', '34.5', '35.5')),
         age_upper = str_split_fixed(age_grp, "-", 2)[, 2]) %>%
  distinct()

saveRDS(cohorts_byage_draws, "cohorts_byage_draws.rds")

# All cohorts by age, collapsed by scenario
cohorts_byage <- cohorts_byage_draws %>%
  collapse_by_scenario_cohorts(by = 'age') 
saveRDS(cohorts_byage, "cohorts_byage.rds")
##########################################################


##########################################################
# Cohorts for grouped by ageatvax

# now we summarize, grouping by age at vaccination
cohorts_ageatvax_draws <- cohorts_rawdraws2 %>%
  rowwise() %>%
  mutate(totaldoses = sum(epiprimary, epibooster, mass, na.rm = TRUE)) %>%
  # Before creating 1 year age at vaccination, have to summarized over age groups by half year age at vaccination 
  # Group by ageatvax (and strategy) only to get total over whole cohort life course  
  group_by(ID, drawID, ageatvax, strategy,
           int_ID, PEV, PEVcov, PEVstrategy, PEVage, PEVrounds,
           EPIbooster, EPIextra, massbooster_rep, MDA, pfpr, seasonality,
           labels, label_int, strategytype, EPIextra_labels, scen_labels) %>%
  mutate_at(vars(cases, sevcases, deaths,
                 totaldoses, contains('averted')),
            sum, na.rm = TRUE) %>%
  mutate(n = mean(n)) %>%
  mutate(ageatvax_lower = as.numeric(stringr::str_split_fixed(ageatvax, "-", 2)[, 1]),
         ageatvax_lower_group = ifelse(ageatvax_lower != '0.5', floor(ageatvax_lower), ageatvax_lower),
         ageatvax_upper = stringr::str_split_fixed(ageatvax, "-", 2)[, 2]) %>%
  ungroup() %>% 
  group_by(ageatvax_lower_group, PEVage) %>%
  mutate(ageatvax = paste0(first(ageatvax_lower), '-', max(ageatvax_upper)),
         ageatvax = factor(ageatvax, levels = c('0.5-1','1-2','2-3','3-4','4-5','5-6','6-7','7-8','8-9','9-10','10-11','11-12','12-13',
                                                '13-14','14-15'))) %>%
  # Select only relevant variables
  select(ID, drawID, strategy, ageatvax,
         int_ID, PEV, PEVcov, PEVstrategy, PEVage, PEVrounds,
         EPIbooster, EPIextra, massbooster_rep, MDA, pfpr, seasonality,
         labels, label_int, strategytype, EPIextra_labels, scen_labels,
         cases, sevcases, deaths, n, totaldoses,
         cases_averted, deaths_averted, severe_averted) %>%
  # Group by 1 year ageatvax (and strategy) only to get total over whole cohort life course  
  group_by(ID, drawID, ageatvax, strategy,
           int_ID, PEV, PEVcov, PEVstrategy, PEVage, PEVrounds,
           EPIbooster, EPIextra, massbooster_rep, MDA, pfpr, seasonality,
           labels, label_int, strategytype, EPIextra_labels, scen_labels) %>%
  mutate_at(vars(cases, sevcases, deaths, n,
                 totaldoses, contains('averted')),
            sum, na.rm = TRUE) %>%
  ungroup() %>%
  mutate(t = 'overall') %>%
  distinct() %>%
  rowwise() %>%
  # Calculate cases averted per population and per doses
  mutate(cases_averted_perpop = cases_averted / n * 1000,
         # cases_averted_perdose= cases_averted / totaldoses * 1000,
         severe_averted_perpop = severe_averted / n * 1000,
         # severe_averted_perdose = severe_averted / totaldoses * 1000
         ) %>%
  # Get cases per 1000 people
  mutate(cases_per1000pop = cases / n * 1000,
         sevcases_per1000pop = sevcases / n * 1000)


saveRDS(cohorts_ageatvax_draws, "cohorts_ageatvax_draws.rds")

# All cohorts by age, collapsed by scenario
cohorts_ageatvax <- cohorts_ageatvax_draws  %>%
  collapse_by_scenario_cohorts(by = 'ageatvax')
saveRDS(cohorts_ageatvax, "cohorts_ageatvax.rds")
##########################################################



##########################################################
# Cohorts for CU grouped by ageatvax and age
cohorts_ageatvaxandage_draws <- cohorts_rawdraws2 %>%
  # group by 1 year timesteps
  mutate(totaldoses = sum(epiprimary, epibooster, mass, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(age_lower_group = ifelse(age_lower != '0.5', floor(age_lower), age_lower)) %>%
  group_by(age_lower_group) %>%
  mutate(age_grp = paste0(first(age_lower), '-', last(age_upper)),
         age_grp = factor(age_grp, 
                          levels = c('0.5-1','1-2','2-3','3-4','4-5','5-6','6-7','7-8','8-9','9-10','10-11','11-12','12-13',
                                              '13-14','14-15','15-16','16-17','17-18','18-19','19-20','20-21','21-22','22-23','23-24',
                                              '24-25','25-26','26-27','27-28','28-29','29-30','30-31','31-32','32-33','33-34','34-35',
                                              '35-36','36-37','37-38','38-39','39-40','40-41','41-42','42-43','43-44','44-45','45-46'))) %>%
  mutate(ageatvax_lower = as.numeric(stringr::str_split_fixed(ageatvax, "-", 2)[, 1]),
         ageatvax_lower_group = ifelse(age_lower != '0.5', floor(ageatvax_lower), ageatvax_lower),
         ageatvax_upper = stringr::str_split_fixed(ageatvax, "-", 2)[, 2]) %>%
  ungroup() %>% 
  group_by(ageatvax_lower_group, PEVage) %>%
  mutate(ageatvax = paste0(first(ageatvax_lower), '-', last(ageatvax_upper)),
         ageatvax = factor(ageatvax, levels = c('0.5-1','1-2','2-3','3-4','4-5','5-6','6-7','7-8','8-9','9-10','10-11','11-12','12-13',
                                                '13-14','14-15'))) %>%
  ungroup() %>%
  select(t, ID, drawID, strategy, age_grp, ageatvax, age_lower, age_upper,
         int_ID, PEV, PEVcov, PEVstrategy, PEVage, PEVrounds,
         EPIbooster, EPIextra, massbooster_rep, MDA, pfpr, seasonality,
         labels, label_int, strategytype, EPIextra_labels, scen_labels,
         cases, sevcases, deaths, n, totaldoses,
         cases_averted, deaths_averted, severe_averted) %>%
  # Because have now combined 2 half-year age groups into new 1 year age groups, need to get sum of everything over these 2 age groups
  # First summarize by 6 month age group/scenario (not t)
  mutate(t = 'overall') %>%
  group_by(ID, drawID, strategy, ageatvax, age_grp, 
           age_lower, age_upper, 
           int_ID, PEV, PEVcov, PEVstrategy, PEVage, PEVrounds,
           EPIbooster, EPIextra, massbooster_rep, MDA, pfpr, seasonality,
           labels, label_int, strategytype, EPIextra_labels, scen_labels) %>%
  # sum of all variables per new 1 year age group and 1 year age at vaccination
  summarize_at(vars(cases, sevcases, deaths,
                 totaldoses, n,
                 contains('averted')),
            sum, na.rm = TRUE) %>%
  # Now, summarizing by 1 year age group and 1 year age at vax (not age_lower, age_upper -- which are by 6m)
  group_by(ID, drawID, strategy, ageatvax, age_grp, 
           int_ID, PEV, PEVcov, PEVstrategy, PEVage, PEVrounds,
           EPIbooster, EPIextra, massbooster_rep, MDA, pfpr, seasonality,
           labels, label_int, strategytype, EPIextra_labels, scen_labels) %>%
  # sum of all variables per new 1 year age group and 1 year age at vaccination
  summarize_at(vars(cases, sevcases, deaths,
                 totaldoses, n,
                 contains('averted')),
            sum, na.rm = TRUE) %>%
  distinct() %>%
  mutate(t = 'overall') %>%
  # Add back in age lower and age upper 
  mutate(age_lower = str_split_fixed(age_grp, "-", 2)[, 1],
         age_upper = str_split_fixed(age_grp, "-", 2)[, 2]) %>%
  rowwise() %>%
  # Calculate cases averted per population and per doses
  mutate(cases_averted_perpop = cases_averted / n * 1000,
         severe_averted_perpop = severe_averted / n * 1000) %>%
  # Get cases per population
  mutate(cases_per1000pop = cases / n * 1000,
         sevcases_per1000pop = sevcases / n * 1000) 

saveRDS(cohorts_ageatvaxandage_draws, "cohorts_ageatvaxandage_draws.rds")

# All cohorts by age, collapsed by scenario
cohorts_ageatvaxandage <- cohorts_ageatvaxandage_draws %>%
  collapse_by_scenario_cohorts(by = 'ageatvaxandage')  
saveRDS(cohorts_ageatvaxandage, "cohorts_ageatvaxandage.rds")
##########################################################