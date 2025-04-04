# Function to calculate outcomes averted 

#' @param df name of data frame for which we want to calculate outcomes averted
#' @param aggregation aggregation level (overall or ageyr)
outcomes_averted <- function(df){
  
  joinvars <- c('ID', 'drawID', 'age_lower','age_upper', 'halfyear')
  baseline_vars <- c('scenario', 'ID', 'drawID', 'age_lower', 'age_upper','dose3',
                     # 'daly_baseline', 
                     'cases_baseline', 'severe_baseline', 'deaths_baseline', 'clinical_baseline',
                     'halfyear')
                     #, 't')
  
  baseline <- df |>
    ungroup() |>
    filter(PEV == 'none') |> # filter out interventions
    rename(#daly_baseline = daly,
           cases_baseline = cases,
           severe_baseline = sevcases,
           deaths_baseline = deaths,
           clinical_baseline = clinical) |>
    select(all_of(baseline_vars)) |> distinct()
  
  base_IDs <- unique(baseline$scenario)
  
  noneaverted <- df |> # filter(!(scenario %in% base_IDs)) |>
    left_join(baseline |> select(-scenario), by = joinvars) |>
    mutate(#dalys_averted = daly_baseline - daly, 
           cases_averted = cases_baseline - cases,
           deaths_averted = deaths_baseline - deaths, 
           severe_averted = severe_baseline - sevcases,
           clinical_diff = clinical_baseline - clinical, # cases/pop difference between vaccinated and not vaccinated
           cases_averted_clin = clinical_diff * person_days) # total number of cases averted 
  
  averted <- noneaverted |>
    mutate(totaldoses = rowSums(across(starts_with('dose'))),
           massdoses = rowSums(across(starts_with('n_pev_mass'))),
           EPIdoses = rowSums(across(starts_with('n_pev_epi')))) %>% 
    # Get new variable that has cases averted in the standard AB strategy for each of the year, pfpr, seasonality settings + drawID and age group
    group_by(halfyear, pfpr, seasonality, age_lower, age_upper, drawID) %>%
    rowwise() %>%
    ungroup() %>%
    mutate(across(cases_averted, 
                  ~ .x / totaldoses * 1000, .names = "{.col}_perdose"), # per total dose
           across(cases_averted, 
                  ~ .x / dose3 * 1000, .names = "{.col}_perFVC"), # per FVC
           across(cases_averted, 
                  ~ .x / n * 1000, .names = "{.col}_perpop")) |> # cases averted per pop should be equal to clinical_diff (?)
    mutate(across(severe_averted, 
                  ~ .x / totaldoses * 1000, .names = "{.col}_perdose"),
           across(severe_averted, 
                  ~ .x / dose3 * 1000, .names = "{.col}_perFVC"),
           across(severe_averted, 
                  ~ .x / n * 1000, .names = "{.col}_perpop")) |>
    mutate(across(deaths_averted, 
                  ~ .x / totaldoses * 1000, .names = "{.col}_perdose"),
           across(deaths_averted, 
                  ~ .x / dose3 * 1000, .names = "{.col}_perFVC"),
           across(deaths_averted, 
                  ~ .x / n * 1000, .names = "{.col}_perpop")) |>
    # Below is the same as clinical, severe, mortality 
    mutate(across(c(cases, sevcases, deaths),
                  ~ .x / n, .names = "{.col}_perpop")) |> # per person
    # Convert NaN's coming from the calculations above to NAs
    mutate(across(c(contains('averted'), cases_perpop, sevcases_perpop, deaths_perpop),
                  ~ ifelse(is.nan(.x), NA, .x))) |>
    mutate(across(c(contains('averted'), cases_perpop, sevcases_perpop, deaths_perpop),
                  ~ ifelse(is.infinite(.x), NA, .x))) |>
    # get percent averted 
    mutate(p_CA = cases_averted / cases_baseline,
           p_SA = severe_averted / severe_baseline,
           p_clin_diff = clinical_diff / clinical_baseline) %>%
    select(-contains('baseline'))
    
}