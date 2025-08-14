# Expected loss calculation 
# 
library(dampack)
orderly_dependency(name = '5_process_combined',
                   "latest()",
                   files = c('summarized_overall_draws'))

df <- readRDS("summarized_overall_draws.R")

expected_loss_icer <- function(df, seas, pfprval){
  # Convert the averted dataset to this format (summarized_draws)
  df2 <- df %>%
    filter(age_lower == 0 & age_upper == 100) %>%
    filter(pfpr == pfprval & seasonality == seas) %>%
    filter(PEV != 'none' & !(PEVage == '-' & EPIextra == '-')) %>%
    mutate(idstrategy = paste0(PEVstrategy, "  ", PEVage, ", ", EPIextra))
  
  params <- data.frame(
    placeholder = seq(1,50,1 ) # the function to convert to the PSA object requires parameters
  )
  
  effects <- df2 %>% 
    mutate(cases_averted_routine_perpop = cases_averted_routine / n) %>%
    select(drawID, idstrategy, cases_averted_routine_perpop) %>%
    pivot_wider(names_from = idstrategy, 
                values_from = cases_averted_routine_perpop) %>%
    select(-drawID)
  
  dosenums <- df2 %>%
    mutate(additional_doses_perpop = additional_doses / n) %>%
    select(drawID, idstrategy, additional_doses_perpop) %>%
    pivot_wider(names_from = idstrategy, 
                values_from = additional_doses_perpop) %>% 
    select(-drawID)
  
  psa_obj <- make_psa_obj(cost = dosenums,
                          effectiveness = effects,
                          parameters = params,
                          strategies = unique(df2$idstrategy),
                          currency = "doses")
  
  # str(psa_obj)
  
  psaplot <- plot(psa_obj) + 
    theme_classic() +
    theme(legend.position = 'none') +
    ggtitle(str_glue("")) + 
    labs(caption = str_glue("Seasonality: {seas}, PfPR: {pfprval}"))
  
  psa_sum <- summary(psa_obj,
                     calc_sds = TRUE)
  
  # psa_sum
  
  icers <- calculate_icers(cost = psa_sum$meanCost,
                           effect = psa_sum$meanEffect,
                           strategies = psa_sum$Strategy)
  icersplot <- plot_icers(icers)  + 
    labs(caption = str_glue("Seasonality: {seas}, PfPR: {pfprval}"))
  
  exploss_obj <- calc_exp_loss(wtp = seq(1e3, 2e4, 1e2),#seq(1e4, 5e5, 5e3),
                               psa = psa_obj) %>%
    separate(Strategy,
             into = c("PEVstrategy", "PEVage", "EPIextra"),
             sep = "\\.\\.",
             remove = FALSE) %>%
    mutate(across(c(PEVstrategy, PEVage),
                  .fns = ~ str_replace(.x, '\\.', '-'))) %>%
    mutate(across(c(EPIextra), ~ .x |>
                    str_replace("^\\.", "") |>        # remove leading .
                    str_replace("^\\.$", "") |>       # replace "." alone with empty
                    str_replace_all("\\.", ", ")       # convert remaining . to -
    )) %>%
    mutate(pfpr = pfprval, 
           seasonality = seas) %>%
    group_by(WTP) %>%
    arrange(Expected_Loss, .by_group = TRUE) %>%
    mutate(rank = rank(Expected_Loss)) 
  
  saveRDS(exploss_obj, 'exploss.rds')
  
  ranking <- ggplot(exploss_obj %>%
           filter(rank < 11)) + 
    geom_line(aes(x = WTP, y = as.integer(rank), color = Strategy)) + 
    geom_label(data = exploss_obj %>% filter(rank < 11 & WTP == 10000), 
               aes(x = WTP, y = rank, color = Strategy, label = Strategy))  +
    scale_y_continuous(breaks = seq(1,10,1)) + 
    labs(y = 'Expected loss rank (1 best)',
         caption = str_glue("Seasonality: {seas}, PfPR: {pfprval}")) +
    theme(legend.position = 'none') 
    
  # plot(exploss_obj) + 
  #   theme(legend.position = 'none')
  #
  # exploss_obj %>% filter(On_Frontier) %>% janitor::tabyl(Strategy)
  
  explossplot <- ggplot(exploss_obj) +
    geom_line(aes(x = WTP / 1000, y = Expected_Loss,
                  color = PEVage, linetype = EPIextra), linewidth = 1) + 
    # geom_text_repel(data = exploss_obj %>% filter(On_Frontier), 
    #   aes(x = WTP / 1000, y = Expected_Loss,
    #       label = paste0(PEVage, ', ', EPIextra))) +
    scale_y_log10() + 
    theme_bw() + 
    labs(x = 'Willingness to Pay\n(incremental doses per case averted (thousands))',
         y = 'Expected loss (doses per 1000 people)',
         caption = str_glue("Seasonality: {seas}, PfPR: {pfprval}"))
  
  ggsave(filename = paste0('psasum_', seas, '_', pfprval, '.png'), plot = psaplot)
  ggsave(filename = paste0('icers_', seas, "_", pfprval, '.png'), plot = icersplot)
  ggsave(filename = paste0('exploss_', seas, '_', pfprval, '.png'), plot = explossplot)
  ggsave(filename = paste0('explossranking_', seas, '_', pfprval, '.png'), plot = ranking)
  
  return(exploss_obj)
}

pfpr_vec <- c(0.01, 0.03, 0.05, 0.25, 0.45, 0.65)
seas_vec <- c('perennial','seasonal')

# Create all combinations of pfpr and sesaonalities 
combinations <- expand.grid(pfpr = pfpr_vec, seas = seas_vec)

# Use map2 from purrr or lapply to apply the analysis function to each of these pfpr and seasonalities 
expectedlosses <- map2(combinations$seas, combinations$pfpr, 
                       ~expected_loss_icer(df, .x, .y))

expectedlosses_frontier <- bind_rows(expectedlosses) %>%
  filter(On_Frontier) %>%
  janitor::tabyl(Strategy, pfpr, seasonality)

saveRDS(expectedlosses_frontier, 'frontier_scenarios.rds')









# from claude.ai

# results_data <- averted %>%
#   filter(age_lower == 0 & age_upper == 100) %>%
#   select(drawID, int_ID, pfpr, seasonality, PEV, PEVstrategy, PEVage, EPIextra, PEV,
#          cases_averted, severe_averted,
#          cases_averted_routine, severe_averted_routine,
#          additional_doses, totaldoses)
# 
# calculate_expected_loss_correct <- function(results_data) {
#   
#   # Step 0: create a mean number of doses per strategy
#   results_data <- results_data %>%
#     group_by(int_ID) %>%
#     mutate(mean_totaldoses = round(mean(totaldoses, na.rm = TRUE),-3)) %>% ungroup()
#   
#   # Step 1: For each parameter set and dose level, 
#   # find the best vaccination scenario (strategy)
#   optimal_by_parameter_dose <- results_data %>%
#     group_by(drawID, mean_totaldoses) %>% #int_ID, pfpr, seasonality, PEVage, EPIextra, mean_totaldoses) %>%
#     summarise(
#       best_scenario_cases = int_ID[which.max(cases_averted)],
#       optimal_cases_averted = max(cases_averted, na.rm = TRUE),
#       .groups = "drop"
#     )
#   
#   # Step 2: Calculate foregone benefits for each vaccination scenario 
#   # in each parameter set
#   results_with_loss <- results_data %>%
#     left_join(optimal_by_parameter_dose, by = c('drawID','mean_totaldoses')) %>%#c('int_ID', 'pfpr', 'seasonality', 'PEVage', 'EPIextra','mean_totaldoses')) %>%
#     mutate(
#       # Foregone benefit: how much worse this vaccination scenario is
#       # compared to the best scenario for this parameter set
#       loss = optimal_cases_averted - cases_averted
#     )
#   
#   # Step 3: Average over the parameter sets (posterior draws)
#   expected_loss_results <- results_with_loss %>%
#     group_by(int_ID, pfpr, seasonality, PEVage, EPIextra, mean_totaldoses, PEV) %>%
#     summarise(
#       expected_cases_averted = mean(cases_averted),
#       expected_loss = mean(loss),  # Average regret across parameter sets
#       sd_cases_averted = sd(cases_averted),
#       n_parameter_sets = n(),
#       .groups = "drop"
#     )
#   
#   expected_loss_frontier <- expected_loss_results %>%
#     group_by(pfpr, seasonality) %>%
#     arrange(totaldoses, .by_group = TRUE) %>% 
#     mutate(mincases = cummin(cases),
#            minsev = cummin(sevcases),
#            maxCA = cummax(cases_averted),
#            maxSA = cummax(severe_averted),
#            maxCA_routine = cummax(cases_averted_routine),
#            maxSA_routine = cummax(severe_averted_routine)) %>% ungroup() %>%
#     mutate(mincases = ifelse(cases == mincases, 1, 0),
#            minsev = ifelse(sevcases == minsev, 1, 0),
#            maxCA = ifelse(cases_averted == maxCA, 1, 0),
#            maxSA = ifelse(severe_averted == maxSA, 1, 0),
#            maxCA_routine = ifelse(cases_averted_routine == maxCA_routine, 1, 0),
#            maxSA_routine = ifelse(severe_averted_routine == maxSA_routine, 1, 0))
#   
#   return(expected_loss_results)
# }
# 
# # Run the calculation
# expected_loss_results <- calculate_expected_loss_correct(results_data)
# 
# # Create efficiency frontier: minimize expected loss at each dose level
# efficiency_frontier <- expected_loss_results %>%
#   group_by(int_ID) %>%
#   slice_min(expected_loss, n = 1, with_ties = FALSE) %>%
#   ungroup() %>%
#   arrange(mean_totaldoses)
# 
# # Plot all scenarios with efficiency frontier highlighted
# ggplot(efficiency_frontier %>% filter(PEV != 'none'), aes(x = mean_totaldoses, y = expected_loss)) +
#   geom_line(color = "black", size = 1.2) +
#   geom_point(aes(color = paste0(PEVage, ' ', EPIextra))) +
#   # geom_point(data = efficiency_frontier, color = "black", size = 3) +
#   # scale_color_gradient(low = "darkgreen", high = "red", name = "Expected\nLoss") +
#   facet_grid(vars(pfpr, seasonality),
#              scales = 'free') +
#   labs(
#     title = "Efficiency Frontier: Robust Scenarios by Dose Level",
#     x = "Dose Level",
#     y = "Expected Cases Averted",
#     subtitle = "Black line shows scenarios that minimize expected loss at each dose level"
#   ) + 
#   theme(legend.position = 'none')
# 
# 
