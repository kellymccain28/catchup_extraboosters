# Expected loss calculation 
# 
library(dampack)
library(purrr)
library(tidyverse)
library(orderly2)
library(ggrepel)

# orderly_dependency(name = '5_process_combined',
#                    "latest()",
#                    files = c('summarized_overall_draws'))

source('R:/Kelly/catchup_extraboosters/src/5_process_combined/plot_icers.R')
source('R:/Kelly/catchup_extraboosters/src/5_process_combined/plot_exploss.R')

# df <- readRDS("summarized_overall_draws.R")
df <- readRDS("R:/Kelly/catchup_extraboosters/archive/7_produce_plots/20250826-151746-e67137b1/summarized_overall_draws.rds") # non-scaled


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
    mutate(additional_doses_perpop = additional_doses/ n) %>%#) %>%# 
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
    theme_classic(base_size = 14) +
    theme(legend.position = 'none') +
    ggtitle(str_glue("")) + 
    labs(caption = str_glue("Seasonality: {seas}, PfPR: {pfprval}"))
  
  psa_sum <- summary(psa_obj,
                     calc_sds = TRUE)
  
  # psa_sum
  # ceac_obj <- ceac(wtp = seq(10, 300, 10),
  #                  psa = psa_obj)
  # plot(ceac_obj,
  #      frontier = TRUE,
  #      points = TRUE) 
  
  icers <- calculate_icers(cost = psa_sum$meanCost,
                           effect = psa_sum$meanEffect,
                           strategies = psa_sum$Strategy)
  icersplot <- plot_icers(icers)  + 
    labs(caption = str_glue("Seasonality: {seas}, PfPR: {pfprval}"),
         x = 'Effect (additional cases averted per person)',
         y = 'Cost (additional doses per person)') + theme_bw(base_size = 14)
  
  wtps <- seq(5,200,10)#seq(0.01, 3, .03)
  exploss_obj <- calc_exp_loss(wtp = wtps,#seq(1e3, 2e4, 1e2),#seq(1e4, 5e5, 5e3),
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
    mutate(rank = rank(Expected_Loss)) %>%
    mutate(label_val = ifelse(PEVage != '' & EPIextra!='', paste0(PEVage, ', ', EPIextra),
                              ifelse(PEVage == '' & EPIextra !='', EPIextra,
                                     ifelse(EPIextra == '' & PEVage != '', PEVage, NA))))
  
  saveRDS(exploss_obj, paste0('exploss',pfprval,'.rds'))
  
  ranking <- ggplot(exploss_obj %>%
           filter(rank < 10)) + 
    geom_line(aes(x = WTP, y = as.integer(rank), color = Strategy), linewidth = 0.8) + 
    geom_point(aes(x = WTP, y = as.integer(rank), color = Strategy)) + 
    geom_label(data = exploss_obj %>% filter(rank < 10 & WTP == 135), 
               aes(x = WTP, y = as.integer(rank), color = Strategy, label = label_val))  +
    geom_label(data = exploss_obj %>% filter(rank < 10 & WTP == 5), 
               aes(x = WTP - 35, y = as.integer(rank), color = Strategy, label = label_val))  +
    scale_x_continuous(breaks = seq(0,200,50),
                       limits = c(-55,200)) + #xlim(c(0,200))+
    labs(y = 'Expected loss rank (1 best)',
         x = 'WTP',#\n(additional doses per additional cases averted (thousands))',
         caption = str_glue("Seasonality: {seas}, PfPR: {pfprval}")) +
    theme_bw(base_size = 14) +
    theme(legend.position = 'none')
    
  # plot(exploss_obj) + 
  #   theme(legend.position = 'none')
  #
  # exploss_obj %>% filter(On_Frontier) %>% janitor::tabyl(Strategy)
  
  explossplot <- ggplot(exploss_obj %>% 
                          mutate(Expected_Loss = ifelse(Expected_Loss == 0, Expected_Loss+0.0001, Expected_Loss))) +
    geom_line(aes(x = WTP, y = Expected_Loss,
                  color = PEVage, linetype = EPIextra), linewidth = 0.7) + 
    # geom_text_repel(data = exploss_obj %>% filter(On_Frontier), 
    #   aes(x = WTP / 1000, y = Expected_Loss,
    #       label = paste0(PEVage, ', ', EPIextra))) +
    scale_y_log10(labels = scales::label_comma()) + #xlim(c(0,200))+
    theme_bw(base_size = 14) + 
    # scale_y_continuous(labels = scales::label_comma()) +
    labs(x = 'Willingness to Pay',#\n(additional doses per additional cases averted (thousands))',
         y = 'Expected loss',#\n(additional doses per 1000 people)',
         caption = str_glue("Seasonality: {seas}, PfPR: {pfprval}"))
  
  ggsave(filename = paste0('psasum_', seas, '_', pfprval, '.png'), plot = psaplot, height = 6, width = 7)
  ggsave(filename = paste0('icers_', seas, "_", pfprval, '.png'), plot = icersplot, height = 6, width = 7)
  ggsave(filename = paste0('exploss_', seas, '_', pfprval, '.png'), plot = explossplot, height = 6, width = 7)
  ggsave(filename = paste0('explossranking_', seas, '_', pfprval, '.png'), plot = ranking, height = 6, width = 7)
  
  return(exploss_obj)
}

pfpr_vec <- c(0.01, 0.03, 0.05, 0.25, 0.45, 0.65)
seas_vec <- c('perennial','seasonal')

# Create all combinations of pfpr and sesaonalities 
combinations <- expand.grid(pfpr = pfpr_vec, seas = seas_vec)

# Use map2 from purrr or lapply to apply the analysis function to each of these pfpr and seasonalities 
expectedlosses <- purrr::map2(combinations$seas, combinations$pfpr, 
                       ~expected_loss_icer(df, .x, .y))

expectedlosses_frontier <- bind_rows(expectedlosses) %>%
  filter(On_Frontier) %>%
  janitor::tabyl(Strategy, pfpr, seasonality)

saveRDS(expectedlosses_frontier, 'frontier_scenarios.rds')








