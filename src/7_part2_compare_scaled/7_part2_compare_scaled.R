# Script to make plots to compare the age-scaled and non age-scaled runs 
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

orderly_resource('add_lim_labels.R')
source('add_lim_labels.R')

# Set dependencies 
orderly2::orderly_dependency("5_process_combined",
                             "latest(parameter:age_scaling == 1)",
                             c(summarized_overall_1.rds = "summarized_overall_draws.rds",
                               summarized_last15_1.rds = "summarized_last15_draws.rds"))
orderly2::orderly_dependency("5_process_combined",
                             "latest(parameter:age_scaling == 0.64)",
                             c(summarized_overall_64.rds = "summarized_overall_draws.rds",
                               summarized_last15_64.rds = "summarized_last15_draws5.rds"))

orderly2::orderly_dependency("6_make_cohorts",
                             "latest(parameter:age_scaling == 1)",
                             c(cohorts_rawdraws.rds = "cohorts_rawdraws.rds"))
orderly2::orderly_dependency("6_make_cohorts",
                             "latest(parameter:age_scaling == 0.64)",
                             c(cohorts_rawdraws.rds = "cohorts_rawdraws.rds"))


summ_1 <- readRDS("summarized_overall_draws_1.rds")
summ_64 <- readRDS("summarized_overall_draws_64.rds")



# Make efficiency frontier with grey line for old frontier
seas_type = 'perennial'
pfpr_vec = c(0.05, 0.25, 0.45)

df_plot1 <- summ_1 %>%
  filter(age_grp == '0-100') %>%
  # Filter to strategy type
  filter(PEVstrategy == 'catch-up' | PEVstrategy == 'AB') %>% 
  filter(!(PEVstrategy %in% c('hybrid', 'SV'))) %>%
  filter(seasonality == seas_type) %>%
  filter(pfpr %in% pfpr_vec) %>%
  mutate(category = ifelse(EPIextra != '-' & PEVage == '-', 'Extra booster(s)', 
                           ifelse(PEVage != '-' & EPIextra == '-', 'Catch-up',
                                  ifelse(PEVage == '-' & EPIextra == '-', "Routine age-based", 'Combined'))),
         EPIextra = factor(EPIextra, levels = c('2y','5y','10y','2y+5y','2y+10y','5y+10y','2y+5y+10y','-')),
         PEVage = ifelse(PEVage == '5-9','5-9y', ifelse(PEVage == '5-14','5-14y', PEVage)),
         PEVage = factor(as.factor(PEVage), levels = c('6m-2y','6m-4y','6m-9y','6m-14y','5-9y','5-14y','-')))
df_plot64 <- summ_64 %>%
  filter(age_grp == '0-100') %>%
  # Filter to strategy type
  filter(PEVstrategy == 'catch-up' | PEVstrategy == 'AB') %>% 
  filter(!(PEVstrategy %in% c('hybrid', 'SV'))) %>%
  filter(seasonality == seas_type) %>%
  filter(pfpr %in% pfpr_vec) %>%
  mutate(category = ifelse(EPIextra != '-' & PEVage == '-', 'Extra booster(s)', 
                           ifelse(PEVage != '-' & EPIextra == '-', 'Catch-up',
                                  ifelse(PEVage == '-' & EPIextra == '-', "Routine age-based", 'Combined'))),
         EPIextra = factor(EPIextra, levels = c('2y','5y','10y','2y+5y','2y+10y','5y+10y','2y+5y+10y','-')),
         PEVage = ifelse(PEVage == '5-9','5-9y', ifelse(PEVage == '5-14','5-14y', PEVage)),
         PEVage = factor(as.factor(PEVage), levels = c('6m-2y','6m-4y','6m-9y','6m-14y','5-9y','5-14y','-')))


booster_colors <- c("2y" = "#c1ef7b" ,"5y" = "#85ecd1", "10y" = "#83bae7" , '2y+5y' = "#acace9","2y+10y" = "#6c8bf7", "5y+10y" = "#4d47d5",  '2y+5y+10y' = "#160e6f")

catch_up_colors <- c("6m-2y" = "#90b260", "6m-4y" = "#efc642", "6m-9y" = "#fd7270", "6m-14y" = "#ce5800",
                     "5-9y" = "#991010", "5-14y" = "#65612c")


eff_plot <- function(var, eff_var){
  dfpl1 <- df_plot1 %>% filter(.data[[eff_var]] == 1) %>%
    mutate(dosesper1000 = totaldoses / n *1000)
  dfpl64 <- df_plot64 %>% filter(.data[[eff_var]] == 1) %>%
    mutate(dosesper1000 = totaldoses / n *1000)
  
  pfpr.labs <- c("5%", "25%", '45%')
  names(pfpr.labs) <- c("0.05","0.25", "0.45")
  
  
  plt <- ggplot(dfpl64) +
    # Add grey line for non-scaled version
    geom_line(data = dfpl1, 
              aes(x = dosesper1000,
                  y = .data[[var]]), color = 'grey60',linetype = 2,
              linewidth = 0.7) +
    geom_line(data = dfpl64, 
              aes(x = dosesper1000,
                  y = .data[[var]]),
              linewidth = 0.7) +
    
    ############ With non-dominated scenarios
    # Plot standalone routine interventions
    geom_point(data = dfpl64 %>% filter(category =='Routine age-based'),
               aes(x = dosesper1000,
                   y = .data[[var]],
                   shape = category),
               color = '#e71d1d',#CUcols[1],
               size = 4.5) +
    # standalone boosters
    geom_point(data = dfpl64 %>% filter(category == 'Extra booster(s)'),
               aes(x = dosesper1000,
                   y = .data[[var]],
                   color = EPIextra,
                   shape = category),
               size = 4.5,
               position = position_nudge(x = -50)) +
    # standalone catch-up
    geom_point(data = dfpl64 %>% filter(category == 'Catch-up'),
               aes(x = dosesper1000,
                   y = .data[[var]],
                   fill = PEVage,
                   shape = category),
               size = 4,
               color = '#ffffff00',
               position = position_nudge(x = -50)) +
    # combined
    geom_point(data = dfpl64 %>% filter(category == 'Combined')%>% mutate(category = 'Extra booster(s)'),
               aes(x = dosesper1000,
                   y = .data[[var]],
                   color = EPIextra,
                   shape = category),
               size = 4.5,
               position = position_nudge(x = -50)) +
    geom_point(data = dfpl64 %>% filter(category == 'Combined') %>% mutate(category = 'Catch-up'),
               aes(x = dosesper1000,
                   y = .data[[var]],
                   fill = PEVage),
               color = '#ffffff00',
               size = 4,
               shape = 22,
               position = position_nudge(x = -140)) +
    
    
    
    # Define shapes for interventions
    scale_shape_manual(
      name = "Vaccination strategy",
      values = c("Routine age-based" = 17, "Extra booster(s)" = 18, "Catch-up" = 22)
    ) +
    scale_x_continuous(labels = scales::label_comma(), limits = c(2900,5600)) +
    scale_y_continuous(labels = scales::label_comma(), ) +
    # Color and fill scales for booster and catch-up with separate legends
    scale_color_manual(
      name = "Extra booster(s) timing",
      values = booster_colors,
      guide = guide_legend(override.aes = list(shape = 16))
    ) +
    scale_fill_manual(
      name = "Catch-up target age group",
      values = catch_up_colors,
      guide = guide_legend(override.aes = list(shape = 22), color = '#ffffff00')
    ) +
    facet_wrap(~pfpr,
               scales = 'free',
               labeller = labeller(pfpr = pfpr.labs)) +
    theme_bw(base_size = 14) +
    theme(axis.title = element_text(size = 20),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(angle = 90, size = 14),
          plot.caption = element_text(size = 14),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 14),
          legend.key.size = unit(0.8, 'cm'),
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 12),
          legend.position = 'none'
    ) 
  return(plt)
}

# Read in legend
legend_img <- grid::rasterGrob(readPNG("R:/Kelly/catchup_extraboosters/src/7_produce_plots/legend.png"), interpolate=TRUE)

#Make plots 
CA <- eff_plot(var = 'cases_averted_perpop', eff_var = 'maxCA') + 
  labs(x = 'Doses per 1000 population',
       y = 'Cumulative clinical cases\naverted per 1000 population',
       color = 'Vaccination strategy',
       shape = 'Strategy type')
CAleg <- plot_grid(CA, legend_img, rel_widths = c(4,1))

ggsave(paste0('CAbytotaldoses', seas_type, '.pdf'), CAleg, width = 14, height = 8)

SA <- eff_plot(var = 'severe_averted_perpop', eff_var = 'maxSA') + 
  labs(x = 'Doses per 1000 population',
       y = 'Cumulative severe cases\naverted per 1000 population',
       color = 'Vaccination strategy',
       shape = 'Strategy type')
SAleg <- plot_grid(SA, legend_img, rel_widths = c(4,1))
ggsave(paste0('SAbytotaldoses', seas_type,'.pdf'), SAleg, width = 14, height = 8)

averted_plt <- cowplot::plot_grid(CA + theme(legend.position="none"), 
                                  SA + theme(legend.position="none"), 
                                  ncol = 1, labels = 'AUTO')
avertedwleg <- plot_grid(averted_plt, legend_img, 
                         ncol = 2, rel_widths = c(4,1))
ggsave(paste0('CASAbytotaldoses', seas_type, '.pdf'), avertedwleg, width = 14, height = 8)










# df_summ_1 <- readRDS("summarized_overall_draws_1.rds") %>% 
#   select(age_grp, age_lower, halfyear, clinical, severe, person_days, time, seasonality, pfpr, 
#          PEVstrategy, PEVrounds, EPIbooster, PEVage, EPIextra, massbooster_rep, age_scaling, 
#          year, week, month, day, 
#          cases_averted_perpop, severe_averted_perpop, cases_averted_perdose, severe_averted_perdose)%>%
#   rename(baseline_CA_perpop = cases_averted_perpop,
#          baseline_SA_perpop = severe_averted_perpop,
#          baseline_CA_perdose = cases_averted_perdose,
#          baseline_SA_perdose = severe_averted_perdose) %>%
#   filter(age_grp == '0-100')
# df_summ_64 <- readRDS("summarized_overall_draws_64.rds") %>% 
#   select(age_grp, age_lower, halfyear, clinical, severe, person_days, time, seasonality, pfpr, 
#          PEVstrategy, PEVrounds, EPIbooster, PEVage, EPIextra, massbooster_rep, age_scaling, 
#          year, week, month, day, 
#          cases_averted_perpop, severe_averted_perpop, cases_averted_perdose, severe_averted_perdose)%>%
#   filter(age_grp == '0-100')
# df_last15_1 <- readRDS("summarized_last15_draws_1.rds") %>% 
#   select(age_grp, age_lower, halfyear, clinical, severe, person_days, time, seasonality, pfpr, 
#          PEVstrategy, PEVrounds, EPIbooster, PEVage, EPIextra, massbooster_rep, age_scaling, 
#          year, week, month, day, 
#          cases_averted_perpop, severe_averted_perpop, cases_averted_perdose, severe_averted_perdose)%>%
#   rename(baseline_CA_perpop = cases_averted_perpop,
#          baseline_SA_perpop = severe_averted_perpop,
#          baseline_CA_perdose = cases_averted_perdose,
#          baseline_SA_perdose = severe_averted_perdose)%>%
#   filter(age_grp == '0-100')
# df_last15_64 <- readRDS("summarized_last15_draws_64.rds") %>% 
#   select(age_grp, age_lower, halfyear, clinical, severe, person_days, time, seasonality, pfpr, 
#          PEVstrategy, PEVrounds, EPIbooster, PEVage, EPIextra, massbooster_rep, age_scaling, 
#          year, week, month, day, 
#          cases_averted_perpop, severe_averted_perpop, cases_averted_perdose, severe_averted_perdose)%>%
#   filter(age_grp == '0-100')
# just for use in non-dependency
# df_summ_64 <- summarized_overall_draws64 %>%
#   select(drawID, age_grp, age_lower, halfyear, clinical, severe, person_days, seasonality, pfpr, PEVage, EPIextra, int_ID,
#          age_scaling, PEVstrategy, PEVrounds, EPIbooster, PEVage, EPIextra, massbooster_rep, age_scaling, 
#          
#          cases_averted_perpop, severe_averted_perpop, cases_averted_perdose, severe_averted_perdose) %>%
#   filter(age_grp == '0-100')
# df_last15_64 <- summarized_last15_draws64 %>%
#   select(drawID, age_grp, age_lower, halfyear, clinical, severe, person_days, seasonality, pfpr, PEVage, EPIextra, int_ID,
#          age_scaling, PEVstrategy, PEVrounds, EPIbooster, PEVage, EPIextra, massbooster_rep, age_scaling, 
#          
#          cases_averted_perpop, severe_averted_perpop, cases_averted_perdose, severe_averted_perdose)%>%
#   filter(age_grp == '0-100')
# df_summ_1 <- summarized_overall_draws1 %>%
#   select(drawID, age_grp, age_lower, halfyear, clinical, severe, person_days, seasonality, pfpr, PEVage, EPIextra, int_ID,
#          age_scaling, PEVstrategy, PEVrounds, EPIbooster, PEVage, EPIextra, massbooster_rep, age_scaling, 
#          
#          cases_averted_perpop, severe_averted_perpop, cases_averted_perdose, severe_averted_perdose) %>%
#   rename(baseline_CA_perpop = cases_averted_perpop,
#          baseline_SA_perpop = severe_averted_perpop,
#          baseline_CA_perdose = cases_averted_perdose,
#          baseline_SA_perdose = severe_averted_perdose)%>%
#   filter(age_grp == '0-100')
# df_last15_1 <- summarized_last15_draws1 %>%
#   select(drawID, age_grp, age_lower, halfyear, clinical, severe, person_days, seasonality, pfpr, PEVage, EPIextra, int_ID,
#          age_scaling, PEVstrategy, PEVrounds, EPIbooster, PEVage, EPIextra, massbooster_rep, age_scaling, 
#          
#          cases_averted_perpop, severe_averted_perpop, cases_averted_perdose, severe_averted_perdose)%>%
#   rename(baseline_CA_perpop = cases_averted_perpop,
#          baseline_SA_perpop = severe_averted_perpop,
#          baseline_CA_perdose = cases_averted_perdose,
#          baseline_SA_perdose = severe_averted_perdose)%>%
#   filter(age_grp == '0-100')

# # First, make a plot showing the cumulative difference in clinical cases per dose in scaled versus non-scaled runs in overall dataset
# df_summ <- left_join(df_summ_1, df_summ_64, by = c('drawID','age_grp','age_lower','seasonality','pfpr',
#                                                    'PEVstrategy','massbooster_rep','EPIbooster','PEVage','EPIextra','int_ID')) %>%
#   # Calculate differences
#   mutate(diff_CA_perpop = baseline_CA_perpop - cases_averted_perpop,
#          diff_SA_perpop = baseline_SA_perpop - severe_averted_perpop,
#          diff_CA_perdose = baseline_CA_perdose - cases_averted_perdose,
#          diff_SA_perdose = baseline_SA_perdose - severe_averted_perdose,
#          pdiff_CA_perpop = diff_CA_perpop / baseline_CA_perpop,
#          pdiff_SA_perpop = diff_SA_perpop / baseline_SA_perpop,
#          pdiff_CA_perdose = diff_CA_perdose / baseline_CA_perdose,
#          pdiff_SA_perdose = diff_SA_perdose / baseline_SA_perdose) %>%
#   add_lim_labels() 
# 
# # Then get 95% Cis and medians 
# diff_overall <- df_summ %>%
#   group_by(age_grp, age_lower, int_ID, labels,PEVstrategy,
#            pfpr, seasonality, PEVage, EPIextra
#   ) %>%
#   summarize(across(c(contains('averted'), contains('baseline'),
#                      contains('diff')),
#                    list(lower = ~quantile(.x, 0.025, na.rm = TRUE),
#                         median = ~quantile(.x, 0.5, na.rm = TRUE),
#                         upper = ~quantile(.x, 0.975, na.rm = TRUE)),
#                    .names = "{.col}_{.fn}") ) %>%
#   # rename those variables with _median to be just the variable name 
#   rename_with(.fn = \(x)sub("_median","", x)) 
# 
# 
# # Then do the same for the age-based ones 
# # First, make a plot showing the cumulative difference in clinical cases per dose in scaled versus non-scaled runs 
# df_last15 <- left_join(df_last15_1, df_last15_64, by = c('drawID','age_grp','age_lower','seasonality','pfpr',
#                                                          'PEVstrategy','massbooster_rep','EPIbooster','PEVage','EPIextra','int_ID')) %>%
#   # Calculate differences
#   mutate(diff_CA_perpop = baseline_CA_perpop - cases_averted_perpop,
#          diff_SA_perpop = baseline_SA_perpop - severe_averted_perpop,
#          diff_CA_perdose = baseline_CA_perdose - cases_averted_perdose,
#          diff_SA_perdose = baseline_SA_perdose - severe_averted_perdose,
#          pdiff_CA_perpop = diff_CA_perpop / baseline_CA_perpop,
#          pdiff_SA_perpop = diff_SA_perpop / baseline_SA_perpop,
#          pdiff_CA_perdose = diff_CA_perdose / baseline_CA_perdose,
#          pdiff_SA_perdose = diff_SA_perdose / baseline_SA_perdose) %>%
#   add_lim_labels() 
# 
# # Then get 95% Cis and medians 
# diff_last15 <- df_last15 %>%
#   group_by(age_grp, age_lower, int_ID, labels,PEVstrategy,
#            pfpr, seasonality, PEVage, EPIextra
#   ) %>%
#   summarize(across(c(contains('averted'), contains('baseline'),
#                      contains('diff')),
#                    list(lower = ~quantile(.x, 0.025, na.rm = TRUE),
#                         median = ~quantile(.x, 0.5, na.rm = TRUE),
#                         upper = ~quantile(.x, 0.975, na.rm = TRUE)),
#                    .names = "{.col}_{.fn}") ) %>%
#   # rename those variables with _median to be just the variable name 
#   rename_with(.fn = \(x)sub("_median","", x)) 
# 
# CUcols1 <- c('#B03A2E','#6C3483','#1F618D','#00796b','#fbc02d','#CA6F1E','#689f38', '#3498db','tan','#283747','#85929E')
# CUcols_ <- c('black','#6C3483','#1F618D','#00796b','#fbc02d','#CA6F1E','#689f38', '#3498db','tan','#283747','#85929E')
# 
# # Make a plot of the difference between the two types first for overall then last15 
# variables = c('diff_CA_perpop', 'diff_SA_perpop', 'pdiff_CA_perpop','pdiff_SA_perpop')
# for(variable in variables){
#   df <- diff_overall %>%
#     filter((EPIextra == '-'  & PEVstrategy == 'catch-up') | (PEVstrategy == 'AB' & EPIextra == '-') & pfpr != 0.03)
#   
#   A <- ggplot(df) +
#     geom_col(aes(x = as.factor(pfpr), 
#                  y = .data[[variable]], 
#                  fill = labels, color = labels), 
#              position ='dodge', 
#              alpha = 0.7) + 
#     geom_errorbar(aes(x = as.factor(pfpr), 
#                       ymin = .data[[paste0(variable, "_lower")]],
#                       ymax = .data[[paste0(variable, "_upper")]], 
#                       color = labels),
#                   position = position_dodge(width = 0.9), 
#                   width = 0.35, 
#                   linewidth = 0.7) +
#     theme_bw() +
#     scale_fill_manual(values = CUcols1) +
#     scale_color_manual(values = CUcols_) +
#     facet_wrap(~seasonality) +
#     labs(y = if(variable == 'diff_CA_perpop'){
#       'Difference in cumulative clinical cases averted\nper 1,000 population'
#     } else if(variable == 'pdiff_CA_perpop'){
#       'Percent difference in cumulative clinical cases averted\nper 1,000 population'
#     } else if (variable == 'diff_SA_perpop'){
#       'Difference in cumulative severe cases averted\nper 1,000 population'
#     } else if (variable == 'pdiff_SA_perpop'){
#       'Percent difference in cumulative severe cases averted\nper 1,000 population'
#     },
#     x = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
#     fill = 'Vaccination strategy',
#     caption = 'Negative values indicate that the non-scaled strategy averted more cases compared to the scaled strategy,\nand positive values show that the scaled strategy averted more cases.'
#     ) +
#     guides(color = 'none') +
#     theme(axis.title = element_text(size = 20),
#           plot.title = element_text(size = 22),
#           legend.text = element_text(size = 15),
#           strip.text.x = element_text(size = 12),
#           legend.title = element_text(size = 18),
#           plot.caption = element_text(size = 12),
#           legend.key.size = unit(0.8, 'cm'),
#           axis.text.x = element_text(size = 12),
#           axis.text.y = element_text(size = 12)
#     )
#   
#   if(grepl('p', variable)){
#     A <- A + 
#       scale_y_continuous(labels = scales::label_percent()) 
#   }
#   
#   ggsave(file = paste0(variable, "summ.png"), A, width = 15, height = 8)
#   
# }
# 
# for(variable in variables){
#   df <- diff_last15 %>% 
#     filter(PEVstrategy == 'AB' & pfpr != 0.03)  
#   
#   A <- ggplot(df) +
#     geom_col(aes(x = as.factor(pfpr), 
#                  y = .data[[variable]], 
#                  fill = labels, color = labels), 
#              position ='dodge', 
#              alpha = 0.7) + 
#     geom_errorbar(aes(x = as.factor(pfpr), 
#                       ymin = .data[[paste0(variable, "_lower")]],
#                       ymax = .data[[paste0(variable, "_upper")]], 
#                       color = labels),
#                   position = position_dodge(width = 0.9), 
#                   width = 0.35, 
#                   linewidth = 0.7) +
#     theme_bw() +
#     scale_fill_manual(values = CUcols1) +
#     scale_color_manual(values = CUcols_) +
#     facet_wrap(~seasonality) +
#     labs(y = if(variable == 'diff_CA_perpop'){
#       'Difference in cumulative clinical cases averted\nper 1,000 population'
#     } else if(variable == 'pdiff_CA_perpop'){
#       'Percent difference in cumulative clinical cases averted\nper 1,000 population'
#     } else if (variable == 'diff_SA_perpop'){
#       'Difference in cumulative severe cases averted\nper 1,000 population'
#     } else if (variable == 'pdiff_SA_perpop'){
#       'Percent difference in cumulative severe cases averted\nper 1,000 population'
#     },
#     x = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
#     fill = 'Vaccination strategy'
#     ) +
#     guides(color = 'none') +
#     theme(axis.title = element_text(size = 20),
#           plot.title = element_text(size = 22),
#           legend.text = element_text(size = 15),
#           strip.text.x = element_text(size = 12),
#           legend.title = element_text(size = 18),
#           plot.caption = element_text(size = 12),
#           legend.key.size = unit(0.8, 'cm'),
#           axis.text.x = element_text(size = 12),
#           axis.text.y = element_text(size = 12)
#     )
#   
#   if(grepl('p', variable)){
#     A <- A + 
#       scale_y_continuous(labels = scales::label_percent()) 
#   }
#   
#   ggsave(file = paste0(variable, "last15.png"), A, width = 15, height = 8)
#   
# }
# 
# 
# 
# 
# ## Cohorts 
# 
# cohorts_draws1 <- readRDS("cohorts_rawdraws1.rds") 
# cohorts_draws1 <- cohorts_draws1%>%
#   rename(baseline_CA_perpop = cases_averted_perpop,
#          baseline_SA_perpop = severe_averted_perpop,
#          baseline_CA_perdose = cases_averted_perdose,
#          baseline_SA_perdose = severe_averted_perdose)
# 
# cohorts_draws64 <- readRDS('cohorts_rawdraws64.rds')
# 
# cohortsdraws <- left_join(cohorts_draws1, cohorts_draws64, by = c('drawID','halfyear','seasonality','pfpr',
#                                                                   'PEVstrategy','massbooster_rep','EPIbooster','PEVage','EPIextra','int_ID')) %>%
#   # Calculate differences
#   mutate(diff_CA_perpop = baseline_CA_perpop - cases_averted_perpop,
#          diff_SA_perpop = baseline_SA_perpop - severe_averted_perpop,
#          diff_CA_perdose = baseline_CA_perdose - cases_averted_perdose,
#          diff_SA_perdose = baseline_SA_perdose - severe_averted_perdose,
#          pdiff_CA_perpop = diff_CA_perpop / baseline_CA_perpop,
#          pdiff_SA_perpop = diff_SA_perpop / baseline_SA_perpop,
#          pdiff_CA_perdose = diff_CA_perdose / baseline_CA_perdose,
#          pdiff_SA_perdose = diff_SA_perdose / baseline_SA_perdose) %>%
#   add_lim_labels() 
# 
# # Then get 95% Cis and medians 
# diffcohorts <- cohortsdraws %>%
#   group_by(halfyear, int_ID, labels, PEVstrategy,
#            pfpr, seasonality, PEVage, EPIextra
#   ) %>%
#   summarize(across(c(contains('averted'), contains('baseline'),
#                      contains('diff')),
#                    list(lower = ~quantile(.x, 0.025, na.rm = TRUE),
#                         median = ~quantile(.x, 0.5, na.rm = TRUE),
#                         upper = ~quantile(.x, 0.975, na.rm = TRUE)),
#                    .names = "{.col}_{.fn}") ) %>%
#   # rename those variables with _median to be just the variable name 
#   rename_with(.fn = \(x)sub("_median","", x)) 
# 
# # Per dose 
# variables = c('diff_CA_perdose', 'diff_SA_perdose', 'pdiff_CA_perdose','pdiff_SA_perdose')
# for(variable in variables){
#   df <- diffcohorts %>% 
#     filter((EPIextra == '-'  & PEVstrategy == 'catch-up') | (PEVstrategy == 'AB' & EPIextra == '-') & pfpr != 0.03)  
#   
#   B <- ggplot(df) + # Catch-up plots are made with cohorts and age-based booster plots with whole simulation 
#     geom_col(aes(x = as.factor(pfpr), 
#                  y = .data[[variable]], 
#                  fill = labels, color = labels), 
#              position ='dodge', 
#              alpha = 0.7) + 
#     geom_errorbar(aes(x = as.factor(pfpr), 
#                       ymin = .data[[paste0(variable, "_lower")]], 
#                       ymax = .data[[paste0(variable, "_upper")]], 
#                       color = labels),
#                   position = position_dodge(width = 0.9), 
#                   width = 0.35, 
#                   linewidth = 0.7) +
#     theme_bw() +
#     scale_fill_manual(values = CUcols1) +
#     scale_color_manual(values = CUcols_) +
#     facet_wrap(~seasonality) +
#     labs(y = if(variable == 'diff_CA_perdose'){
#       'Difference in cumulative clinical cases averted\nper 1,000 doses'
#     } else if(variable == 'pdiff_CA_perdose'){
#       'Percent difference in cumulative clinical cases averted\nper 1,000 doses'
#     } else if (variable == 'diff_SA_perdose'){
#       'Difference in cumulative severe cases averted\nper 1,000 doses'
#     } else if (variable == 'pdiff_SA_perdose'){
#       'Percent difference in cumulative severe cases averted\nper 1,000 doses'
#     },
#     x = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
#     fill = 'Vaccination strategy'
#     ) +
#     guides(color = 'none') +
#     theme(axis.title = element_text(size = 20),
#           axis.text.x = element_text(size = 14),
#           axis.text.y = element_text(size = 14),
#           plot.caption = element_text(size = 14),
#           legend.title = element_text(size = 18),
#           legend.text = element_text(size = 14),
#           legend.key.size = unit(0.8, 'cm'),
#           strip.text.x = element_text(size = 12),
#           strip.text.y = element_text(size = 12)
#     )
#   
#   if(grepl('p', variable)){
#     B <- B + 
#       scale_y_continuous(labels = scales::label_percent()) 
#   }
#   
#   ggsave(file = paste0(variable, "cohorts_CU.png"), B, width = 15, height = 8)
# }
# 
# for(variable in variables){
#   df <- diff_last15 %>% 
#     filter(PEVstrategy == 'AB' & pfpr != 0.03)  
#   
#   B <- gdiff_last15B <- ggplot(df) + # Catch-up plots are made with cohorts and age-based booster plots with whole simulation 
#     geom_col(aes(x = as.factor(pfpr), 
#                  y = .data[[variable]], 
#                  fill = labels, color = labels), 
#              position ='dodge', 
#              alpha = 0.7) + 
#     geom_errorbar(aes(x = as.factor(pfpr), 
#                       ymin = .data[[paste0(variable, "_lower")]], 
#                       ymax = .data[[paste0(variable, "_upper")]], 
#                       color = labels),
#                   position = position_dodge(width = 0.9), 
#                   width = 0.35, 
#                   linewidth = 0.7) +
#     theme_bw() +
#     scale_fill_manual(values = CUcols1) +
#     scale_color_manual(values = CUcols_) +
#     scale_y_continuous(labels = scales::label_percent()) +
#     facet_wrap(~seasonality) +
#     labs(y = if(variable == 'diff_CA_perdose'){
#       'Difference in cumulative clinical cases averted\nper 1,000 doses'
#     } else if(variable == 'pdiff_CA_perdose'){
#       'Percent difference in cumulative clinical cases averted\nper 1,000 doses'
#     } else if (variable == 'diff_SA_perdose'){
#       'Difference in cumulative severe cases averted\nper 1,000 doses'
#     } else if (variable == 'pdiff_SA_perdose'){
#       'Percent difference in cumulative severe cases averted\nper 1,000 doses'
#     },
#     x = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
#     fill = 'Vaccination strategy'
#     ) +
#     guides(color = 'none') +
#     theme(axis.title = element_text(size = 20),
#           axis.text.x = element_text(size = 14),
#           axis.text.y = element_text(size = 14),
#           plot.caption = element_text(size = 14),
#           legend.title = element_text(size = 18),
#           legend.text = element_text(size = 14),
#           legend.key.size = unit(0.8, 'cm'),
#           strip.text.x = element_text(size = 12),
#           strip.text.y = element_text(size = 12)
#     )
#   
#   if(grepl('p', variable)){
#     B <- B + 
#       scale_y_continuous(labels = scales::label_percent()) 
#   }
#   
#   ggsave(file = paste0(variable, "AB.png"), B, width = 15, height = 8)
#   
# }

