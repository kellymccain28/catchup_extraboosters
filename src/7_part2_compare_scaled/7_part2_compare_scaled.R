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
library(dampack)

orderly_strict_mode()
orderly2::orderly_description('Make figures for manuscript')

orderly_resource('add_lim_labels.R')
source('add_lim_labels.R')

orderly_shared_resource('get_icers.R')
source('get_icers.R')
# Set dependencies 
# orderly2::orderly_dependency("5_process_combined",
#                              "latest(parameter:age_scaling == 1)",
#                              c(summarized_overall_draws1.rds = "summarized_overall_draws.rds",
#                                summarized_overall_1.rds = "summarized_overall.rds"))
# orderly2::orderly_dependency("5_process_combined",
#                              "latest(parameter:age_scaling == 0.64)",
#                              c(summarized_overall_draws64.rds = "summarized_overall_draws.rds",
#                                summarized_overall_64.rds = "summarized_overall.rds"))


# summ_draws1 <- readRDS("summarized_overall_draws1.rds")
# summ_draws64 <- readRDS("summarized_overall_draws64.rds")
summ_draws64 <- readRDS("R:/Kelly/catchup_extraboosters/archive/7_produce_plots/20250826-151746-e671d9b9/summarized_overall_draws.rds")
summ_draws1 <- readRDS("R:/Kelly/catchup_extraboosters/archive/7_produce_plots/20250826-151746-e67137b1/summarized_overall_draws.rds")



# Make efficiency frontier with grey line for old frontier
seas_type = 'perennial'
pfpr_vec = c(0.05, 0.25, 0.45)


dfplot64 <- get_icers(df = summ_draws64)
dfplot1 <- get_icers(df = summ_draws1)

# booster_colors <- c("2y" = "#85ecd1" ,"5y" = "#72d5d9", "10y" = "#8abae7", '2y+5y' = "#acace9" ,"2y+10y" = "#6c8bf7", "5y+10y" = "#4d47d5",  '2y+5y+10y' = "#160e6f")
booster_colors <- c("2y" = "#c1ef7b" ,"5y" = "#85ecd1", "10y" = "#83bae7" , '2y+5y' = "#acace9","2y+10y" = "#6c8bf7", "5y+10y" = "#4d47d5",  '2y+5y+10y' = "#160e6f")

# booster_colors <- c("2y" = "#85ece8" ,"5y" = "#5fc8e0", "10y" = "#83bae7" , '2y+5y' = "#acace9","2y+10y" = "#6c8bf7", "5y+10y" = "#4d47d5",  '2y+5y+10y' = "#160e6f")
catch_up_colors <- c("6m-2y" = "#90b260", "6m-4y" = "#efc642", "6m-9y" = "#fd7270", "6m-14y" = "#ce5800",
                     "5-9y" = "#991010", "5-14y" = "#65612c")

eff_plot <- function(casetype, seas_type){
  
  minx = 0
  maxx = 2.35
  
  pfpr.labs <- c("5%", "25%", '45%')
  names(pfpr.labs) <- c("0.05","0.25", "0.45")
  
  dfplot <- dfplot64 %>%
    filter(type == casetype) %>%
    filter(seasonality == seas_type)
  
  dfplot1 <- dfplot1 %>%
    filter(type == casetype) %>%
    filter(seasonality == seas_type)
  
  lab_data <- dfplot[dfplot$Status == 'ND', ]
  lab_data <- lab_data %>%
    mutate(labels = case_when(
      EPIextra == '' ~ PEVage,
      PEVage == '' ~ EPIextra, 
      TRUE ~ paste0(PEVage, ", ", EPIextra)))
  
  plt <- ggplot() +
    # Add grey line for non-scaled version
    geom_line(data = dfplot1 %>% filter(Status == 'ND'), 
              aes(x = Cost,
                  y = Effect), color = 'grey60',linetype = 2,
              linewidth = 0.7) +
    
    geom_line(data = dfplot %>% filter(Status == 'ND') ,
              aes(x = Cost,
                  y = Effect),
              linewidth = 0.7) +
    
    
    ############ With extended-dominated scenarios
    # standalone boosters
    geom_point(data = dfplot %>% filter(category == 'Extra booster(s)' & Status == 'ED'),
               aes(x = Cost,
                   y = Effect,
                   color = EPIextra,
                   shape = category,
                   alpha = Status),
               size = 2.5,
               position = position_nudge(x = -0.05)) +
    # standalone catch-up
    geom_point(data = dfplot %>% filter(category == 'Catch-up' & Status == 'ED'),
               aes(x = Cost,
                   y = Effect,
                   fill = PEVage,
                   shape = category,
                   alpha = Status),
               size = 2.4,
               stroke = 0.01,
               color = '#ffffff00',
               position = position_nudge(x = -0.05)) +
    # combined
    geom_point(data = dfplot %>% filter(category == 'Combined' & Status == 'ED') %>% mutate(category = 'Extra booster(s)'),
               aes(x = Cost,
                   y = Effect,
                   color = EPIextra,
                   shape = category,
                   alpha = Status),
               size = 2.5, 
               position = position_nudge(x = 0.035)) +
    geom_point(data = dfplot %>% filter(category == 'Combined' & Status == 'ED') %>% mutate(category = 'Catch-up'),
               aes(x = Cost,
                   y = Effect,
                   fill = PEVage,
                   alpha = Status),
               color = '#ffffff00',
               stroke = 0.01,
               size = 2.4,
               shape = 22,
               position = position_nudge(x = -0.05)) +
    
    ############ With non-dominated scenarios
    # standalone boosters
    geom_point(data = dfplot %>% filter(category == 'Extra booster(s)' & Status == 'ND'),
               aes(x = Cost,
                   y = Effect,
                   color = EPIextra,
                   shape = category,
                   alpha = Status),
               size = 2.5,
               position = position_nudge(x = -0.05)) +
    # standalone catch-up
    geom_point(data = dfplot %>% filter(category == 'Catch-up' & Status == 'ND'),
               aes(x = Cost,
                   y = Effect,
                   fill = PEVage,
                   shape = category,
                   alpha = Status),
               size = 2.4,
               stroke = 0.01,
               color = '#ffffff00',
               position = position_nudge(x = -0.05)) +
    # combined
    geom_point(data = dfplot %>% filter(category == 'Combined' & Status == 'ND') %>% mutate(category = 'Extra booster(s)'),
               aes(x = Cost,
                   y = Effect,
                   color = EPIextra,
                   shape = category,
                   alpha = Status),
               size = 2.5, 
               position = position_nudge(x = 0.035)) +
    geom_point(data = dfplot %>% filter(category == 'Combined' & Status == 'ND') %>% mutate(category = 'Catch-up'),
               aes(x = Cost,
                   y = Effect,
                   fill = PEVage,
                   alpha = Status),
               color = '#ffffff00',
               stroke = 0.01,
               size = 2.4,
               shape = 22,
               position = position_nudge(x = -0.05)) +
    geom_text_repel(data = lab_data,
                    aes(x = Cost, y = Effect, label = labels),
                    size = 2.2,
                    show.legend = FALSE,
                    max.iter = 20000,
                    direction = "both",
                    # min.segment.length = 0,
                    # point.padding = unit(0.5, 'lines'),
                    nudge_x = -0.2)+#,
    # box.padding = unit(1, 'lines')) +
    
    # Define shapes for interventions
    scale_shape_manual(
      name = "Vaccination strategy",
      values = c(#"Routine age-based" = 17, 
        "Extra booster(s)" = 18, "Catch-up" = 22)
    ) +
    # Define alpha values for Status variable 
    scale_alpha_discrete(range = c(0.6,1)) +
    scale_x_continuous(labels = scales::label_comma(),
                       limits = c(minx,maxx)
    ) +
    scale_y_continuous(labels = scales::label_comma(),
                       expand = expansion(mult = 0.1)) +
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
               scales = 'free_y',
               labeller = labeller(pfpr = pfpr.labs)) +
    theme_bw() +
    theme(axis.title = element_text(size = 8),
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 10),
          plot.caption = element_text(size = 10),
          legend.key.size = unit(0.3, 'cm'),
          axis.text.x = element_text(size = 8),
          axis.text.y = element_text(angle = 90, hjust = 0.5, size = 8),
          strip.text = element_text(size = 8),
          plot.margin = margin(t = 2,  # Top margin
                               r = 2,  # Right margin
                               b = 2,  # Bottom margin
                               l = 2),
          legend.position = 'none'
    ) 
  
  return(plt)
}

# Read in legend
legend_img <- grid::rasterGrob(readPNG("R:/Kelly/catchup_extraboosters/src/7_produce_plots/legend2.png"), interpolate=TRUE)

# per 1000 additional doses
CAadd <- eff_plot(casetype = 'clinical',
                  seas_type = seas_type) + 
  labs(x = 'Additional doses per person',
       y = 'Cumulative additional clinical cases\naverted per 1000 population',
       color = 'Vaccination strategy',
       shape = 'Strategy type')
CAlegadd <- plot_grid(CAadd, legend_img, rel_widths = c(4,1))

ggsave(paste0('CAbyadditionaldoses', seas_type, '.pdf'), CAlegadd, width = 7.5, height = 4.5, dpi = 300,            
       units = 'in')#, compression = 'lzw')
ggsave(paste0('CAbyadditionaldoses', seas_type, '.tiff'), CAlegadd, width = 7.5, height = 4.5, 
       dpi = 500, units = 'in')

SAadd <- eff_plot(casetype = 'severe',
                  seas_type = seas_type) + 
  labs(x = 'Additional doses per person',
       y = 'Cumulative additional severe cases\naverted per 1000 population',
       color = 'Vaccination strategy',
       shape = 'Strategy type')
SAlegadd <- plot_grid(SAadd, legend_img, rel_widths = c(4,1))
ggsave(paste0('SAbyadditionaldoses', seas_type,'.pdf'), SAlegadd, width = 7.5, height = 4.5, dpi = 300,            
       units = 'in')#, compression = 'lzw')
ggsave(paste0('SAbyadditionaldoses', seas_type,'.tiff'), SAlegadd, width = 7.5, height = 4.5, 
       dpi = 500,            
       units = 'in')

averted_pltadd <- cowplot::plot_grid(CAadd + theme(legend.position="none"), 
                                     SAadd + theme(legend.position="none"), 
                                     ncol = 1, labels = 'AUTO')
avertedwlegadd <- plot_grid(averted_pltadd, legend_img, 
                            ncol = 2, rel_widths = c(4,1))
ggsave(paste0('CASAbyadditionaldoses', seas_type, '.pdf'), avertedwlegadd,
       width = 9, height = 5.75, dpi = 300,            
       units = 'in')#, compression = 'lzw')
ggsave(paste0('CASAbyadditionaldoses', seas_type, '.tiff'), avertedwlegadd,
       width = 9, height = 5.75, dpi = 500,            
       units = 'in')


# Compare % differences ####################################
df_summ64 <- summ_draws64 %>% 
  select(drawID, age_lower, age_upper, pfpr, seasonality, PEVage, EPIextra, PEVstrategy, 
         cases_averted_perpop, severe_averted_perpop, cases_averted_routine_perpop, severe_averted_routine_perpop,
         cases_averted_routine_peradddose, severe_averted_routine_peradddose) %>%
  filter(age_lower == 0 & age_upper == 100) %>%
  rename()

df_summ1 <- summ_draws1 %>% 
  select(drawID, age_lower, age_upper, pfpr, seasonality, PEVage, EPIextra, PEVstrategy, 
         cases_averted_perpop, severe_averted_perpop, cases_averted_routine_perpop, severe_averted_routine_perpop,
         cases_averted_routine_peradddose, severe_averted_routine_peradddose) %>%
  filter(age_lower == 0 & age_upper == 100)


# Merge datasets by group variables
dfsumm <- df_summ1 %>%
  inner_join(df_summ64, by = c('drawID','pfpr','seasonality','PEVage','EPIextra','PEVstrategy'),
             suffix = c("_1", "_64")) 

numeric_cols <- sapply(df_summ1, is.numeric)
value_vars <- names(df_summ1)[numeric_cols & !(names(df_summ1) %in% c('pfpr','seasonality','PEVage','EPIextra','PEVstrategy'))]

# Calculate percentage differences for value variables
for (var in value_vars) {
  col1 <- paste0(var, "_1")
  col2 <- paste0(var, "_64")
  pct_diff_col <- paste0(var, "_pct_diff")
  abs_diff_col <- paste0(var, "_abs_diff")
  
  if (col1 %in% names(dfsumm) && col2 %in% names(dfsumm)) {
    dfsumm <- dfsumm %>%
      mutate(
        !!abs_diff_col := !!sym(col2) - !!sym(col1),
        !!pct_diff_col := case_when(
          !!sym(col1) == 0 & !!sym(col2) == 0 ~ 0,
          !!sym(col1) == 0 & !!sym(col2) != 0 ~ Inf,
          TRUE ~ round(((!!sym(col2) - !!sym(col1)) / abs(!!sym(col1))) * 100, 2)
        )
      )
  }
}

dfsumm <- dfsumm %>%
  mutate(label = paste0(PEVage, ", ", EPIextra),
         PEVstrategy = case_when(
           PEVstrategy == 'AB' ~ 'AB',
           PEVage !='-' & EPIextra == '-' ~ 'catch-up',
           PEVage!='-' & EPIextra != '-' ~ 'combination',
           TRUE~ PEVstrategy
         ),
         EPIextra = factor(EPIextra, levels = c('-','2y','5y','10y','2y+5y','2y+10y','5y+10y','2y+5y+10y')),
         PEVage = factor(PEVage, levels = c('-','6m-2y','6m-4y','6m-9y','6m-14y','5-9','5-14'))) %>%
  # summarize over the draws
  group_by(pfpr,seasonality,PEVage,EPIextra,PEVstrategy, label) %>%
  summarize(across(c(cases_averted_perpop_abs_diff, cases_averted_perpop_pct_diff, 
                     severe_averted_perpop_abs_diff, severe_averted_perpop_pct_diff,
                     cases_averted_routine_perpop_abs_diff, cases_averted_routine_perpop_pct_diff,
                     severe_averted_routine_perpop_abs_diff, severe_averted_routine_perpop_pct_diff,
                     cases_averted_routine_peradddose_abs_diff, cases_averted_routine_peradddose_pct_diff,
                     severe_averted_routine_peradddose_abs_diff, severe_averted_routine_peradddose_pct_diff),
                   list(lower = ~quantile(.x, 0.025, na.rm = TRUE),
                        median = ~quantile(.x, 0.5, na.rm = TRUE),
                        upper = ~quantile(.x, 0.975, na.rm = TRUE)),
                   .names = "{.col}_{.fn}") ) %>%
  # rename those variables with _median to be just the variable name 
  rename_with(.fn = \(x)sub("_median","", x))

  
p1 <- ggplot(dfsumm %>% filter(seasonality == 'perennial' & PEVstrategy == 'AB')) + 
  geom_col(aes(x = EPIextra, y = cases_averted_perpop_pct_diff,
               fill = as.factor(pfpr)), position = 'dodge', alpha = 0.7) + 
  geom_errorbar(aes(x = EPIextra, ymin = cases_averted_perpop_pct_diff_lower, ymax = cases_averted_perpop_pct_diff_upper,
                    color = as.factor(pfpr)), position = 'dodge', linewidth = 0.5) +
  facet_wrap(~PEVstrategy,
             scales = 'free') + 
  labs(color = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')), 
       x = 'Extra booster dose timing',
       y = 'Difference in cases averted per population (%)',
       fill = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')))

ggsave('ab_pctdiff.pdf', p1,
       width = 9, height = 5.75, dpi = 300,            
       units = 'in')
ggsave('ab_pctdiff.tiff', p1,
       width = 9, height = 5.75, dpi = 500,            
       units = 'in')

p2 <- ggplot(dfsumm %>% filter(seasonality == 'perennial' & PEVstrategy == 'catch-up')) + 
  geom_col(aes(x = PEVage, y = cases_averted_perpop_pct_diff,
               fill = as.factor(pfpr)), position = 'dodge', alpha = 0.7) + 
  geom_errorbar(aes(x = PEVage, ymin = cases_averted_perpop_pct_diff_lower, ymax = cases_averted_perpop_pct_diff_upper,
                    color = as.factor(pfpr)), position = 'dodge', linewidth = 0.5) +
  facet_wrap(~PEVstrategy,
             scales = 'free')+ 
  labs(color = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')), 
       x = 'Catch-up campaign target age group',
       y = 'Difference in cases averted per population (%)',
       fill = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')))
ggsave('cu_pctdiff.pdf', p2,
       width = 9, height = 5.75, dpi = 300,            
       units = 'in')
ggsave('cu_pctdiff.tiff', p2,
       width = 9, height = 5.75, dpi = 500,            
       units = 'in')

p3 <- ggplot(dfsumm %>% filter(seasonality == 'perennial' & PEVstrategy == 'combination')) + 
  geom_col(aes(x = EPIextra, y = cases_averted_perpop_pct_diff,
               fill = as.factor(pfpr)), position = 'dodge', alpha = 0.7) + 
  geom_errorbar(aes(x = EPIextra, ymin = cases_averted_perpop_pct_diff_lower, ymax = cases_averted_perpop_pct_diff_upper,
                    color = as.factor(pfpr)), position = 'dodge', linewidth = 0.5) +
  facet_wrap(~PEVage,
             scales = 'free')+ 
  labs(color = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')), 
       x = 'Extra booster dose timing',
       y = 'Difference in cases averted per population (%)',
       fill = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')))
ggsave('combo_pctdiff.pdf', p3,
       width = 13, height = 5.75, dpi = 300,            
       units = 'in')
ggsave('combo_pctdiff.tiff', p3,
       width = 13, height = 5.75, dpi = 500,            
       units = 'in')

dfsumm %>% ungroup() %>%
  group_by(pfpr, seasonality) %>%
  summarize(mean_pct_diff_CApop = mean(cases_averted_perpop_pct_diff, na.rm = TRUE),
            mean_pct_diff_SApop = mean(severe_averted_perpop_pct_diff, na.rm = TRUE),
            mean_pct_diff_CAadddose = mean(cases_averted_routine_peradddose_pct_diff, na.rm = TRUE),
            mean_pct_diff_SAadddose = mean(severe_averted_routine_peradddose_pct_diff, na.rm = TRUE),
            median_pct_diff_CApop = median(cases_averted_perpop_pct_diff, na.rm = TRUE),
            median_pct_diff_SApop = median(severe_averted_perpop_pct_diff, na.rm = TRUE),
            median_pct_diff_CAadddose = median(cases_averted_routine_peradddose_pct_diff, na.rm = TRUE),
            median_pct_diff_SAadddose = median(severe_averted_routine_peradddose_pct_diff, na.rm = TRUE)) %>% 
  # t() %>% 
  View()




p1 <- ggplot(dfsumm %>% filter(seasonality == 'perennial' & PEVstrategy == 'AB')) + 
  geom_col(aes(x = EPIextra, y = cases_averted_routine_peradddose_pct_diff,
               fill = as.factor(pfpr)), position = 'dodge', alpha = 0.7) + 
  # geom_errorbar(aes(x = EPIextra, ymin = cases_averted_routine_peradddose_pct_diff_lower, ymax = cases_averted_routine_peradddose_pct_diff_upper,
  #                   color = as.factor(pfpr)), position = 'dodge', linewidth = 0.5) +
  facet_wrap(~PEVstrategy,
             scales = 'free') + 
  labs(color = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')), 
       x = 'Extra booster dose timing',
       y = 'Difference in cases averted per additional dose (%)',
       fill = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')))

ggsave('abadddose_pctdiff.pdf', p1,
       width = 9, height = 5.75, dpi = 300,            
       units = 'in')
ggsave('abadddose_pctdiff.tiff', p1,
       width = 9, height = 5.75, dpi = 500,            
       units = 'in')

p2 <- ggplot(dfsumm %>% filter(seasonality == 'perennial' & PEVstrategy == 'catch-up')) + 
  geom_col(aes(x = PEVage, y = cases_averted_routine_peradddose_pct_diff,
               fill = as.factor(pfpr)), position = 'dodge', alpha = 0.7) + 
  # geom_errorbar(aes(x = PEVage, ymin = cases_averted_routine_peradddose_pct_diff_lower, ymax = cases_averted_routine_peradddose_pct_diff_upper,
  #                   color = as.factor(pfpr)), position = 'dodge', linewidth = 0.5) +
  facet_wrap(~PEVstrategy,
             scales = 'free')+ 
  labs(color = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')), 
       x = 'Catch-up campaign target age group',
       y = 'Difference in cases averted per population (%)',
       fill = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')))
ggsave('cuadddose_pctdiff.pdf', p2,
       width = 9, height = 5.75, dpi = 300,            
       units = 'in')
ggsave('cuadddose_pctdiff.tiff', p2,
       width = 9, height = 5.75, dpi = 500,            
       units = 'in')

p3 <- ggplot(dfsumm %>% filter(seasonality == 'perennial' & PEVstrategy == 'combination')) + 
  geom_col(aes(x = EPIextra, y = cases_averted_routine_peradddose_pct_diff,
               fill = as.factor(pfpr)), position = 'dodge', alpha = 0.7) + 
  # geom_errorbar(aes(x = EPIextra, ymin = cases_averted_routine_peradddose_pct_diff_lower, ymax = cases_averted_routine_peradddose_pct_diff_upper,
  #                   color = as.factor(pfpr)), position = 'dodge', linewidth = 0.5) +
  facet_wrap(~PEVage,
             scales = 'free')+ 
  labs(color = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')), 
       x = 'Extra booster dose timing',
       y = 'Difference in cases averted per population (%)',
       fill = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')))
ggsave('combo_pctdiff.pdf', p3,
       width = 13, height = 5.75, dpi = 300,            
       units = 'in')
ggsave('combo_pctdiff.tiff', p3,
       width = 13, height = 5.75, dpi = 500,            
       units = 'in')







































# df_plot1 <- summ_draws1 %>%
#   filter(age_grp == '0-100') %>%
#   # Filter to strategy type
#   filter(PEVstrategy == 'catch-up' | PEVstrategy == 'AB') %>% 
#   filter(!(PEVstrategy %in% c('hybrid', 'SV'))) %>%
#   filter(seasonality == seas_type) %>%
#   filter(pfpr %in% pfpr_vec) %>%
#   mutate(category = ifelse(EPIextra != '-' & PEVage == '-', 'Extra booster(s)', 
#                            ifelse(PEVage != '-' & EPIextra == '-', 'Catch-up',
#                                   ifelse(PEVage == '-' & EPIextra == '-', "Routine age-based", 'Combined'))),
#          EPIextra = factor(EPIextra, levels = c('2y','5y','10y','2y+5y','2y+10y','5y+10y','2y+5y+10y','-')),
#          PEVage = ifelse(PEVage == '5-9','5-9y', ifelse(PEVage == '5-14','5-14y', PEVage)),
#          PEVage = factor(as.factor(PEVage), levels = c('6m-2y','6m-4y','6m-9y','6m-14y','5-9y','5-14y','-')))
# df_plot64 <- summ_draws64 %>%
#   filter(age_grp == '0-100') %>%
#   # Filter to strategy type
#   filter(PEVstrategy == 'catch-up' | PEVstrategy == 'AB') %>% 
#   filter(!(PEVstrategy %in% c('hybrid', 'SV'))) %>%
#   filter(seasonality == seas_type) %>%
#   filter(pfpr %in% pfpr_vec) %>%
#   mutate(category = ifelse(EPIextra != '-' & PEVage == '-', 'Extra booster(s)', 
#                            ifelse(PEVage != '-' & EPIextra == '-', 'Catch-up',
#                                   ifelse(PEVage == '-' & EPIextra == '-', "Routine age-based", 'Combined'))),
#          EPIextra = factor(EPIextra, levels = c('2y','5y','10y','2y+5y','2y+10y','5y+10y','2y+5y+10y','-')),
#          PEVage = ifelse(PEVage == '5-9','5-9y', ifelse(PEVage == '5-14','5-14y', PEVage)),
#          PEVage = factor(as.factor(PEVage), levels = c('6m-2y','6m-4y','6m-9y','6m-14y','5-9y','5-14y','-')))
# 
# 
# booster_colors <- c("2y" = "#c1ef7b" ,"5y" = "#85ecd1", "10y" = "#83bae7" , '2y+5y' = "#acace9","2y+10y" = "#6c8bf7", "5y+10y" = "#4d47d5",  '2y+5y+10y' = "#160e6f")
# 
# catch_up_colors <- c("6m-2y" = "#90b260", "6m-4y" = "#efc642", "6m-9y" = "#fd7270", "6m-14y" = "#ce5800",
#                      "5-9y" = "#991010", "5-14y" = "#65612c")
# 
# 
# 
# eff_plot <- function(var, eff_var){
#   dfpl1 <- df_plot1 %>% filter(.data[[eff_var]] == 1) %>%
#     mutate(dosesper1000 = totaldoses / n *1000)
#   dfpl64 <- df_plot64 %>% filter(.data[[eff_var]] == 1) %>%
#     mutate(dosesper1000 = totaldoses / n *1000)
#   
#   pfpr.labs <- c("5%", "25%", '45%')
#   names(pfpr.labs) <- c("0.05","0.25", "0.45")
#   
#   
#   plt <- ggplot(dfpl64) +
#     # Add grey line for non-scaled version
#     geom_line(data = dfpl1, 
#               aes(x = dosesper1000,
#                   y = .data[[var]]), color = 'grey60',linetype = 2,
#               linewidth = 0.7) +
#     geom_line(data = dfpl64, 
#               aes(x = dosesper1000,
#                   y = .data[[var]]),
#               linewidth = 0.7) +
#     
#     ############ With non-dominated scenarios
#     # Plot standalone routine interventions
#     geom_point(data = dfpl64 %>% filter(category =='Routine age-based'),
#                aes(x = dosesper1000,
#                    y = .data[[var]],
#                    shape = category),
#                color = '#e71d1d',#CUcols[1],
#                size = 4.5) +
#     # standalone boosters
#     geom_point(data = dfpl64 %>% filter(category == 'Extra booster(s)'),
#                aes(x = dosesper1000,
#                    y = .data[[var]],
#                    color = EPIextra,
#                    shape = category),
#                size = 4.5,
#                position = position_nudge(x = -50)) +
#     # standalone catch-up
#     geom_point(data = dfpl64 %>% filter(category == 'Catch-up'),
#                aes(x = dosesper1000,
#                    y = .data[[var]],
#                    fill = PEVage,
#                    shape = category),
#                size = 4,
#                color = '#ffffff00',
#                position = position_nudge(x = -50)) +
#     # combined
#     geom_point(data = dfpl64 %>% filter(category == 'Combined')%>% mutate(category = 'Extra booster(s)'),
#                aes(x = dosesper1000,
#                    y = .data[[var]],
#                    color = EPIextra,
#                    shape = category),
#                size = 4.5,
#                position = position_nudge(x = -50)) +
#     geom_point(data = dfpl64 %>% filter(category == 'Combined') %>% mutate(category = 'Catch-up'),
#                aes(x = dosesper1000,
#                    y = .data[[var]],
#                    fill = PEVage),
#                color = '#ffffff00',
#                size = 4,
#                shape = 22,
#                position = position_nudge(x = -140)) +
#     
#     
#     
#     # Define shapes for interventions
#     scale_shape_manual(
#       name = "Vaccination strategy",
#       values = c("Routine age-based" = 17, "Extra booster(s)" = 18, "Catch-up" = 22)
#     ) +
#     scale_x_continuous(labels = scales::label_comma(), limits = c(2900,5600)) +
#     scale_y_continuous(labels = scales::label_comma(), ) +
#     # Color and fill scales for booster and catch-up with separate legends
#     scale_color_manual(
#       name = "Extra booster(s) timing",
#       values = booster_colors,
#       guide = guide_legend(override.aes = list(shape = 16))
#     ) +
#     scale_fill_manual(
#       name = "Catch-up target age group",
#       values = catch_up_colors,
#       guide = guide_legend(override.aes = list(shape = 22), color = '#ffffff00')
#     ) +
#     facet_wrap(~pfpr,
#                scales = 'free',
#                labeller = labeller(pfpr = pfpr.labs)) +
#     theme_bw(base_size = 14) +
#     theme(axis.title = element_text(size = 20),
#           axis.text.x = element_text(size = 14),
#           axis.text.y = element_text(angle = 90, size = 14),
#           plot.caption = element_text(size = 14),
#           legend.title = element_text(size = 18),
#           legend.text = element_text(size = 14),
#           legend.key.size = unit(0.8, 'cm'),
#           strip.text.x = element_text(size = 12),
#           strip.text.y = element_text(size = 12),
#           legend.position = 'none'
#     ) 
#   return(plt)
# }
# 
# # Read in legend
# legend_img <- grid::rasterGrob(readPNG("R:/Kelly/catchup_extraboosters/src/7_produce_plots/legend2.png"), interpolate=TRUE)
# 
# #Make plots 
# CA <- eff_plot(var = 'cases_averted_perpop', eff_var = 'maxCA') + 
#   labs(x = 'Doses per 1000 population',
#        y = 'Cumulative clinical cases\naverted per 1000 population',
#        color = 'Vaccination strategy',
#        shape = 'Strategy type')
# CAleg <- plot_grid(CA, legend_img, rel_widths = c(4,1))
# 
# ggsave(paste0('CAbytotaldoses', seas_type, '.tiff'), CAleg, height = 5, width = 8.75, dpi = 500)
# 
# SA <- eff_plot(var = 'severe_averted_perpop', eff_var = 'maxSA') + 
#   labs(x = 'Doses per 1000 population',
#        y = 'Cumulative severe cases\naverted per 1000 population',
#        color = 'Vaccination strategy',
#        shape = 'Strategy type')
# SAleg <- plot_grid(SA, legend_img, rel_widths = c(4,1))
# ggsave(paste0('SAbytotaldoses', seas_type,'.pdf'), SAleg, height = 5, width = 8.75, dpi = 500)
# 
# averted_plt <- cowplot::plot_grid(CA + theme(legend.position="none"), 
#                                   SA + theme(legend.position="none"), 
#                                   ncol = 1, labels = 'AUTO')
# avertedwleg <- plot_grid(averted_plt, legend_img, 
#                          ncol = 2, rel_widths = c(4,1))
# ggsave(paste0('CASAbytotaldoses', seas_type, '.pdf'), avertedwleg, height = 5, width = 8.75, dpi = 500)
# 
# 
# 
# 
# 
# 
# 
# 
# 
