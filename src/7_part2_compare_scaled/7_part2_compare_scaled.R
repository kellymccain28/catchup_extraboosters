# Script to make plots to compare the age-scaled and non age-scaled runs 
library(dplyr)
library(ggplot2)
library(orderly)
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
library(ggpattern)

orderly_strict_mode()
orderly::orderly_description('Make figures for manuscript')

orderly_resource('add_lim_labels.R')
source('add_lim_labels.R')

orderly_shared_resource('get_icers.R')
source('get_icers.R')
# Set dependencies 
# orderly::orderly_dependency("5_process_combined",
#                              "latest(parameter:age_scaling == 1)",
#                              c(summarized_overall_draws1.rds = "summarized_overall_draws.rds",
#                                summarized_overall_1.rds = "summarized_overall.rds"))
# orderly::orderly_dependency("5_process_combined",
#                              "latest(parameter:age_scaling == 0.64)",
#                              c(summarized_overall_draws64.rds = "summarized_overall_draws.rds",
#                                summarized_overall_64.rds = "summarized_overall.rds"))


# summ_draws1 <- readRDS("summarized_overall_draws1.rds")
# summ_draws64 <- readRDS("summarized_overall_draws64.rds")
summ_draws64 <- readRDS("R:/Kelly/catchup_extraboosters/archive/7_produce_plots/20250826-151746-e671d9b9/summarized_overall_draws.rds")
summ_draws1 <- readRDS("R:/Kelly/catchup_extraboosters/archive/7_produce_plots/20250826-151746-e67137b1/summarized_overall_draws.rds")
summ_draws04 <- readRDS('R:/Kelly/catchup_extraboosters/archive/5_process_combined/20260204-140719-6e37c9f5/summarized_overall_draws.rds')


# Make efficiency frontier with grey line for old frontier
seas_type = 'perennial'
pfpr_vec = c(0.05, 0.25, 0.45)


dfplot64 <- get_icers(df = summ_draws64)
dfplot1 <- get_icers(df = summ_draws1)
dfplot04 <- get_icers(df = summ_draws04)

# booster_colors <- c("2y" = "#85ecd1" ,"5y" = "#72d5d9", "10y" = "#8abae7", '2y+5y' = "#acace9" ,"2y+10y" = "#6c8bf7", "5y+10y" = "#4d47d5",  '2y+5y+10y' = "#160e6f")
booster_colors <- c("2y" = "#c1ef7b" ,"5y" = "#85ecd1", "10y" = "#83bae7" , '2y+5y' = "#acace9","2y+10y" = "#6c8bf7", "5y+10y" = "#4d47d5",  '2y+5y+10y' = "#160e6f")

# booster_colors <- c("2y" = "#85ece8" ,"5y" = "#5fc8e0", "10y" = "#83bae7" , '2y+5y' = "#acace9","2y+10y" = "#6c8bf7", "5y+10y" = "#4d47d5",  '2y+5y+10y' = "#160e6f")
catch_up_colors <- c("6m-2y" = "#90b260", "6m-4y" = "#efc642", "6m-9y" = "#fd7270", "6m-14y" = "#ce5800",
                     "5-9y" = "#991010", "5-14y" = "#65612c")

eff_plot <- function(casetype, seas_type, scaler = 0.64){
  
  minx = 0
  maxx = 2.35
  
  pfpr.labs <- c("5%", "25%", '45%')
  names(pfpr.labs) <- c("0.05","0.25", "0.45")
  
  dfplot64 <- dfplot64 %>%
    filter(type == casetype) %>%
    filter(seasonality == seas_type)
  
  dfplot1 <- dfplot1 %>%
    filter(type == casetype) %>%
    filter(seasonality == seas_type)
  
  dfplot04 <- dfplot04 %>%
    filter(type == casetype) %>%
    filter(seasonality == seas_type)
  
  
  if(scaler == 0.64){
    dfplot <- dfplot64
  } else if(scaler == 0.4){
    dfplot <- dfplot04
  }
  
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
              linewidth = 0.7) 
  
  if(scaler == 0.4){
    plt <- plt + 
      geom_line(data = dfplot64 %>% filter(Status == 'ND'), 
                aes(x = Cost,
                    y = Effect), color = 'grey60',linetype = 3,
                linewidth = 0.7) 
      
  }
    plt <- plt + 
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
      guide = guide_legend(override.aes = list(shape = 22, color = '#ffffff00'))
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

# 0.64
# per 1000 additional doses
CAadd <- eff_plot(casetype = 'clinical',
                  seas_type = seas_type) + 
  labs(x = 'Additional doses per person',
       y = 'Cumulative additional clinical cases\naverted per 1000 population',
       color = 'Vaccination strategy',
       shape = 'Strategy type')
CAlegadd <- plot_grid(CAadd, legend_img, rel_widths = c(4,1))

ggsave(paste0('CAbyadditionaldoses', seas_type, '_64.pdf'), CAlegadd, width = 7.5, height = 4.5, dpi = 300,            
       units = 'in')#, compression = 'lzw')
ggsave(paste0('CAbyadditionaldoses', seas_type, '_64.tiff'), CAlegadd, width = 7.5, height = 4.5, 
       dpi = 500, units = 'in')

SAadd <- eff_plot(casetype = 'severe',
                  seas_type = seas_type) + 
  labs(x = 'Additional doses per person',
       y = 'Cumulative additional severe cases\naverted per 1000 population',
       color = 'Vaccination strategy',
       shape = 'Strategy type')
SAlegadd <- plot_grid(SAadd, legend_img, rel_widths = c(4,1))
ggsave(paste0('SAbyadditionaldoses', seas_type,'_64.pdf'), SAlegadd, width = 7.5, height = 4.5, dpi = 300,            
       units = 'in')#, compression = 'lzw')
ggsave(paste0('SAbyadditionaldoses', seas_type,'_64.tiff'), SAlegadd, width = 7.5, height = 4.5, 
       dpi = 500,            
       units = 'in')

averted_pltadd <- cowplot::plot_grid(CAadd + theme(legend.position="none"), 
                                     SAadd + theme(legend.position="none"), 
                                     ncol = 1, labels = 'AUTO')
avertedwlegadd <- plot_grid(averted_pltadd, legend_img, 
                            ncol = 2, rel_widths = c(4,1))
ggsave(paste0('CASAbyadditionaldoses', seas_type, '_64.pdf'), avertedwlegadd,
       width = 9, height = 5.75, dpi = 300,            
       units = 'in')#, compression = 'lzw')
ggsave(paste0('CASAbyadditionaldoses', seas_type, '_64.tiff'), avertedwlegadd,
       width = 9, height = 5.75, dpi = 500,            
       units = 'in')

# 0.4
# per 1000 additional doses
CAadd <- eff_plot(casetype = 'clinical',
                  seas_type = seas_type,
                  scaler = 0.4) + 
  labs(x = 'Additional doses per person',
       y = 'Cumulative additional clinical cases\naverted per 1000 population',
       color = 'Vaccination strategy',
       shape = 'Strategy type')
CAlegadd <- plot_grid(CAadd, legend_img, rel_widths = c(4,1))

ggsave(paste0('CAbyadditionaldoses', seas_type, '_4.pdf'), CAlegadd, width = 7.5, height = 4.5, dpi = 300,            
       units = 'in')#, compression = 'lzw')
ggsave(paste0('CAbyadditionaldoses', seas_type, '_4.tiff'), CAlegadd, width = 7.5, height = 4.5, 
       dpi = 500, units = 'in')

SAadd <- eff_plot(casetype = 'severe',
                  seas_type = seas_type,
                  scaler = 0.4) + 
  labs(x = 'Additional doses per person',
       y = 'Cumulative additional severe cases\naverted per 1000 population',
       color = 'Vaccination strategy',
       shape = 'Strategy type')
SAlegadd <- plot_grid(SAadd, legend_img, rel_widths = c(4,1))
ggsave(paste0('SAbyadditionaldoses', seas_type,'_4.pdf'), SAlegadd, width = 7.5, height = 4.5, dpi = 300,            
       units = 'in')#, compression = 'lzw')
ggsave(paste0('SAbyadditionaldoses', seas_type,'_4.tiff'), SAlegadd, width = 7.5, height = 4.5, 
       dpi = 500,            
       units = 'in')

averted_pltadd <- cowplot::plot_grid(CAadd + theme(legend.position="none"), 
                                     SAadd + theme(legend.position="none"), 
                                     ncol = 1, labels = 'AUTO')
avertedwlegadd <- plot_grid(averted_pltadd, legend_img, 
                            ncol = 2, rel_widths = c(4,1))
ggsave(paste0('CASAbyadditionaldoses', seas_type, '_4.pdf'), avertedwlegadd,
       width = 9, height = 5.75, dpi = 300,            
       units = 'in')#, compression = 'lzw')
ggsave(paste0('CASAbyadditionaldoses', seas_type, '_4.tiff'), avertedwlegadd,
       width = 9, height = 5.75, dpi = 500,            
       units = 'in')


# Get table of strategies on eff frontier ----
# Combine all three datasets with scenario identifier
clinical_rankings <- bind_rows(
  dfplot1 %>% mutate(scenario = 'imm1', label = paste0(PEVage, ", ", EPIextra)),
  dfplot64 %>% mutate(scenario = 'imm64', label = paste0(PEVage, ", ", EPIextra)),
  dfplot04 %>% mutate(scenario = 'imm04', label = paste0(PEVage, ", ", EPIextra))
) %>%
  filter(Status == 'ND' & pfpr %in% c(0.05, 0.25, 0.45) & seasonality == 'perennial', type == 'clinical') %>%
  arrange(pfpr, scenario, Cost, Effect) %>%
  group_by(pfpr, scenario) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  select(scenario, pfpr, label, rank) %>%
  mutate(label = gsub("^,\\s*", "", label)) %>%
  pivot_wider(names_from = scenario, values_from = label, values_fill = NA) %>%
  # make nicer for latex 
  mutate(pfpr = scales::percent(pfpr)) %>%
  rename(PfPR = pfpr, 
         `No scaling` = imm1,
         `Scaled to 0.64` = imm64,
         `Scaled to 0.4` = imm04) %>%
  select(-rank)

write.csv(clinical_rankings, 'clinical_rankings.csv', row.names = FALSE)

severe_rankings <- bind_rows(
  dfplot1 %>% mutate(scenario = 'imm1', label = paste0(PEVage, ", ", EPIextra)),
  dfplot64 %>% mutate(scenario = 'imm64', label = paste0(PEVage, ", ", EPIextra)),
  dfplot04 %>% mutate(scenario = 'imm04', label = paste0(PEVage, ", ", EPIextra))
) %>%
  filter(Status == 'ND' & pfpr %in% c(0.05, 0.25, 0.45) & seasonality == 'perennial', type == 'severe') %>%
  arrange(pfpr, scenario, Cost, Effect) %>%
  group_by(pfpr, scenario) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  select(scenario, pfpr, rank, label) %>%
  mutate(label = gsub("^,\\s*", "", label)) %>%
  pivot_wider(names_from = scenario, values_from = label, values_fill = NA) %>%
  # make nicer for latex 
  mutate(pfpr = scales::percent(pfpr)) %>%
  rename(PfPR = pfpr, 
         `No scaling` = imm1,
         `Scaled to 0.64` = imm64,
         `Scaled to 0.4` = imm04) %>%
  select(-rank)

write.csv(severe_rankings, 'severe_rankings.csv', row.names = FALSE)

# Compare % differences 0.64 ####################################
df_summ64 <- summ_draws64 %>% 
  select(drawID, age_lower, age_upper, pfpr, seasonality, PEVage, EPIextra, PEVstrategy, 
         cases_averted_perpop, severe_averted_perpop, cases_averted_routine_perpop, severe_averted_routine_perpop,
         cases_averted_routine_peradddose, severe_averted_routine_peradddose) %>%
  filter(age_lower == 0 & age_upper == 100) 

df_summ1 <- summ_draws1 %>% 
  select(drawID, age_lower, age_upper, pfpr, seasonality, PEVage, EPIextra, PEVstrategy, 
         cases_averted_perpop, severe_averted_perpop, cases_averted_routine_perpop, severe_averted_routine_perpop,
         cases_averted_routine_peradddose, severe_averted_routine_peradddose) %>%
  filter(age_lower == 0 & age_upper == 100)

df_summ04 <- summ_draws04 %>% 
  select(drawID, age_lower, age_upper, pfpr, seasonality, PEVage, EPIextra, PEVstrategy, 
         cases_averted_perpop, severe_averted_perpop, cases_averted_routine_perpop, severe_averted_routine_perpop,
         cases_averted_routine_peradddose, severe_averted_routine_peradddose) %>%
  filter(age_lower == 0 & age_upper == 100)

# Merge datasets by group variables
dfsumm <- df_summ04 %>%
  inner_join(df_summ64, by = c('drawID','pfpr','seasonality','PEVage','EPIextra','PEVstrategy'),
             suffix = c("_04", "_64")) %>%
  inner_join(df_summ1, suffix = c("", "_1")) 

numeric_cols <- sapply(df_summ1, is.numeric)
value_vars <- names(df_summ1)[numeric_cols & !(names(df_summ1) %in% c('drawID','age_lower','age_upper','pfpr','seasonality','PEVage','EPIextra','PEVstrategy'))]

# Calculate percentage differences for value variables
for (var in value_vars) {
  col1 <- var
  col2 <- paste0(var, "_64")
  col3 <- paste0(var, "_04")
  pct_diff_col64 <- paste0(var, "_pct_diff_64")
  abs_diff_col64 <- paste0(var, "_abs_diff_64")
  pct_diff_col4 <- paste0(var, "_pct_diff_04")
  abs_diff_col4 <- paste0(var, "_abs_diff_04")
  
  if (col1 %in% names(dfsumm) && col2 %in% names(dfsumm)) {
    dfsumm <- dfsumm %>%
      mutate(
        !!abs_diff_col64 := !!sym(col2) - !!sym(col1),
        !!pct_diff_col64 := case_when(
          !!sym(col1) == 0 & !!sym(col2) == 0 ~ 0,
          !!sym(col1) == 0 & !!sym(col2) != 0 ~ Inf,
          TRUE ~ round(((!!sym(col2) - !!sym(col1)) / abs(!!sym(col1))) * 100, 2)
        )
      )
  }
  
  if (col1 %in% names(dfsumm) && col3 %in% names(dfsumm)) {
    dfsumm <- dfsumm %>%
      mutate(
        !!abs_diff_col4 := !!sym(col3) - !!sym(col1),
        !!pct_diff_col4 := case_when(
          !!sym(col1) == 0 & !!sym(col3) == 0 ~ 0,
          !!sym(col1) == 0 & !!sym(col3) != 0 ~ Inf,
          TRUE ~ round(((!!sym(col3) - !!sym(col1)) / abs(!!sym(col1))) * 100, 2)
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
  summarize(across(c(contains('diff')),
                   list(lower = ~quantile(.x, 0.025, na.rm = TRUE),
                        median = ~quantile(.x, 0.5, na.rm = TRUE),
                        upper = ~quantile(.x, 0.975, na.rm = TRUE)),
                   .names = "{.col}_{.fn}") ) %>%
  # rename those variables with _median to be just the variable name 
  rename_with(.fn = \(x)sub("_median","", x))


  
p1 <- ggplot(dfsumm %>% filter(seasonality == 'perennial' & PEVstrategy == 'AB')) + 
  geom_col(aes(x = EPIextra, y = cases_averted_perpop_pct_diff_64,
               fill = as.factor(pfpr)), position = 'dodge', alpha = 0.7) + 
  geom_errorbar(aes(x = EPIextra, ymin = cases_averted_perpop_pct_diff_64_lower, ymax = cases_averted_perpop_pct_diff_64_upper,
                    color = as.factor(pfpr)), position = 'dodge', linewidth = 0.5) +
  # facet_wrap(~PEVstrategy,
  #            scales = 'free') + 
  labs(color = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')), 
       x = 'Extra booster dose timing',
       y = 'Difference in cases averted per population (%)',
       fill = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
       title = 'Extra boosters only')+ 
  theme_bw()

ggsave('ab_pctdiff.pdf', p1,
       width = 9, height = 5.75, dpi = 300,            
       units = 'in')
ggsave('ab_pctdiff.tiff', p1,
       width = 9, height = 5.75, dpi = 500,            
       units = 'in')

p2 <- ggplot(dfsumm %>% filter(seasonality == 'perennial' & PEVstrategy == 'catch-up')) + 
  geom_col(aes(x = PEVage, y = cases_averted_perpop_pct_diff_64,
               fill = as.factor(pfpr)), position = 'dodge', alpha = 0.7) + 
  geom_errorbar(aes(x = PEVage, ymin = cases_averted_perpop_pct_diff_64_lower, ymax = cases_averted_perpop_pct_diff_64_upper,
                    color = as.factor(pfpr)), position = 'dodge', linewidth = 0.5) +
  # facet_wrap(~PEVstrategy,
  #            scales = 'free')+ 
  labs(color = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')), 
       x = 'Catch-up campaign target age group',
       y = 'Difference in cases averted per population (%)',
       fill = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
       title = 'Catch-up vaccination only')+ 
  theme_bw()

ggsave('cu_pctdiff.pdf', p2,
       width = 9, height = 5.75, dpi = 300,            
       units = 'in')
ggsave('cu_pctdiff.tiff', p2,
       width = 9, height = 5.75, dpi = 500,            
       units = 'in')

p3 <- ggplot(dfsumm %>% filter(seasonality == 'perennial' & PEVstrategy == 'combination')) + 
  geom_col(aes(x = EPIextra, y = cases_averted_perpop_pct_diff_64,
               fill = as.factor(pfpr)), position = 'dodge', alpha = 0.7) + 
  geom_errorbar(aes(x = EPIextra, ymin = cases_averted_perpop_pct_diff_64_lower, ymax = cases_averted_perpop_pct_diff_64_upper,
                    color = as.factor(pfpr)), position = 'dodge', linewidth = 0.5) +
  facet_wrap(~PEVage,
             scales = 'free')+ 
  labs(color = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')), 
       x = 'Extra booster dose timing',
       y = 'Difference in cases averted per population (%)',
       fill = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
       title = 'Combination of catch-up campaigns and extra boosters') + 
  theme_bw()

ggsave('combo_pctdiff.pdf', p3,
       width = 13, height = 5.75, dpi = 300,            
       units = 'in')
ggsave('combo_pctdiff.tiff', p3,
       width = 13, height = 5.75, dpi = 500,            
       units = 'in')

dfsumm %>% ungroup() %>%
  # group_by(pfpr, seasonality) %>%
  summarize(mean_pct_diff_CApop64 = mean(cases_averted_perpop_pct_diff_64, na.rm = TRUE),
            mean_pct_diff_SApop64 = mean(severe_averted_perpop_pct_diff_64, na.rm = TRUE),
            mean_pct_diff_CAadddose64 = mean(cases_averted_routine_peradddose_pct_diff_64, na.rm = TRUE),
            mean_pct_diff_SAadddose64 = mean(severe_averted_routine_peradddose_pct_diff_64, na.rm = TRUE),
            median_pct_diff_CApop64 = median(cases_averted_perpop_pct_diff_64, na.rm = TRUE),
            median_pct_diff_SApop64 = median(severe_averted_perpop_pct_diff_64, na.rm = TRUE),
            median_pct_diff_CAadddose64 = median(cases_averted_routine_peradddose_pct_diff_64, na.rm = TRUE),
            median_pct_diff_SAadddose64 = median(severe_averted_routine_peradddose_pct_diff_64, na.rm = TRUE),
            
            mean_pct_diff_CApop4 = mean(cases_averted_perpop_pct_diff_04, na.rm = TRUE),
            mean_pct_diff_SApop4 = mean(severe_averted_perpop_pct_diff_04, na.rm = TRUE),
            mean_pct_diff_CAadddose4 = mean(cases_averted_routine_peradddose_pct_diff_04, na.rm = TRUE),
            mean_pct_diff_SAadddose4 = mean(severe_averted_routine_peradddose_pct_diff_04, na.rm = TRUE),
            median_pct_diff_CApop4 = median(cases_averted_perpop_pct_diff_04, na.rm = TRUE),
            median_pct_diff_SApop4 = median(severe_averted_perpop_pct_diff_04, na.rm = TRUE),
            median_pct_diff_CAadddose4 = median(cases_averted_routine_peradddose_pct_diff_04, na.rm = TRUE),
            median_pct_diff_SAadddose4 = median(severe_averted_routine_peradddose_pct_diff_04, na.rm = TRUE)) %>%
  ungroup() %>% summarize(meanclin64 = mean(mean_pct_diff_CApop64), 
                          meanclin4 = mean(mean_pct_diff_CApop4),
                          meansev64 = mean(mean_pct_diff_SApop64), 
                          meansev4 = mean(mean_pct_diff_SApop4),
                          meanclindose64 = mean(mean_pct_diff_CAadddose64), 
                          meanclindose4 = mean(mean_pct_diff_CAadddose4),
                          meansevdose64 = mean(mean_pct_diff_SAadddose64), 
                          meansevdose4 = mean(mean_pct_diff_SAadddose4)) %>%
  t() %>%
  View()




p1 <- ggplot(dfsumm %>% filter(seasonality == 'perennial' & PEVstrategy == 'AB')) + 
  geom_col(aes(x = EPIextra, y = cases_averted_routine_peradddose_pct_diff_64,
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
  geom_col(aes(x = PEVage, y = cases_averted_routine_peradddose_pct_diff_64,
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
  geom_col(aes(x = EPIextra, y = cases_averted_routine_peradddose_pct_diff_64,
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






# Compare % differences 0.4  ####################################

p1 <- ggplot(dfsumm %>% filter(seasonality == 'perennial' & PEVstrategy == 'AB')) + 
  geom_col(aes(x = EPIextra, y = cases_averted_perpop_pct_diff_04,
               fill = as.factor(pfpr)), position = 'dodge', alpha = 0.7) + 
  geom_errorbar(aes(x = EPIextra, ymin = cases_averted_perpop_pct_diff_04_lower, ymax = cases_averted_perpop_pct_diff_04_upper,
                    color = as.factor(pfpr)), position = 'dodge', linewidth = 0.5) +
  facet_wrap(~PEVstrategy,
             scales = 'free') + 
  labs(color = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')), 
       x = 'Extra booster dose timing',
       y = 'Difference in cases averted per population (%)',
       fill = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')))

ggsave('ab_pctdiff_04.pdf', p1,
       width = 9, height = 5.75, dpi = 300,            
       units = 'in')
ggsave('ab_pctdiff_04.tiff', p1,
       width = 9, height = 5.75, dpi = 500,            
       units = 'in')

p2 <- ggplot(dfsumm %>% filter(seasonality == 'perennial' & PEVstrategy == 'catch-up')) + 
  geom_col(aes(x = PEVage, y = cases_averted_perpop_pct_diff_04,
               fill = as.factor(pfpr)), position = 'dodge', alpha = 0.7) + 
  geom_errorbar(aes(x = PEVage, ymin = cases_averted_perpop_pct_diff_04_lower, ymax = cases_averted_perpop_pct_diff_04_upper,
                    color = as.factor(pfpr)), position = 'dodge', linewidth = 0.5) +
  facet_wrap(~PEVstrategy,
             scales = 'free')+ 
  labs(color = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')), 
       x = 'Catch-up campaign target age group',
       y = 'Difference in cases averted per population (%)',
       fill = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')))
ggsave('cu_pctdiff_04.pdf', p2,
       width = 9, height = 5.75, dpi = 300,            
       units = 'in')
ggsave('cu_pctdiff_04.tiff', p2,
       width = 9, height = 5.75, dpi = 500,            
       units = 'in')

p3 <- ggplot(dfsumm %>% filter(seasonality == 'perennial' & PEVstrategy == 'combination')) + 
  geom_col(aes(x = EPIextra, y = cases_averted_perpop_pct_diff_04,
               fill = as.factor(pfpr)), position = 'dodge', alpha = 0.7) + 
  geom_errorbar(aes(x = EPIextra, ymin = cases_averted_perpop_pct_diff_04_lower, ymax = cases_averted_perpop_pct_diff_04_upper,
                    color = as.factor(pfpr)), position = 'dodge', linewidth = 0.5) +
  facet_wrap(~PEVage,
             scales = 'free')+ 
  labs(color = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')), 
       x = 'Extra booster dose timing',
       y = 'Difference in cases averted per population (%)',
       fill = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')))
ggsave('combo_pctdiff_04.pdf', p3,
       width = 13, height = 5.75, dpi = 300,            
       units = 'in')
ggsave('combo_pctdiff_04.tiff', p3,
       width = 13, height = 5.75, dpi = 500,            
       units = 'in')

# dfsumm %>% ungroup() %>%
#   group_by(pfpr, seasonality) %>%
#   summarize(mean_pct_diff_CApop = mean(cases_averted_perpop_pct_diff, na.rm = TRUE),
#             mean_pct_diff_SApop = mean(severe_averted_perpop_pct_diff, na.rm = TRUE),
#             mean_pct_diff_CAadddose = mean(cases_averted_routine_peradddose_pct_diff, na.rm = TRUE),
#             mean_pct_diff_SAadddose = mean(severe_averted_routine_peradddose_pct_diff, na.rm = TRUE),
#             median_pct_diff_CApop = median(cases_averted_perpop_pct_diff, na.rm = TRUE),
#             median_pct_diff_SApop = median(severe_averted_perpop_pct_diff, na.rm = TRUE),
#             median_pct_diff_CAadddose = median(cases_averted_routine_peradddose_pct_diff, na.rm = TRUE),
#             median_pct_diff_SAadddose = median(severe_averted_routine_peradddose_pct_diff, na.rm = TRUE)) %>% 
#   # t() %>% 
#   View()




p1 <- ggplot(dfsumm %>% filter(seasonality == 'perennial' & PEVstrategy == 'AB')) + 
  geom_col(aes(x = EPIextra, y = cases_averted_routine_peradddose_pct_diff_04,
               fill = as.factor(pfpr)), position = 'dodge', alpha = 0.7) + 
  facet_wrap(~PEVstrategy,
             scales = 'free') + 
  labs(color = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')), 
       x = 'Extra booster dose timing',
       y = 'Difference in cases averted per additional dose (%)',
       fill = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')))

ggsave('abadddose_pctdiff_04.pdf', p1,
       width = 9, height = 5.75, dpi = 300,            
       units = 'in')
ggsave('abadddose_pctdiff_04.tiff', p1,
       width = 9, height = 5.75, dpi = 500,            
       units = 'in')

p2 <- ggplot(dfsumm %>% filter(seasonality == 'perennial' & PEVstrategy == 'catch-up')) + 
  geom_col(aes(x = PEVage, y = cases_averted_routine_peradddose_pct_diff_04,
               fill = as.factor(pfpr)), position = 'dodge', alpha = 0.7) + 
  facet_wrap(~PEVstrategy,
             scales = 'free')+ 
  labs(color = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')), 
       x = 'Catch-up campaign target age group',
       y = 'Difference in cases averted per population (%)',
       fill = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')))
ggsave('cuadddose_pctdiff_04.pdf', p2,
       width = 9, height = 5.75, dpi = 300,            
       units = 'in')
ggsave('cuadddose_pctdiff_04.tiff', p2,
       width = 9, height = 5.75, dpi = 500,            
       units = 'in')

p3 <- ggplot(dfsumm %>% filter(seasonality == 'perennial' & PEVstrategy == 'combination')) + 
  geom_col(aes(x = EPIextra, y = cases_averted_routine_peradddose_pct_diff_04,
               fill = as.factor(pfpr)), position = 'dodge', alpha = 0.7) + 
  facet_wrap(~PEVage,
             scales = 'free')+ 
  labs(color = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')), 
       x = 'Extra booster dose timing',
       y = 'Difference in cases averted per population (%)',
       fill = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')))
ggsave('combo_pctdiff_04.pdf', p3,
       width = 13, height = 5.75, dpi = 300,            
       units = 'in')
ggsave('combo_pctdiff_04.tiff', p3,
       width = 13, height = 5.75, dpi = 500,            
       units = 'in')











# Put both sensitivity analyses on same plot ----
dfsumm_long <- dfsumm %>%
  pivot_longer(
    cols = c(cases_averted_perpop_pct_diff_04, 
             cases_averted_perpop_pct_diff_04_lower, 
             cases_averted_perpop_pct_diff_04_upper,
             cases_averted_perpop_pct_diff_64, 
             cases_averted_perpop_pct_diff_64_lower, 
             cases_averted_perpop_pct_diff_64_upper),
    names_to = c("scenario", "bound"),
    names_pattern = "cases_averted_perpop_pct_diff_(04|64)(.*)",
    values_to = "value"
  ) %>%
  mutate(
    bound = case_when(
      bound == "" ~ "estimate",
      bound == "_lower" ~ "lower",
      bound == "_upper" ~ "upper"
    )
  ) %>%
  pivot_wider(names_from = bound, values_from = value)

p1 <- ggplot(dfsumm_long %>% filter(seasonality == 'perennial' & PEVstrategy == 'AB'), 
       aes(x = EPIextra, y = estimate, fill = as.factor(pfpr), group = interaction(scenario, pfpr))) + 
  geom_col_pattern(aes(pattern = scenario),
                   position = position_dodge(width = 0.9), 
                   alpha = 0.7,
                   pattern_fill = 'black',
                   pattern_color = 'black',
                   pattern_density = 0.5,
                   pattern_spacing = 0.015
                   ) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = as.factor(pfpr), group = interaction(scenario, pfpr)),
                position = position_dodge(width = 0.9), 
                linewidth = 0.5,
                width = 0.5) +
  scale_pattern_manual(values = c('04' = 'none', '64' = 'stripe'),
                       labels = c('04' = 'Scaler: 0.4',
                                  '64' = 'Scaler: 0.64')
                       ) +
  facet_wrap(~PEVstrategy, scales = 'free') + 
  labs(color = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')), 
       x = 'Extra booster dose timing',
       y = 'Difference in cases averted per population (%)',
       fill = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
       pattern = 'Scenario',
       title = 'Extra boosters') + 
  theme_bw(base_size = 14)

ggsave('ab_pctdiff_6404.pdf', p1,
       width = 10, height = 6, dpi = 300,            
       units = 'in')
ggsave('ab_pctdiff_6404.tiff', p1,
       width = 10, height = 6, dpi = 500,            
       units = 'in')
ggsave('ab_pctdiff_6404.png', p1,
       width = 10, height = 6, dpi = 500,            
       units = 'in')

p2 <- ggplot(dfsumm_long %>% filter(seasonality == 'perennial' & PEVstrategy == 'catch-up'), 
             aes(x = PEVage, y = estimate, fill = as.factor(pfpr), group = interaction(scenario, pfpr))) + 
  geom_col_pattern(aes(pattern = scenario),
                   position = position_dodge(width = 0.9), 
                   alpha = 0.7,
                   pattern_fill = 'black',
                   pattern_color = 'black',
                   pattern_density = 0.5,
                   pattern_spacing = 0.015
  ) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = as.factor(pfpr), group = interaction(scenario, pfpr)),
                position = position_dodge(width = 0.9), 
                linewidth = 0.5,
                width = 0.5) +
  scale_pattern_manual(values = c('04' = 'none', '64' = 'stripe'),
                       labels = c('04' = 'Scaler: 0.4',
                                  '64' = 'Scaler: 0.64')) +
  facet_wrap(~PEVstrategy, scales = 'free') + 
  labs(color = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')), 
       x = 'Catch-up campaign target age group',
       y = 'Difference in cases averted per population (%)',
       fill = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
       pattern = 'Scenario',
       title = 'Catch-up campaigns') + 
  theme_bw(base_size = 14)

ggsave('cu_pctdiff_6404.pdf', p2,
       width = 10, height = 6.5, dpi = 300,            
       units = 'in')
ggsave('cu_pctdiff_6404.tiff', p2,
       width = 10, height = 6.5, dpi = 500,            
       units = 'in')
ggsave('cu_pctdiff_6404.png', p2,
       width = 10, height = 6.5, dpi = 500,            
       units = 'in')


p3 <- ggplot(dfsumm_long %>% filter(seasonality == 'perennial' & PEVstrategy == 'combination'), 
             aes(x = EPIextra, y = estimate, fill = as.factor(pfpr), group = interaction(scenario, pfpr))) + 
  geom_col_pattern(aes(pattern = scenario),
                   position = position_dodge(width = 0.9), 
                   alpha = 0.7,
                   pattern_fill = 'black',
                   pattern_color = 'black',
                   pattern_density = 0.5,
                   pattern_spacing = 0.015
  ) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = as.factor(pfpr), group = interaction(scenario, pfpr)),
                position = position_dodge(width = 0.9), 
                linewidth = 0.5,
                width = 0.5) +
  scale_pattern_manual(values = c('04' = 'none', '64' = 'stripe'),
                       labels = c('04' = 'Scaler: 0.4',
                                  '64' = 'Scaler: 0.64')) +
  facet_wrap(~PEVage, scales = 'free') + 
  labs(color = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')), 
       x = 'Extra booster dose timing',
       y = 'Difference in cases averted per population (%)',
       fill = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
       pattern = 'Scenario',
       title = 'Combination scenarios') + 
  theme_bw(base_size = 14)

ggsave('combo_pctdiff_6404.pdf', p3,
       width = 24, height = 14, dpi = 300,            
       units = 'in')
ggsave('combo_pctdiff_6404.tiff', p3,
       width = 24, height = 14, dpi = 500,            
       units = 'in')
ggsave('combo_pctdiff_6404.png', p3,
       width = 24, height = 14, dpi = 500,            
       units = 'in')















