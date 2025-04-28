df_summ <- readRDS("R:/Kelly/catchup_extraboosters/archive/5_process_combined/20241022-154945-133bb6db/summarized_overall.rds")
df
df_plot <- df %>%
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

dfpl1 <- df_plot%>%
    mutate(dosesper1000 = totaldoses / n *1000)

# dfpl1 <- df_plot %>% filter(.data[[eff_var]] == 1) %>%
#   mutate(dosesper1000 = totaldoses / n *1000)
eff_var = 'maxCA'
var = 'cases_averted_perpop'

# booster_colors <- c("2y" = "#c1ef7b" ,"5y" = "#73C6B6", "10y" = "#9fc5e8", "2y+10y" = "#C3C3E6" ,'2y+5y' = "#6380e5", "5y+10y" = "#403bbd",  '2y+5y+10y' = "#160e6f")
# booster_colors <- c("2y" = "#85ecd1" ,"5y" = "#72d5d9", "10y" = "#8abae7", "2y+10y" = "#acace9" ,'2y+5y' = "#6c8bf7", "5y+10y" = "#4d47d5",  '2y+5y+10y' = "#160e6f")
# 
# booster_colors <- c("2y" = "#85ece8" ,"5y" = "#5fc8e0", "10y" = "#83bae7" , '2y+5y' = "#acace9","2y+10y" = "#6c8bf7", "5y+10y" = "#4d47d5",  '2y+5y+10y' = "#160e6f")
booster_colors <- c("2y" = "#c1ef7b" ,"5y" = "#85ecd1", "10y" = "#83bae7" , '2y+5y' = "#acace9","2y+10y" = "#6c8bf7", "5y+10y" = "#4d47d5",  '2y+5y+10y' = "#160e6f")
catch_up_colors <- c("6m-2y" = "#90b260", "6m-4y" = "#efc642", "6m-9y" = "#fd7270", "6m-14y" = "#ce5800",
                     "5-9y" = "#991010", "5-14y" = "#65612c")

plt <- ggplot(dfpl1) +
  geom_line(aes(x = dosesper1000,
                y = .data[[var]]),
            linewidth = 0.7) +
  # Plot standalone routine interventions
  geom_point(data = dfpl1 ,#%>% filter(category =='Routine age-based'),
             aes(x = dosesper1000,
                 y = .data[[var]],
                 shape = category),
             color = CUcols[1],
             size = 3.5) +
  # standalone boosters
  geom_point(data = dfpl1 ,# %>% filter(category == 'Extra booster(s)'),
             aes(x = dosesper1000,
                 y = .data[[var]],
                 color = EPIextra,
                 shape = category),
             size = 3.5,
             position = position_nudge(x = 90)) +
  # standalone catch-up
  geom_point(data = dfpl1 ,# %>% filter(category == 'Catch-up'),
             aes(x = dosesper1000,
                 y = .data[[var]],
                 fill = PEVage,
                 shape = category),
             size = 3,
             #color = '#ffffff00',
             position = position_nudge(x = -90)) +
  # combined
  geom_point(data = dfpl1  ,#%>% filter(category == 'Combined')%>% mutate(category = 'Extra booster(s)'),
             aes(x = dosesper1000,
                 y = .data[[var]],
                 color = EPIextra,
                 shape = category),
             size = 4,
             position = position_nudge(x = 90)) +
  geom_point(data = dfpl1  ,#%>% filter(category == 'Combined') %>% mutate(category = 'Catch-up'),
             aes(x = dosesper1000,
                 y = .data[[var]],
                 fill = PEVage),
             #color = '#ffffff00',
             size = 3,
             shape = 22,
             position = position_nudge(x = -90)) +
  
  # Define shapes for interventions
  scale_shape_manual(
    name = "Vaccination strategy",
    values = c("Routine age-based" = 17, "Extra booster(s)" = 18, "Catch-up" = 22),
    guide = guide_legend(override.aes = list(size =5))
  ) +
  scale_x_continuous(labels = scales::label_comma()) +
  # Color and fill scales for booster and catch-up with separate legends
  scale_color_manual(
    name = "Extra booster(s) timing",
    values = booster_colors,
    guide = guide_legend(override.aes = list(shape = 18, size = 5))
  ) +
  scale_fill_manual(
    name = "Catch-up target age group",
    values = catch_up_colors,
    guide = guide_legend(override.aes = list(shape = 22, size = 5, color = '#ffffff00'))
  ) +
  # facet_wrap(~pfpr,
  #            scales = 'free',
  #            labeller = labeller(pfpr = pfpr.labs)) + 
  theme_bw(base_size = 12) +
  theme(axis.title = element_text(size = 12),
        plot.title = element_text(size = 18),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        # legend.position = 'bottom',
        strip.text.x = element_text(size = 10),
        strip.text.y = element_text(size = 10),
        plot.caption = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)
  )

plt
# pull out legend
legend <- cowplot::get_plot_component(
  # create some space to the left of the legend
  plt + theme(legend.box.margin = margin(0, 0, 0, 12)),
  'guide-box-right', return_all = TRUE)

ggsave("legend.tiff", plot = legend, width = 2.3, height = 5.5) 
