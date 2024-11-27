# Script to plot percent of cases / severe cases averted by scenario

# x = pfpr
# y = percent averted
# ribbon and lines 
# faceted by seasonality and PEVage 

#' @param df summarized_overall_draws.rds - overall dataset with percent of cases averted
plot_perc_averted <- function(df){
  
  dfpl <- df %>%
    filter(PEVstrategy != 'SV' & PEVstrategy != 'hybrid')
 
  #Clinical
  plt <- ggplot(dfpl %>%
           dplyr::filter(seasonality == 'seasonal') %>%
           mutate(PEVage = ifelse(PEVage == '-','Age-based',PEVage))) +
    geom_ribbon(aes(x = as.factor(pfpr), ymin = p_CA_lower, ymax = p_CA_upper, fill = labels, group = labels), 
                alpha = 0.2) +
    geom_line(aes(x = as.factor(pfpr), y = p_CA, color = labels, group = labels), linewidth = 1, alpha = 0.9) + 
    # scale_x_continuous(breaks = c(0.01, 0.03, 0.05,0.25, 0.45, 0.65)) +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    scale_y_continuous(labels = scales::percent) +
    facet_wrap(~ PEVage, nrow = 2) + 
    labs(y = 'Percentage of uncomplicated\ncases averted',
         x = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
         fill = 'Vaccination strategy',
         color = 'Vaccination strategy') +
    theme_bw(base_size = 14) + 
    theme(axis.title = element_text(size = 20),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(angle = 90, size = 14),
          plot.caption = element_text(size = 14),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 14),
          legend.key.size = unit(0.8, 'cm'),
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 12))
  ggsave("plots/plot_perc_uncomplicated_averted.pdf", width = 16, height = 8) 
  
  
  # heat <- ggplot(dfpl %>%
  #                  # filter(pfpr %in% c(0.01,0.25,0.65)) %>%
  #                  mutate(PEVage = ifelse(PEVage == '-','Age-based',PEVage),
  #                         labels = factor(labels, levels = c('Age-based;\n12m booster','6m-2y;\n12m booster','6m-4y;\n12m booster','6m-9y;\n12m booster',
  #                                                            '6m-14y;\n12m booster','5-9y;\n12m booster', '5-14y;\n12m booster',
  #                                                            
  #                                                            'Age-based;\n12m, 5y boosters','6m-2y;\n12m, 5y boosters','6m-4y;\n12m, 5y boosters',
  #                                                            '6m-9y;\n12m, 5y boosters','6m-14y;\n12m, 5y boosters','5-9y;\n12m, 5y boosters','5-14y;\n12m, 5y boosters',
  #                                                            
  #                                                            'Age-based;\n12m, 10y boosters','6m-2y;\n12m, 10y boosters','6m-4y;\n12m, 10y boosters','6m-9y;\n12m, 10y boosters',
  #                                                            '6m-14y;\n12m, 10y boosters','5-9y;\n12m, 10y boosters','5-14y;\n12m, 10y boosters',
  #                                                            
  #                                                            'Age-based;\n12m, 5y, 10y boosters','6m-2y;\n12m, 5y, 10y boosters','6m-4y;\n12m, 5y, 10y boosters','6m-9y;\n12m, 5y, 10y boosters',
  #                                                            '6m-14y;\n12m, 5y, 10y boosters','5-9y;\n12m, 5y, 10y boosters','5-14y;\n12m, 5y, 10y boosters')))) +
  #   geom_tile(aes(x = as.factor(pfpr), y = labels, fill = round(p_CA*100, 1)),
  #             color = "white",
  #             lwd = 0.6,
  #             linetype = 1) + 
  #   geom_hline(aes(yintercept = 7.5), lwd = 2) +
  #   geom_hline(aes(yintercept = 14.5), lwd = 2) +
  #   geom_hline(aes(yintercept = 21.5), lwd = 2) +
  #   facet_wrap(~seasonality)+
  #   scale_fill_viridis(discrete = FALSE) + theme_minimal() + 
  #   labs(y = 'Vaccination strategy',
  #        x = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
  #        fill = 'Percentage of\nuncomplicated\ncases averted') +
  #   theme(axis.title = element_text(size = 18),
  #         plot.title = element_text(size = 22),
  #         legend.text = element_text(size = 13),
  #         legend.title = element_text(size = 18),
  #         legend.key.size = unit(0.8, 'cm'),
  #         axis.text = element_text(size = 13),
  #         strip.text = element_text(size = 18))
  # ggsave("plots/heatmap_perc_uncomplicated_averted.pdf", width = 16, height = 12) 
  # coord_fixed()
  
  # leg <- get_legend(plt)
  # 
  # plot <- plot_grid(plt + theme(legend.position = 'none'), leg, rel_widths = c(4, 0.7))
  # plot <- grid.draw(shift_legend(plt))
  # pdf(plot, filename ="plots/plot_perc_uncomplicated_averted.pdf")
  # dev.off()
  #Severe
  # ggplot(dfpl %>%
  #          filter(seasonality == 'seasonal')) +
  #   geom_ribbon(aes(x = pfpr, ymin = p_SA_lower, ymax = p_SA_upper, fill = labels, group = labels), alpha = 0.3) +
  #   geom_line(aes(x = pfpr, y = p_SA, color = labels, group = labels), linewidth = 1) + 
  #   # scale_x_continuous(breaks = c(0.01, 0.03, 0.05,0.25, 0.45, 0.65)) +
  #   scale_fill_manual(values = colors) +
  #   scale_color_manual(values = colors) +
  #   facet_wrap(~ PEVage, nrow = 2) + 
  #   theme_bw()
  
  # get table of percentages averted 
  dftbl <- dfpl %>%
    # filter(pfpr %in% c(0.05, 0.25, 0.45)) %>%
    filter(age_grp == '0-100') %>%
    select(labels, #PEVstrategy, PEVage, EPIextra, 
           pfpr, seasonality, age_grp,
           p_CA_lower, p_CA, p_CA_upper, p_SA_lower, p_SA, p_SA_upper,
          ) %>% ungroup() %>%
    group_by(pfpr, seasonality) %>%
    arrange(desc(p_CA), .by_group = TRUE) %>%
    mutate(across(c(p_CA_lower, p_CA, p_CA_upper, p_SA_lower, p_SA, p_SA_upper), ~ round(.x * 100, 0))) %>%
    mutate(`Percent of cases averted in whole population` = paste0(p_CA, ' (', p_CA_lower, ', ', p_CA_upper, ')'),
           `Percent of severe cases averted in whole population` = paste0(p_SA, ' (', p_SA_lower, ', ', p_SA_upper, ')')) %>%
    select(-c(p_CA_lower:p_SA_upper), -age_grp)

  write.csv(dftbl, 'plots/table_perc_outcomes_averted_all.csv', row.names = FALSE)
  
  # get table of percentages averted 
  dftbl2 <- dfpl %>%
    # filter(pfpr %in% c(0.05, 0.25, 0.45)) %>%
    filter(age_grp == '0-5') %>%
    select(labels, #PEVstrategy, PEVage, EPIextra, 
           pfpr, seasonality, age_grp,
           p_CA_lower, p_CA, p_CA_upper, p_SA_lower, p_SA, p_SA_upper,
           # cases_averted_perpop_lower, cases_averted_perpop, cases_averted_perpop_upper
    ) %>%
    # mutate(PEVage = ifelse(PEVage == '5-9' | PEVage == '5-14', paste0(PEVage,'y'), PEVage)) %>%
    mutate(across(c(p_CA_lower, p_CA, p_CA_upper, p_SA_lower, p_SA, p_SA_upper), ~ round(.x * 100, 0))) %>%
    mutate(`Percent of cases averted in U5s` = paste0(p_CA, ' (', p_CA_lower, ', ', p_CA_upper, ')'),
           `Percent of severe cases averted in U5s` = paste0(p_SA, ' (', p_SA_lower, ', ', p_SA_upper, ')')) %>%
    select(-c(p_CA_lower:p_SA_upper), -age_grp)

  write.csv(dftbl2, 'plots/table_perc_outcomes_averted_U5.csv', row.names = FALSE)
  
  
  dfcombine <- dftbl %>%
    left_join(dftbl2, by = c('labels', 'pfpr', 'seasonality')) 
  write.csv(dfcombine, 'plots/table_perc_outcomes_averted_allstrategies.csv', row.names = FALSE)
  
  dfcombine <- dftbl %>%
    left_join(dftbl2, by = c('labels', 'pfpr', 'seasonality')) %>%
    filter(pfpr %in% c(0.05, 0.25, 0.45)) %>%
    filter((grepl('booster', labels) | labels == 'Routine age-based') & (labels != '2y booster' & labels != '5y booster' & labels != '10y booster' & labels != '5y, 10y boosters'))
  write.csv(dfcombine, 'plots/table_perc_outcomes_averted.csv', row.names = FALSE)
}
