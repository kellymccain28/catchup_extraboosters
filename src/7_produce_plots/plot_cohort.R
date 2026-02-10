# Function to plot age-based cohorts over age (time from vaccination)
#' @param df will be cohorts_byage
plot_cohort <- function(df,
                        seas = 'seasonal'){
  
  # Make labels for facets
  pfpr.labs <- c("5%", "25%", '45%', '65%')
  names(pfpr.labs) <- c("0.05","0.25", "0.45", "0.65")
  cohortcols <- CUcols1[1:8]#c('#B03A2E','#6C3483','#1F618D','#00796b','#fbc02d','#CA6F1E','#689f38', '#3498db')
  make_co_plot <- function(var,
                           strat){
    
    # Get medians of each of the scenarios (will have multiple values for each age group as there are multiple cohorts)
    cohorts_plot <- cohorts_byage %>%
      mutate(age_lower = as.numeric(age_lower)) %>%
      filter(PEVstrategy == strat) |>
      filter(seasonality == seas & PEVstrategy != 'none' & pfpr != 0.01 & pfpr != 0.03) 
    
    ggplot(data = cohorts_plot) +
      # geom_ribbon(aes(x = age_lower, 
      #                 ymin = .data[[paste0(var,'_lower')]], 
      #                 ymax = .data[[paste0(var,'_upper')]], 
      #                 fill = labels),
      #             alpha = 0.15) +
      geom_line(aes(x = age_lower, 
                    y = .data[[var]], 
                    color = labels)) +
      geom_line(aes(x = age_lower, 
                    y = .data[[var]], 
                    color = labels), 
                linewidth = 1) +
      scale_color_manual(values = if(strat == 'AB') {cohortcols} else if(strat == 'catch-up'){colsCU}) +
      scale_fill_manual(values = if(strat == 'AB') {cohortcols} else if(strat == 'catch-up'){colsCU}) +
      scale_x_continuous(breaks = seq(0,30,2)) +
      geom_hline(yintercept = 0, 
                 linetype = 2) + 
      facet_wrap(~ pfpr , scales = 'free',
                 labeller = labeller(pfpr = pfpr.labs)) + 
      theme_bw(base_size = 14) +
      theme(axis.title = element_text(size = 8),
            # plot.title = element_text(size = 22),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 10),
            plot.caption = element_text(size = 10),
            legend.key.size = unit(0.3, 'cm'),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(angle = 90, size = 8),
            strip.text = element_text(size = 8),
            plot.margin = margin(t = 2,  # Top margin
                                 r = 2,  # Right margin
                                 b = 2,  # Bottom margin
                                 l = 2))
  }
  
  makeplots <- function(strat){# PER POP
    cases_cohort <- make_co_plot(var = 'cases_averted_perpop',
                                 strat = strat) + 
      labs(y = 'Clinical cases averted per 1000 people',
           x = 'Age (years)',
           fill = 'Vaccination strategy',
           color = 'Vaccination strategy')
    
    ggsave(paste0('plots/cohorts_', strat, '_CA_perpop_', seas, '.pdf'), 
           height = 4.3, width = 7.5, dpi = 300,          
           units = 'in')
    ggsave(paste0('plots/cohorts_', strat, '_CA_perpop_', seas, '.svg'), 
           height = 4.3, width = 7.5, dpi = 300,          
           units = 'in')
    # ggsave(paste0('plots/cohorts_', strat, '_CA_perpop_', seas, '.png'), height = 4.3, width = 7.5, dpi = 300,
    #        units = 'in')
    
    sev_cohort <- make_co_plot(var = 'severe_averted_perpop',
                               strat = strat) + 
      labs(y = 'Severe cases averted per 1000 people',
           x = 'Age (years)',
           fill = 'Vaccination strategy',
           color = 'Vaccination strategy')
    
    ggsave(paste0('plots/cohorts_', strat,'_SA_perpop_', seas, '.pdf'), height = 4.3, width = 7.5, dpi = 300,          
           units = 'in')
    ggsave(paste0('plots/cohorts_', strat,'_SA_perpop_', seas, '.svg'), height = 4.3, width = 7.5, dpi = 300,          
           units = 'in')
    # ggsave(paste0('plots/cohorts_', strat,'_SA_perpop_', seas, '.png'), height = 4.3, width = 7.5, dpi = 300,          
    #        units = 'in')
    
    casesandsevcohort <- cowplot::plot_grid(cases_cohort + theme(legend.position="none"), 
                                            sev_cohort + theme(legend.position="none"), 
                                            ncol = 1, labels = 'AUTO')
    
    # extract the legend from one of the plots
    legend <- cowplot::get_plot_component(
      # create some space to the left of the legend
      cases_cohort + theme(legend.box.margin = margin(0, 0, 0, 8)),
      'guide-box-right', return_all = TRUE
    )
    
    # add the legend to the row we made earlier. Give it one-third of 
    # the width of one plot (via rel_widths).
    casesandsevcohort <- plot_grid(casesandsevcohort, legend, 
                                   ncol = 2, rel_widths = c(3.2, 1.1))
    
    ggsave(paste0('plots/cohorts_',strat,'_CAandSA_perpop_', seas, '.pdf'), casesandsevcohort, 
           width = 10, height = 8, dpi = 300)
    ggsave(paste0('plots/cohorts_',strat,'_CAandSA_perpop_', seas, '.svg'), casesandsevcohort, 
           width = 10, height = 8, dpi = 300)
    
    # OVERALL COUNTS 
    cases_cohort <- make_co_plot(var = 'cases_averted',
                                 strat = strat) + 
      labs(y = 'Cumulative clinical cases averted',
           x = 'Age (years)',
           fill = 'Vaccination strategy',
           color = 'Vaccination strategy')
    
    ggsave(paste0('plots/cohorts_',strat, '_CA_overall_', seas, '.pdf'), height = 4.3, width = 7.5, dpi = 300,          
           units = 'in')#, compression = 'lzw')
    ggsave(paste0('plots/cohorts_',strat, '_CA_overall_', seas, '.svg'), height = 4.3, width = 7.5, dpi = 300,          
           units = 'in')
    
    sev_cohort <- make_co_plot(var = 'severe_averted',
                               strat = strat) + 
      labs(y = 'Cumulative severe cases averted',
           x = 'Age (years)',
           fill = 'Vaccination strategy',
           color = 'Vaccination strategy')
    
    ggsave(paste0('plots/cohorts_',strat, '_SA_overall_', seas, '.pdf'), height = 4.3, width = 7.5, dpi = 300,          
           units = 'in')#, compression = 'lzw')
    ggsave(paste0('plots/cohorts_',strat, '_SA_overall_', seas, '.svg'), height = 4.3, width = 7.5, dpi = 300,          
           units = 'in')
    
    casesandsevcohort <- cowplot::plot_grid(cases_cohort + theme(legend.position="none"), 
                                            sev_cohort + theme(legend.position="none"), 
                                            ncol = 1, labels = 'AUTO')
    
    # extract the legend from one of the plots
    legend <- cowplot::get_plot_component(
      # create some space to the left of the legend
      cases_cohort + theme(legend.box.margin = margin(0, 0, 0, 8)),
      'guide-box-right', return_all = TRUE
    )
    
    # add the legend to the row we made earlier. Give it one-third of 
    # the width of one plot (via rel_widths).
    casesandsevcohort <- plot_grid(casesandsevcohort, legend, 
                                   ncol = 2, rel_widths = c(3.2, 1.1))
    
    ggsave(paste0('plots/cohorts_',strat, '_CAandSA_overall_', seas, '.pdf'), 
           casesandsevcohort, width = 10, height = 8, dpi = 300)
    ggsave(paste0('plots/cohorts_',strat, '_CAandSA_overall_', seas, '.svg'), 
           casesandsevcohort, width = 10, height = 8, dpi = 300)
    
    
  }
  
  makeplots(strat = 'AB')
  makeplots(strat = 'catch-up')
  
}
