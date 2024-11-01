# Function to plot age-based cohorts over age (time from vaccination)
#' @param df will be cohorts_byage
plot_cohort <- function(df,
                        seas = 'seasonal'){
  
  # Make labels for facets
  pfpr.labs <- c("5%", "25%", '45%', '65%')
  names(pfpr.labs) <- c("0.05","0.25", "0.45", "0.65")
  cohortcols <- c('#B03A2E','#6C3483','#1F618D','#00796b','#fbc02d','#CA6F1E','#689f38', '#3498db')
  make_co_plot <- function(var,
                           strat){
    
    # Get medians of each of the scenarios (will have multiple values for each age group as there are multiple cohorts)
    cohorts_plot <- cohorts_byage %>%
      mutate(age_lower = as.numeric(age_lower)) %>%
      filter(PEVstrategy == strat) |>
      filter(seasonality == seas & PEVstrategy != 'none' & pfpr != 0.01 & pfpr != 0.03) #%>%
    
    # Take median of the 30 age-based cohorts 
    # if(strat == 'AB'){
    #   cohorts_plot <- cohorts_plot %>%
    #     group_by(pfpr, labels, age_lower) %>%
    #     summarize(cases_per1000pop = median(cases_per1000pop),
    #               cases_averted_perpop  = median(cases_averted_perpop),
    #               severe_averted_perpop = median(severe_averted_perpop),
    #               cases_averted = median(cases_averted),
    #               severe_averted = median(severe_averted),
    #               # cases_averted_perdose = median(cases_averted_perdose),
    #               # severe_averted_perdose = median(severe_averted_perdose),
    #               
    #               cases_per1000pop_lower = median(cases_per1000pop_lower),
    #               cases_averted_perpop_lower  = median(cases_averted_perpop_lower),
    #               severe_averted_perpop_lower = median(severe_averted_perpop_lower),
    #               cases_averted_lower = median(cases_averted_lower),
    #               severe_averted_lower = median(severe_averted_lower),
    #               # cases_averted_perdose_lower = median(cases_averted_perdose_lower),
    #               # severe_averted_perdose_lower = median(severe_averted_perdose_lower),
    #               
    #               cases_per1000pop_upper = median(cases_per1000pop_upper),
    #               cases_averted_perpop_upper  = median(cases_averted_perpop_upper),
    #               severe_averted_perpop_upper = median(severe_averted_perpop_upper),
    #               cases_averted_upper = median(cases_averted_upper),
    #               severe_averted_upper = median(severe_averted_upper))#,
    #               # cases_averted_perdose_upper = median(cases_averted_perdose_upper),
    #               # severe_averted_perdose_upper = median(severe_averted_perdose_upper))
    # }
    # group_by(floor(age_lower), age_grp, int_ID, labels, label_int, PEV, PEVcov, PEVstrategy, PEVage, 
    #          EPIextra, pfpr, seasonality) %>%
    # summarize(cases = sum(cases),
    #           n = sum(n),
    #           cases_averted = sum(cases_averted),
    #           cases_averted_perpop  = cases_averted / n * 1000)#%>%
    # group_by(age_lower, int_ID, labels, label_int, PEV, PEVcov, PEVstrategy, PEVage, PEVrounds,
    #          EPIbooster, EPIextra, massbooster_rep, MDA, pfpr, seasonality) %>%
    # summarize(cases_averted_perpop_med = median(cases_averted_perpop),
    #           cases_averted_med = median(cases_averted),
    #           cases_averted_perdose_med = median(cases_averted_perdose),
    # 
    #           severe_averted_perpop_med = median(severe_averted_perpop),
    #           severe_averted_med = median(severe_averted),
    #           severe_averted_perdose_med = median(severe_averted_perdose))
    
    # cohorts_plot <- cohorts_ageatvaxandage %>%
    #   filter(ageatvax == '0.5-1' | ageatvax == '1-1.5') %>%
    #   filter(PEVstrategy == strategy | PEVstrategy == 'none') |>
    #   filter(seasonality == seas & PEVstrategy != 'none' & pfpr != 0.01 & pfpr != 0.03) 
    
    ggplot(data = cohorts_plot) +
      geom_ribbon(aes(x = age_lower, 
                      ymin = .data[[paste0(var,'_lower')]], 
                      ymax = .data[[paste0(var,'_upper')]], 
                      fill = labels),
                  alpha = 0.15) +
      geom_line(aes(x = age_lower, 
                    y = .data[[var]], 
                    color = labels), 
                linewidth = 1.2) +
       scale_color_manual(values = if(strat == 'AB') {cohortcols} else if(strat == 'catch-up'){colsCU}) +
      scale_fill_manual(values = if(strat == 'AB') {cohortcols} else if(strat == 'catch-up'){colsCU}) +
      scale_x_continuous(breaks = seq(0,max(age_lower),1)) +
      geom_hline(yintercept = 0, 
                 linetype = 2) + 
      facet_wrap(~ pfpr , scales = 'free',
                 labeller = labeller(pfpr = pfpr.labs)) + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, size = 12),
            axis.title = element_text(size = 20),
            plot.title = element_text(size = 22),
            legend.text = element_text(size = 15),
            strip.text.x = element_text(size = 12),
            legend.title = element_text(size = 18),
            plot.caption = element_text(size = 12),
            legend.key.size = unit(0.8, 'cm'),
            axis.text.y = element_text(size = 12))
  }
  
  makeplots <- function(strat){# PER POP
    cases_cohort <- make_co_plot(var = 'cases_averted_perpop',
                                 strat = strat) + 
      labs(y = 'Uncomplicated cases averted per 1000 people',
           x = 'Age (years)',
           fill = 'Vaccination strategy',
           color = 'Vaccination strategy')
    
    ggsave(paste0('plots/cohorts_', strat, '_CA_perpop_', seas, '.pdf'), height = 8, width = 14)
    ggsave(paste0('plots/cohorts_', strat, '_CA_perpop_', seas, '.png'), height = 8, width = 14, dpi = 1000)
    
    sev_cohort <- make_co_plot(var = 'severe_averted_perpop',
                               strat = strat) + 
      labs(y = 'Severe cases averted per 1000 people',
           x = 'Age (years)',
           fill = 'Vaccination strategy',
           color = 'Vaccination strategy')
    
    ggsave(paste0('plots/cohorts_', strat,'_SA_perpop_', seas, '.pdf'), height = 8, width = 14)
    ggsave(paste0('plots/cohorts_', strat,'_SA_perpop_', seas, '.png'), height = 8, width = 14, dpi = 1000)
    
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
    
    ggsave(paste0('plots/cohorts_',strat,'_CAandSA_perpop_', seas, '.pdf'), casesandsevcohort, width = 10, height = 8)
    
    # OVERALL COUNTS 
    cases_cohort <- make_co_plot(var = 'cases_averted',
                                 strat = strat) + 
      labs(y = 'Cumulative uncomplicated cases averted',
           x = 'Age (years)',
           fill = 'Vaccination strategy',
           color = 'Vaccination strategy')
    
    ggsave(paste0('plots/cohorts_',strat, '_CA_overall_', seas, '.pdf'), height = 8, width = 14)
    
    sev_cohort <- make_co_plot(var = 'severe_averted',
                               strat = strat) + 
      labs(y = 'Cumulative severe cases averted',
           x = 'Age (years)',
           fill = 'Vaccination strategy',
           color = 'Vaccination strategy')
    
    ggsave(paste0('plots/cohorts_',strat, '_SA_overall_', seas, '.pdf'), height = 8, width = 14)
    
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
    
    ggsave(paste0('plots/cohorts_',strat, '_CAandSA_overall_', seas, '.pdf'), casesandsevcohort, width = 10, height = 8)
    
    # PER DOSE
    # cases_cohort <- make_co_plot(var = 'cases_averted_perdose',
    #                              strat = strat) + 
    #   labs(y = 'Uncomplicated cases averted per 1000 doses',
    #        x = 'Age (years)',
    #        fill = 'Vaccination strategy',
    #        color = 'Vaccination strategy')
    # 
    # ggsave(paste0('plots/cohorts_', strat, '_CA_overall_perdose_', seas, '.pdf'), height = 5, width = 10)
    # 
    # sev_cohort <- make_co_plot(var = 'severe_averted_perdose',
    #                            strat = strat) + 
    #   labs(y = 'Severe cases averted per 1000 doses',
    #        x = 'Age (years)',
    #        fill = 'Vaccination strategy',
    #        color = 'Vaccination strategy')
    # 
    # ggsave(paste0('plots/cohorts_', strat, '_SA_overall_perdose_', seas, '.pdf'), height = 5, width = 10)
    # 
    # casesandsevcohort <- cowplot::plot_grid(cases_cohort + theme(legend.position="none"), 
    #                                         sev_cohort + theme(legend.position="none"), 
    #                                         ncol = 1, labels = 'AUTO')
    # 
    # # extract the legend from one of the plots
    # legend <- cowplot::get_plot_component(
    #   # create some space to the left of the legend
    #   cases_cohort + theme(legend.box.margin = margin(0, 0, 0, 8)),
    #   'guide-box-right', return_all = TRUE
    # )
    
    # add the legend to the row we made earlier. Give it one-third of 
    # the width of one plot (via rel_widths).
    # casesandsevcohort <- plot_grid(casesandsevcohort, legend, 
    #                                ncol = 2, rel_widths = c(3.2, 1.1))
    # 
    # ggsave(paste0('plots/cohorts_', strat, '_CAandSA_overall_perdose_', seas, '.pdf'), casesandsevcohort, width = 10, height = 8)
  }
  
  makeplots(strat = 'AB')
  makeplots(strat = 'catch-up')
  
}
