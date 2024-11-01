# Function to plot the age distributions
plot_age_dist <- function(df, 
                          seas_type = 'seasonal'){
  
  dfpl <- cohorts_byage %>%
    filter(pfpr %in% c(0.05, 0.25, 0.45)) %>%
    filter(EPIextra=='-' & (PEVage == '6m-14y' | PEVage == '6m-2y' | PEVstrategy == 'AB'  ) |
             PEVstrategy == 'none') %>%
    filter(seasonality == seas_type) %>%
    mutate(age_lower = as.numeric(age_lower)) %>%
    filter(age_upper < 25 & !(age_grp %in% c('0-5','5-10','10-15'))) #%>%
    # mutate(age_lower_floor = floor(age_lower)) 
  
  pfpr.labs <- c("5%", "25%", '45%')
  names(pfpr.labs) <- c("0.05","0.25", "0.45")
  
  # Plot function
  plot_ages <- function(var){
    plt <- ggplot(dfpl) +
      geom_ribbon(aes(x = age_lower, 
                      ymin = .data[[paste0(var,'_lower')]], 
                      ymax = .data[[paste0(var,'_upper')]], 
                      group = labels,
                      fill = labels), 
                  alpha = 0.1) +
      geom_line(aes(x = age_lower, 
                    y = .data[[var]], 
                    group = labels, 
                    color = labels, 
                    linetype = labels), 
                linewidth = 1.5, alpha = 0.8) + #, color = '#17556d'
      facet_wrap(~ pfpr, scales = 'free',
                 labeller = labeller(pfpr = pfpr.labs)) +
      theme_bw() +
      scale_color_manual(values = CUcols) +
      scale_fill_manual(values = CUcols) +
      scale_linetype_manual(values = c(6, 6, 1, 1)) +
      scale_x_continuous(breaks = seq(0,25, by = 2)) +
      theme(axis.title = element_text(size = 12),
            plot.title = element_text(size = 18),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 12),
            legend.position = 'bottom')
    
    return(plt)
  }

  cases_plt <- plot_ages(var = "cases_per1000pop") +
    labs(y = 'Uncomplicated cases per 1000 population',
         x = 'Age group (years)',
         color = 'Vaccination strategy',
         fill = 'Vaccination strategy',
         linetype = 'Vaccination strategy') 
  
  ggsave(paste0('plots/Age_dist_casesperperson_CU_', seas_type,'.pdf'), width = 9, height = 4, units = 'in')
  
  severe_plt <- plot_ages(var = "sevcases_per1000pop") +
    labs(y = 'Severe cases per 1000 population',
         x = 'Age group (years)',
         color = 'Vaccination strategy',
         fill = 'Vaccination strategy',
         linetype = 'Vaccination strategy') 
  ggsave(paste0('plots/Age_dist_sevcasesperperson_CU_', seas_type,'.pdf'), width = 9, height = 4, units = 'in')
  
  agedist <- cowplot::plot_grid(cases_plt + theme(legend.position="none"), 
                                severe_plt + theme(legend.position="none"), 
                                ncol = 1, labels = 'AUTO')
  
  # extract the legend from one of the plots
  legend <- get_legend(
    # create some space to the left of the legend
    cases_plt + theme(legend.box.margin = margin(0, 0, 0, 12))
  )
  
  # add the legend to the row we made earlier. 
  agedist <- plot_grid(agedist, legend, ncol = 1,
                   rel_heights = c(9, 1))
  
  ggsave(paste0('plots/Age_dist_casesandsev_perperson_CU_', seas_type,'.pdf'),  
         width = 9, height = 7, units = 'in')
}