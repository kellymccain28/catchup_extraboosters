
# Function to loop over all options to make this plot 

plot_cumul_CA <- function(df_summ, cohorts){#df_last15
  
  plottheme <- theme(axis.title = element_text(size = 8),
                     # plot.title = element_text(size = 22),
                     legend.text = element_text(size = 7),
                     strip.text.x = element_text(size = 8),
                     legend.title = element_text(size = 8),
                     plot.caption = element_text(size = 10),
                     legend.key.size = unit(0.3, 'cm'),
                     axis.text.x = element_text(size = 8),
                     axis.text.y = element_text(size = 8),
                     plot.margin = margin(t = 2,  # Top margin
                                          r = 2,  # Right margin
                                          b = 2,  # Bottom margin
                                          l = 2)
  )
  
  # variable can be either 'cases_averted' or 'severe_averted'
  # Function to make a plot of cumulative cases and severe cases averted
  
  #' @param strategy 'catch-up no booster', 'age-based', 'seasonal routine', or 'catch-up all'
  #'                  with age-based and seasonal routine, need to use only last 15 years of simulation
  #' @param df should be df_summ (summarized over whole simulation)
  #' @param compare is either '' or AB
  #' @param variable inherited from larger function - either 'cases_averted' or 'severe_averted'
  #' @param seas seasonality - seasonal or perennial
  plot_cumul_CA2 <- function(df,
                             strategy,
                             compare,
                             variable,
                             seas) {
    # CUcols1 <- c('#B03A2E','#6C3483','#1F618D','#00796b','#fbc02d','#CA6F1E','#689f38', '#3498db','tan','#283747','#85929E')
    # CUcols_ <- c('black','#6C3483','#1F618D','#00796b','#fbc02d','#CA6F1E','#689f38', '#3498db','tan','#283747','#85929E')
    cohortspl <- cohorts %>%
      filter(pfpr != 0.03) %>%
      filter(seasonality == seas) %>%
      mutate(pfpr = factor(paste0(pfpr * 100, '%'), levels = c('1%','3%','5%','25%','45%','65%')))
       
    
    dfpl <- df %>%
      filter(age_grp == '0-100') %>%
      filter(pfpr != 0.03) %>%
      filter(seasonality == seas) %>%
      dplyr::select(c(pfpr, labels, PEVstrategy, EPIextra, 
                      contains('relevant'), contains('perpop'), contains('perdose'), 
                      -starts_with('AB'), contains('peradddose'))) %>%
      mutate(pfpr = factor(paste0(pfpr * 100, '%'), levels = c('1%','3%','5%','25%','45%','65%')))
    
    # Filter datasets to specific strategies based on plot to make
    if(strategy == 'catch-up no booster'){
      dfpl <- dfpl %>%
        filter((EPIextra == '-'  & PEVstrategy == 'catch-up') | (PEVstrategy == 'AB' & EPIextra == '-'))
      copl <- cohortspl %>%
        filter((EPIextra == '-'  & PEVstrategy == 'catch-up') | (PEVstrategy == 'AB' & EPIextra == '-'))
    } else if(strategy == 'age-based'){
      dfpl <- dfpl %>%
        filter(PEVstrategy == 'AB')  
      copl <- cohortspl %>%
        filter(PEVstrategy == 'AB')  
    } else if(strategy == 'catch-up all'){ 
      dfpl <- dfpl %>%
        filter(PEVstrategy == 'catch-up' | (PEVstrategy == 'AB' & EPIextra == '-')) 
      copl <- cohortspl %>%
        filter(PEVstrategy == 'catch-up' | (PEVstrategy == 'AB' & EPIextra == '-')) 
    } else if(strategy == 'seasonal routine'){
      dfpl <- dfpl %>%
        filter((PEVstrategy == 'SV' | PEVstrategy == 'hybrid' | PEVstrategy == 'AB') & EPIextra=='-') 
      copl <- cohortspl %>%
        filter((PEVstrategy == 'SV' | PEVstrategy == 'hybrid' | PEVstrategy == 'AB') & EPIextra=='-') 
    } else if(strategy == 'combination'){
      dfpl <- dfpl %>%
        filter((PEVstrategy == 'catch-up' & EPIextra != '-') | (PEVstrategy == 'AB' & EPIextra == '-')) 
      copl <- cohortspl %>%
        filter((PEVstrategy == 'catch-up' & EPIextra != '-') | (PEVstrategy == 'AB' & EPIextra == '-')) 
      
      CUcols <- colscombo
      CUcols_ <- colscombo
      CUcols_[1] <- 'black'
      
    }
    
    if (variable %in% c('cases_averted_routine', 'severe_averted_routine')){
      varnew <- str_replace(variable,'_routine','')
    } else varnew <- variable
    
    A <- ggplot(dfpl) +
      geom_col(aes(x = as.factor(pfpr), 
                   y = .data[[paste0(compare, varnew, '_perpop')]], 
                   fill = labels, color = labels), 
               position ='dodge', 
               alpha = 0.7) + 
      geom_errorbar(aes(x = as.factor(pfpr), 
                        ymin = .data[[paste0(compare, varnew, "_perpop_lower")]],
                        ymax = .data[[paste0(compare, varnew, "_perpop_upper")]], 
                        color = labels),
                    position = position_dodge(width = 0.9), 
                    width = 0.4, 
                    linewidth = 0.38) +
      theme_bw() +
      scale_fill_manual(values = CUcols1) +
      scale_color_manual(values = CUcols_) +
      labs(y = if(variable == 'cases_averted'){'Cumulative clinical cases averted\nper 1000 population'} 
           else {'Cumulative severe cases averted\nper 1000 population'},
           x = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
           fill = 'Vaccination strategy'
      ) +
      guides(color = 'none') +
      plottheme
    
    B <- ggplot(if(strategy == 'catch-up no booster' | strategy == 'catch-up all'){copl} else dfpl) + # Catch-up plots are made with cohorts and age-based booster plots with whole simulation 
      geom_col(aes(x = as.factor(pfpr), 
                   y = .data[[paste0(varnew, "_perdose")]], 
                   fill = labels, color = labels), 
               position ='dodge', 
               alpha = 0.7) + 
      geom_errorbar(aes(x = as.factor(pfpr), 
                        ymin = .data[[paste0(varnew, "_perdose_lower")]], 
                        ymax = .data[[paste0(varnew, "_perdose_upper")]], 
                        color = labels),
                    position = position_dodge(width = 0.9), 
                    width = 0.4, 
                    linewidth = 0.38) +
      theme_bw() +
      scale_fill_manual(values = CUcols1) +
      scale_color_manual(values = CUcols_) +
      labs(y = if(variable == 'cases_averted'){'Cumulative clinical cases averted\nper 1000 doses'} 
           else {'Cumulative severe cases averted\nper 1000 doses'},
           x = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
           fill = 'Vaccination strategy'
           ) +
      guides(color = 'none') +
      plottheme
    
    C <- ggplot(dfpl %>% 
                  mutate(cases_averted_routine_peradddose = ifelse(labels == 'Routine age-based',
                                                                   cases_averted_perdose, cases_averted_routine_peradddose),
                         severe_averted_routine_peradddose = ifelse(labels == 'Routine age-based',
                                                                    severe_averted_perdose, severe_averted_routine_peradddose),
                         cases_averted_routine_peradddose_lower = ifelse(labels == 'Routine age-based',
                                                                   cases_averted_perdose_lower, cases_averted_routine_peradddose_lower),
                         severe_averted_routine_peradddose_lower = ifelse(labels == 'Routine age-based',
                                                                    severe_averted_perdose_lower, severe_averted_routine_peradddose_lower),
                         cases_averted_routine_peradddose_upper = ifelse(labels == 'Routine age-based',
                                                                   cases_averted_perdose_upper, cases_averted_routine_peradddose_upper),
                         severe_averted_routine_peradddose_upper = ifelse(labels == 'Routine age-based',
                                                                    severe_averted_perdose_upper, severe_averted_routine_peradddose_upper))) +
                         # filter(labels !='Routine age-based')) + # Catch-up plots are made with cohorts and age-based booster plots with whole simulation 
      geom_col(aes(x = as.factor(pfpr), 
                   y = .data[[paste0(variable, "_routine_peradddose")]], 
                   fill = labels, color = labels), 
               position ='dodge', 
               alpha = 0.7) + 
      geom_errorbar(aes(x = as.factor(pfpr), 
                        ymin = .data[[paste0(variable, "_routine_peradddose_lower")]], 
                        ymax = .data[[paste0(variable, "_routine_peradddose_upper")]], 
                        color = labels),
                    position = position_dodge(width = 0.9), 
                    width = 0.4, 
                    linewidth = 0.38) +
      theme_bw() +
      scale_fill_manual(values = CUcols1) +
      scale_color_manual(values = CUcols1) +
      labs(y = if(variable == 'cases_averted'){'Cumulative clinical cases averted\nper 1000 additional doses\n(relative to routine age-based)'} 
           else {'Cumulative severe cases averted\nper 1000 additional doses\n(relative to routine age-based)'},
           x = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
           fill = 'Vaccination strategy'
      ) +
      guides(color = 'none') +
      plottheme
    
    
    # Maek combination plot with per pop and per dose 
    p <- cowplot::plot_grid(A + theme(#axis.title = element_text(size = 14), 
                                      legend.position="none"), 
                            B + theme(#axis.title = element_text(size = 14),
                                      legend.position="none"), labels = 'AUTO')
    
    # extract the legend from one of the plots
    legend <- cowplot::get_plot_component(
      # create some space to the left of the legend
      A + theme(legend.box.margin = margin(0, 12, 0, 20)),
      'guide-box-right', return_all = TRUE)
    
    # add the legend to the row we made earlier.
    plot <- plot_grid(p, legend, rel_widths = c(3, 0.63))
    
    
    # Maek combination plot with per pop and per ADDITIONAL dose 
    p2 <- cowplot::plot_grid(A + theme(#axis.title = element_text(size = 14), 
      legend.position="none"), 
      C + theme(#axis.title = element_text(size = 14),
        legend.position="none"), labels = 'AUTO')
    
    # extract the legend from one of the plots
    legend <- cowplot::get_plot_component(
      # create some space to the left of the legend
      A + theme(legend.box.margin = margin(0, 12, 0, 20)),
      'guide-box-right', return_all = TRUE)
    
    # add the legend to the row we made earlier.
    plot2 <- plot_grid(p2, legend, rel_widths = c(3, 0.63))
    
    # Save files to refer to in text 
    if((strategy == 'catch-up no booster' | strategy == 'age-based') & variable == 'cases_averted') {
      write.csv(copl %>% select(pfpr, seasonality, labels, PEVage, PEVstrategy, 
                                contains('perdose')), paste0('plots/cohorts_CAperdose_', strategy, seas,".csv"))
    }
    return(list(plot, A, B, C, plot2))
  }
  
  # Make and save all versions of plot
  seasonalities <- c('perennial', 'seasonal')
  # clinical cases
  for(s in seq_along(seasonalities)){
    CAcatchup_noboost_none <- plot_cumul_CA2(df_summ, 
                                             strategy = 'catch-up no booster',
                                             compare = '',
                                             variable = 'cases_averted',
                                             seas = seasonalities[s]) 
    ggsave(paste0("plots/plot_cumulCA_catchupnobooster_none_", seasonalities[s], ".tiff"), 
           plot = CAcatchup_noboost_none[[1]], width = 7.5, height = 2.83, dpi = 500,
           units = 'in')
    ggsave(paste0("plots/plot_cumulCA_catchupnobooster_routine_", seasonalities[s], ".tiff"), 
           plot = CAcatchup_noboost_none[[5]], width = 7.5, height = 2.83, dpi = 500,
           units = 'in')
    ggsave(paste0("plots/plot_cumulCA_catchupnobooster_none_", seasonalities[s], ".pdf"), 
           plot = CAcatchup_noboost_none[[1]], width = 7.5, height = 2.83, dpi = 500,
           units = 'in')
    ggsave(paste0("plots/plot_cumulCA_catchupnobooster_routine_", seasonalities[s], ".pdf"), 
           plot = CAcatchup_noboost_none[[5]], width = 7.5, height = 2.83, dpi = 500,
           units = 'in')
    
    CAAB_none <- plot_cumul_CA2(df_summ, 
                                strategy = 'age-based',
                                compare = '',
                                variable = 'cases_averted',
                                seas = seasonalities[s])
    ggsave(paste0("plots/plot_cumulCA_AB_none_", seasonalities[s], ".tiff"), 
           plot = CAAB_none[[1]], width = 7.5, height = 2.83, dpi = 500, units = 'in' ) 
    ggsave(paste0("plots/plot_cumulCA_AB_routine_", seasonalities[s], ".tiff"), 
           plot = CAAB_none[[5]], width = 7.5, height = 2.83, dpi = 500, units = 'in' ) 
    ggsave(paste0("plots/plot_cumulCA_AB_none_", seasonalities[s], ".pdf"), 
           plot = CAAB_none[[1]], width = 7.5, height = 2.83, dpi = 500, units = 'in' ) 
    ggsave(paste0("plots/plot_cumulCA_AB_routine_", seasonalities[s], ".pdf"), 
           plot = CAAB_none[[5]], width = 7.5, height = 2.83, dpi = 500, units = 'in' ) 
    

    CAseasonal_routine <- plot_cumul_CA2(df_summ,
                                         strategy = 'seasonal routine',
                                         compare = '',
                                         variable = 'cases_averted',
                                         seas = seasonalities[s])
    ggsave(paste0("plots/plot_cumulCA_SVhybrid_none_", seasonalities[s], ".tiff"), 
           plot = CAseasonal_routine[[1]], width = 7.6, height = 2.83, dpi = 500,            
           units = 'in')
    ggsave(paste0("plots/plot_cumulCA_SVhybrid_none_", seasonalities[s], ".pdf"), 
           plot = CAseasonal_routine[[1]], width = 7.6, height = 2.83, dpi = 500,            
           units = 'in' )
    
    # CAcombination <- plot_cumul_CA2(df_summ,
    #                                      strategy = 'combination',
    #                                      compare = '',
    #                                      variable = 'cases_averted',
    #                                      seas = seasonalities[s])
    # ggsave(paste0("plots/plot_cumulCA_combination_none_", seasonalities[s], ".tiff"), 
    #        plot = CAcombination[[1]], width = 7.5, height = 2.83, dpi = 500,            units = 'in' )
    
    # severe cases
    SAcatchup_noboost_none <- plot_cumul_CA2(df_summ, 
                                               strategy = 'catch-up no booster',
                                             compare = '',
                                             variable = 'severe_averted',
                                             seas = seasonalities[s]) 
    ggsave(paste0("plots/plot_cumulSA_catchupnobooster_none_", seasonalities[s], ".tiff"), 
           plot = SAcatchup_noboost_none[[1]], width = 7.5, height = 2.83, dpi = 500,            units = 'in' )
    ggsave(paste0("plots/plot_cumulSA_catchupnobooster_routine_", seasonalities[s], ".tiff"), 
           plot = SAcatchup_noboost_none[[5]], width = 7.5, height = 2.83, dpi = 500,            units = 'in' )
    ggsave(paste0("plots/plot_cumulSA_catchupnobooster_none_", seasonalities[s], ".pdf"), 
           plot = SAcatchup_noboost_none[[1]], width = 7.5, height = 2.83, dpi = 500,            
           units = 'in')
    ggsave(paste0("plots/plot_cumulSA_catchupnobooster_routine_", seasonalities[s], ".pdf"), 
           plot = SAcatchup_noboost_none[[5]], width = 7.5, height = 2.83, dpi = 500,            
           units = 'in')
    
    SAAB_none <- plot_cumul_CA2(df_summ, 
                                strategy = 'age-based',
                                compare = '',
                                variable = 'severe_averted',
                                seas = seasonalities[s]) 
    ggsave(paste0("plots/plot_cumulSA_AB_none_", seasonalities[s], ".tiff"), 
           plot = SAAB_none[[1]], width = 7.5, height = 2.83, dpi = 500,            units = 'in' )
    ggsave(paste0("plots/plot_cumulSA_AB_routine_", seasonalities[s], ".tiff"), 
           plot = SAAB_none[[5]], width = 7.5, height = 2.83, dpi = 500,            units = 'in' )
    ggsave(paste0("plots/plot_cumulSA_AB_none_", seasonalities[s], ".pdf"), 
           plot = SAAB_none[[1]], width = 7.5, height = 2.83, dpi = 500,            units = 'in')
    ggsave(paste0("plots/plot_cumulSA_AB_routine_", seasonalities[s], ".pdf"), 
           plot = SAAB_none[[5]], width = 7.5, height = 2.83, dpi = 500,            units = 'in')
    
    SAseasonal_routine <- plot_cumul_CA2(df_summ,
                                         strategy = 'seasonal routine',
                                         compare = '',
                                         variable = 'severe_averted',
                                         seas = seasonalities[s])
    ggsave(paste0("plots/plot_cumulSA_SVhybrid_none_", seasonalities[s], ".tiff"), 
           plot = SAseasonal_routine[[1]], width = 7.5, height = 2.83, dpi = 500,            units = 'in' )
    ggsave(paste0("plots/plot_cumulSA_SVhybrid_none_", seasonalities[s], ".pdf"), 
           plot = SAseasonal_routine[[1]], width = 7.5, height = 2.83, dpi = 500,            units = 'in' )
    
    
    ####### Make plot with clinical and severe just for the per DOSE for CATCH-UP
    CASACU <- cowplot::plot_grid(CAcatchup_noboost_none[[3]] + theme(legend.position="none"), 
                                 SAcatchup_noboost_none[[3]] + theme(legend.position="none"), labels = 'AUTO')
    
    # extract the legend from one of the plots
    legend <- cowplot::get_plot_component(
      # create some space to the left of the legend
      CAcatchup_noboost_none[[2]] + theme(legend.box.margin = margin(0, 20, 0, 25)),
      'guide-box-right')
    
    # add the legend to the row we made earlier.
    plotCASA_CU <- plot_grid(CASACU, legend, rel_widths = c(3, 0.63))
    ggsave(paste0("plots/plot_cumulCASA_CU_", seasonalities[s], ".tiff"),
           plot = plotCASA_CU, width = 7.5, height = 2.83, dpi = 500,            
           units = 'in' )
    
    
    ######### Make plot with clinical and severe just for the per dose for AGE-BASED
    CASAAB <- cowplot::plot_grid(CAAB_none[[3]] + theme(legend.position="none"), 
                                 SAAB_none[[3]] + theme(legend.position="none"), labels = 'AUTO')
    
    # extract the legend from one of the plots
    legend <- cowplot::get_plot_component(
      # create some space to the left of the legend
      CAAB_none[[2]] + theme(legend.box.margin = margin(0, 20, 0, 25)),
      'guide-box-right')
    
    # add the legend to the row we made earlier.
    plotCASA_AB <- plot_grid(CASAAB, legend, rel_widths = c(3, 0.63))
    ggsave(paste0("plots/plot_cumulCASA_AB_", seasonalities[s], ".tiff"), 
           plot = plotCASA_AB, width = 7.5, height = 2.83, dpi = 500,            units = 'in' )
    ggsave(paste0("plots/plot_cumulCASA_AB_", seasonalities[s], ".pdf"), 
           plot = plotCASA_AB, width = 7.5, height = 2.83, dpi = 500,            units = 'in' )
    
    
    
    ####### Make plot with clinical and severe just for the per POP for CATCH-UP
    CASACU <- cowplot::plot_grid(CAcatchup_noboost_none[[2]] + theme(legend.position="none"), 
                                 SAcatchup_noboost_none[[2]] + theme(legend.position="none"), labels = 'AUTO')
    
    # extract the legend from one of the plots
    legend <- cowplot::get_plot_component(
      # create some space to the left of the legend
      CAcatchup_noboost_none[[2]] + theme(legend.box.margin = margin(0, 20, 0, 25)),
      'guide-box-right')
    
    # add the legend to the row we made earlier.
    plotCASA_CU <- plot_grid(CASACU, legend, rel_widths = c(3, 0.63))
    ggsave(paste0("plots/plot_cumulCASAperpop_CU_", seasonalities[s], ".tiff"), 
           plot = plotCASA_CU, width = 7.5, height = 2.83, dpi = 500,            units = 'in' )
    ggsave(paste0("plots/plot_cumulCASAperpop_CU_", seasonalities[s], ".pdf"), 
           plot = plotCASA_CU, width = 7.5, height = 2.83, dpi = 500,            units = 'in')
    
    ######### Make plot with clinical and severe just for the per pop for AGE-BASED
    CASAAB <- cowplot::plot_grid(CAAB_none[[2]] + theme(legend.position="none"), 
                                 SAAB_none[[2]] + theme(legend.position="none"), labels = 'AUTO')
    
    # extract the legend from one of the plots
    legend <- cowplot::get_plot_component(
      # create some space to the left of the legend
      CAAB_none[[2]] + theme(legend.box.margin = margin(0, 20, 0, 25)),
      'guide-box-right')
    
    # add the legend to the row we made earlier.
    plotCASA_AB <- plot_grid(CASAAB, legend, rel_widths = c(3, 0.63))
    ggsave(paste0("plots/plot_cumulCASAperpop_AB_", seasonalities[s], ".tiff"), 
           plot = plotCASA_AB, width = 7.5, height = 2.83, dpi = 500,            units = 'in' )
    ggsave(paste0("plots/plot_cumulCASAperpop_AB_", seasonalities[s], ".pdf"), 
           plot = plotCASA_AB, width = 7.5, height = 2.83, dpi = 500,            units = 'in' )
    
    
    
    ############## Make plot with clinical and severe just for the per ADDITIONAL DOSE for CATCH-UP
    CASACU <- cowplot::plot_grid(CAcatchup_noboost_none[[4]] + theme(legend.position="none"), 
                                 SAcatchup_noboost_none[[4]] + theme(legend.position="none"), labels = 'AUTO')
    
    # extract the legend from one of the plots
    legend <- cowplot::get_plot_component(
      # create some space to the left of the legend
      CAcatchup_noboost_none[[4]] + theme(legend.box.margin = margin(0, 20, 0, 25)),
      'guide-box-right')
    
    # add the legend to the row we made earlier.
    plotCASA_CU <- plot_grid(CASACU, legend, rel_widths = c(3, 0.63))
    ggsave(paste0("plots/plot_cumulCASA_CU_ADDDOSE", seasonalities[s], ".tiff"),
           plot = plotCASA_CU, width = 7.5, height = 2.83, dpi = 500,            
           units = 'in' )
    ggsave(paste0("plots/plot_cumulCASA_CU_ADDDOSE", seasonalities[s], ".pdf"),
           plot = plotCASA_CU, width = 7.5, height = 2.83, dpi = 500,            
           units = 'in')
    
    
    ######### Make plot with clinical and severe just for the per ADDITIONAL DOSE for AGE-BASED
    CASAAB <- cowplot::plot_grid(CAAB_none[[4]] + theme(legend.position="none"), 
                                 SAAB_none[[4]] + theme(legend.position="none"), labels = 'AUTO')
    
    # extract the legend from one of the plots
    legend <- cowplot::get_plot_component(
      # create some space to the left of the legend
      CAAB_none[[4]] + theme(legend.box.margin = margin(0, 20, 0, 25)),
      'guide-box-right')
    
    # add the legend to the row we made earlier.
    plotCASA_AB <- plot_grid(CASAAB, legend, rel_widths = c(3, 0.63))
    ggsave(paste0("plots/plot_cumulCASA_AB_ADDDOSE", seasonalities[s], ".tiff"), 
           plot = plotCASA_AB, width = 7.5, height = 2.83, dpi = 500,            units = 'in' )
    ggsave(paste0("plots/plot_cumulCASA_AB_ADDDOSE", seasonalities[s], ".pdf"), 
           plot = plotCASA_AB, width = 7.5, height = 2.83, dpi = 500,            units = 'in' )
    
    
    # Make separate plot like plot B for just the routine age-based outcomes 
    dfpl <- df_summ %>%
      filter(age_grp == '0-100') %>%
      filter(pfpr != 0.03) %>%
      filter(seasonality == 'perennial') %>%
      dplyr::select(c(pfpr, labels, PEVstrategy, EPIextra, 
                      contains('relevant'), contains('perpop'), contains('perdose'), 
                      -starts_with('AB'), contains('peradddose'))) %>%
      mutate(pfpr = factor(paste0(pfpr * 100, '%'), levels = c('1%','3%','5%','25%','45%','65%')))
    
    routine_clin <- ggplot(dfpl %>% filter(labels == 'Routine age-based')) + # Catch-up plots are made with cohorts and age-based booster plots with whole simulation 
      geom_col(aes(x = as.factor(pfpr), 
                   y = cases_averted_perdose, 
                   fill = labels, color = labels), 
               position ='dodge', 
               alpha = 0.7) + 
      geom_errorbar(aes(x = as.factor(pfpr), 
                        ymin = cases_averted_perdose_lower, 
                        ymax = cases_averted_perdose_upper, 
                        color = labels),
                    position = position_dodge(width = 0.9), 
                    width = 0.4, 
                    linewidth = 0.38) +
      theme_bw() +
      scale_fill_manual(values = CUcols1) +
      scale_color_manual(values = CUcols_) +
      labs(y = 'Cumulative clinical cases averted\nper 1000 doses',
           x = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
           fill = 'Vaccination strategy'
      ) +
      guides(color = 'none') +
      plottheme
    routine_sev <- ggplot(dfpl %>% filter(labels == 'Routine age-based')) + # Catch-up plots are made with cohorts and age-based booster plots with whole simulation 
      geom_col(aes(x = as.factor(pfpr), 
                   y = severe_averted_perdose, 
                   fill = labels, color = labels), 
               position ='dodge', 
               alpha = 0.7) + 
      geom_errorbar(aes(x = as.factor(pfpr), 
                        ymin = severe_averted_perdose_lower, 
                        ymax = severe_averted_perdose_upper, 
                        color = labels),
                    position = position_dodge(width = 0.9), 
                    width = 0.4, 
                    linewidth = 0.38) +
      theme_bw() +
      scale_fill_manual(values = CUcols1) +
      scale_color_manual(values = CUcols_) +
      labs(y = 'Cumulative severe cases averted\nper 1000 doses',
           x = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
           fill = 'Vaccination strategy'
      ) +
      guides(color = 'none') +
      plottheme
    routine_clinsev <- cowplot::plot_grid(routine_clin + theme(legend.position="none"), routine_sev + theme(legend.position="none"), labels = 'AUTO')
    
    # add the legend to the row we made earlier.
    ggsave(paste0("plots/plot_cumul_routine_clinsev_per.tiff"), 
           plot = routine_clinsev, width = 7.5, height = 2.83, dpi = 500, units = 'in' )
    ggsave(paste0("plots/plot_cumul_routine_clinsev_per.pdf"), 
           plot = routine_clinsev, width = 7.5, height = 2.83, dpi = 500, units = 'in' )
    
    # Make separate plot like plot B for just the routine age-based outcomes - seasonal
    dfpl <- df_summ %>%
      filter(age_grp == '0-100') %>%
      filter(pfpr != 0.03) %>%
      filter(seasonality == 'seasonal') %>%
      dplyr::select(c(pfpr, labels, PEVstrategy, EPIextra, 
                      contains('relevant'), contains('perpop'), contains('perdose'), 
                      -starts_with('AB'), contains('peradddose'))) %>%
      mutate(pfpr = factor(paste0(pfpr * 100, '%'), levels = c('1%','3%','5%','25%','45%','65%')))
    
    routine_clin <- ggplot(dfpl %>% filter(labels == 'Routine age-based')) + # Catch-up plots are made with cohorts and age-based booster plots with whole simulation 
      geom_col(aes(x = as.factor(pfpr), 
                   y = cases_averted_perdose, 
                   fill = labels, color = labels), 
               position ='dodge', 
               alpha = 0.7) + 
      geom_errorbar(aes(x = as.factor(pfpr), 
                        ymin = cases_averted_perdose_lower, 
                        ymax = cases_averted_perdose_upper, 
                        color = labels),
                    position = position_dodge(width = 0.9), 
                    width = 0.4, 
                    linewidth = 0.38) +
      theme_bw() +
      scale_fill_manual(values = CUcols1) +
      scale_color_manual(values = CUcols_) +
      labs(y = 'Cumulative clinical cases averted\nper 1000 doses',
           x = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
           fill = 'Vaccination strategy'
      ) +
      guides(color = 'none') +
      plottheme
    routine_sev <- ggplot(dfpl %>% filter(labels == 'Routine age-based')) + # Catch-up plots are made with cohorts and age-based booster plots with whole simulation 
      geom_col(aes(x = as.factor(pfpr), 
                   y = severe_averted_perdose, 
                   fill = labels, color = labels), 
               position ='dodge', 
               alpha = 0.7) + 
      geom_errorbar(aes(x = as.factor(pfpr), 
                        ymin = severe_averted_perdose_lower, 
                        ymax = severe_averted_perdose_upper, 
                        color = labels),
                    position = position_dodge(width = 0.9), 
                    width = 0.4, 
                    linewidth = 0.38) +
      theme_bw() +
      scale_fill_manual(values = CUcols1) +
      scale_color_manual(values = CUcols_) +
      labs(y = 'Cumulative severe cases averted\nper 1000 doses',
           x = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
           fill = 'Vaccination strategy'
      ) +
      guides(color = 'none') +
      plottheme
    routine_clinsev <- cowplot::plot_grid(routine_clin + theme(legend.position="none"), routine_sev + theme(legend.position="none"), labels = 'AUTO')
    
    # add the legend to the row we made earlier.
    ggsave(paste0("plots/plot_cumul_routine_clinsev_seas.tiff"), 
           plot = routine_clinsev, width = 7.5, height = 2.83, dpi = 500, units = 'in' )
    ggsave(paste0("plots/plot_cumul_routine_clinsev_seas.pdf"), 
           plot = routine_clinsev, width = 7.5, height = 2.83, dpi = 500, units = 'in' )
    
    # same thing but for per ppopulation 
    routine_clinperpop <- ggplot(dfpl %>% filter(labels == 'Routine age-based')) + # Catch-up plots are made with cohorts and age-based booster plots with whole simulation 
      geom_col(aes(x = as.factor(pfpr), 
                   y = cases_averted_perpop, 
                   fill = labels, color = labels), 
               position ='dodge', 
               alpha = 0.7) + 
      geom_errorbar(aes(x = as.factor(pfpr), 
                        ymin = cases_averted_perpop_lower, 
                        ymax = cases_averted_perpop_upper, 
                        color = labels),
                    position = position_dodge(width = 0.9), 
                    width = 0.4, 
                    linewidth = 0.38) +
      theme_bw() +
      scale_fill_manual(values = CUcols1) +
      scale_color_manual(values = CUcols_) +
      labs(y = 'Cumulative clinical cases averted\nper 1000 population',
           x = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
           fill = 'Vaccination strategy'
      ) +
      guides(color = 'none') +
      plottheme
    routine_sevperpop <- ggplot(dfpl %>% filter(labels == 'Routine age-based')) + # Catch-up plots are made with cohorts and age-based booster plots with whole simulation 
      geom_col(aes(x = as.factor(pfpr), 
                   y = severe_averted_perpop, 
                   fill = labels, color = labels), 
               position ='dodge', 
               alpha = 0.7) + 
      geom_errorbar(aes(x = as.factor(pfpr), 
                        ymin = severe_averted_perpop_lower, 
                        ymax = severe_averted_perpop_upper, 
                        color = labels),
                    position = position_dodge(width = 0.9), 
                    width = 0.4, 
                    linewidth = 0.38) +
      theme_bw() +
      scale_fill_manual(values = CUcols1) +
      scale_color_manual(values = CUcols_) +
      labs(y = 'Cumulative severe cases averted\nper 1000 population',
           x = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
           fill = 'Vaccination strategy'
      ) +
      guides(color = 'none') +
      plottheme
    routine_clinsevperpop <- cowplot::plot_grid(routine_clinperpop + theme(legend.position="none"), 
                                          routine_sevperpop + theme(legend.position="none"), labels = 'AUTO')
    
    # add the legend to the row we made earlier.
    ggsave(paste0("plots/plot_cumul_routineperpop_clinsev_per.tiff"), 
           plot = routine_clinsevperpop, width = 7.5, height = 2.83, dpi = 500, units = 'in' )
    ggsave(paste0("plots/plot_cumul_routineperpop_clinsev_per.pdf"), 
           plot = routine_clinsevperpop, width = 7.5, height = 2.83, dpi = 500, units = 'in' )
    
    
    # Check on outcomes averted per FVC
    perFVC <- function(df,
                       strategy,
                       variable,
                       seas) {
      
      # CUcols1 <- c('#B03A2E','#6C3483','#1F618D','#00796b','#fbc02d','#CA6F1E','#689f38', '#3498db','tan','#283747','#85929E')
      # CUcols_ <- c('black','#6C3483','#1F618D','#00796b','#fbc02d','#CA6F1E','#689f38', '#3498db','tan','#283747','#85929E')
      # 
      dfpl <- df %>%
        filter(age_grp == '0-100') %>%
        filter(pfpr != 0.03) %>%
        filter(seasonality == seas) %>%
        dplyr::select(c(pfpr, labels, PEVstrategy, EPIextra, contains('relevant'), contains('perFVC'), contains('perpop'), contains('perdose'), -starts_with('AB'))) %>%
        mutate(pfpr = factor(paste0(pfpr * 100, '%'), levels = c('1%','3%','5%','25%','45%','65%')))
      
      # Filter datasets to specific strategies based on plot to make
      if(strategy == 'catch-up no booster'){
        dfpl <- dfpl %>%
          filter((EPIextra == '-'  & PEVstrategy == 'catch-up') | (PEVstrategy == 'AB' & EPIextra == '-'))
      } else if(strategy == 'age-based'){
        dfpl <- dfpl %>%
          filter(PEVstrategy == 'AB')   
      }
      
      A_perfvc <- ggplot(dfpl) +
        geom_col(aes(x = as.factor(pfpr), 
                     y = .data[[paste0(variable, '_perFVC')]], 
                     fill = labels, color = labels), 
                 position ='dodge', 
                 alpha = 0.7) + 
        geom_errorbar(aes(x = as.factor(pfpr), 
                          ymin = .data[[paste0(variable, "_perFVC_lower")]],
                          ymax = .data[[paste0(variable, "_perFVC_upper")]], 
                          color = labels),
                      position = position_dodge(width = 0.9), 
                      width = 0.35, 
                      linewidth = 0.7) +
        theme_bw() +
        scale_fill_manual(values = CUcols1) +
        scale_color_manual(values = CUcols_) +
        labs(y = 'Cumulative clinical cases averted\nper 1,000 FVC',
             x = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
             fill = 'Vaccination strategy'
        ) +
        guides(color = 'none') +
        theme(axis.title = element_text(size = 8),
              # plot.title = element_text(size = 22),
              legend.text = element_text(size = 10),
              strip.text = element_text(size = 8),
              legend.title = element_text(size = 10),
              plot.caption = element_text(size = 10),
              legend.key.size = unit(0.3, 'cm'),
              axis.text.x = element_text(size = 8),
              axis.text.y = element_text(size = 8),
              plot.margin = margin(t = 2,  # Top margin
                                   r = 2,  # Right margin
                                   b = 2,  # Bottom margin
                                   l = 2)
        )
      return(A_perfvc)
    }

    # Make and save all versions of plot by seasonality and by severe v clinical
    for(s in seq_along(seasonalities)){
      CACU_fvc <- perFVC(df_summ, 
                         strategy = 'catch-up no booster',
                         variable = 'cases_averted',
                         seas = seasonalities[s]) 
      ggsave(paste0("plots/plot_cumulCA_CU_perFVC", seasonalities[s], ".tiff"), 
             plot = CACU_fvc, width = 7.5, height = 2.83, dpi = 500,            units = 'in' )
      ggsave(paste0("plots/plot_cumulCA_CU_perFVC", seasonalities[s], ".pdf"), 
             plot = CACU_fvc, width = 7.5, height = 2.83, dpi = 500,            units = 'in' )
      
      CASU_fvc <- perFVC(df_summ, 
                         strategy = 'catch-up no booster',
                         variable = 'severe_averted',
                         seas = seasonalities[s]) 
      ggsave(paste0("plots/plot_cumulSA_CU_perFVC", seasonalities[s], ".tiff"), 
             plot = CACU_fvc, width = 7.5, height = 2.83, dpi = 500,            units = 'in' )
      ggsave(paste0("plots/plot_cumulSA_CU_perFVC", seasonalities[s], ".pdf"), 
             plot = CACU_fvc, width = 7.5, height = 2.83, dpi = 500,            units = 'in' )
    }
  }
}




