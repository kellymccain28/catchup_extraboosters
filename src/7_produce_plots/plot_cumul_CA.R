
# Function to loop over all options to make this plot 

plot_cumul_CA <- function(df_last15, df_summ, cohorts){
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
    CUcols1 <- c('#B03A2E','#6C3483','#1F618D','#00796b','#fbc02d','#CA6F1E','#689f38', '#3498db','tan','#283747','#85929E')
    CUcols_ <- c('black','#6C3483','#1F618D','#00796b','#fbc02d','#CA6F1E','#689f38', '#3498db','tan','#283747','#85929E')
    
    cohortspl <- cohorts %>%
      filter(pfpr != 0.03) %>%
      filter(seasonality == seas) %>%
      mutate(pfpr = factor(paste0(pfpr * 100, '%'), levels = c('1%','3%','5%','25%','45%','65%')))
       
    
    dfpl <- df %>%
      filter(age_grp == '0-100') %>%
      filter(pfpr != 0.03) %>%
      filter(seasonality == seas) %>%
      dplyr::select(c(pfpr, labels, PEVstrategy, EPIextra, contains('relevant'), contains('perpop'), contains('perdose'), -starts_with('AB'))) %>%
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
    
    A <- ggplot(dfpl) +
      geom_col(aes(x = as.factor(pfpr), 
                   y = .data[[paste0(compare, variable, '_perpop')]], 
                   fill = labels, color = labels), 
               position ='dodge', 
               alpha = 0.7) + 
      geom_errorbar(aes(x = as.factor(pfpr), 
                        ymin = .data[[paste0(compare, variable, "_perpop_lower")]],
                        ymax = .data[[paste0(compare, variable, "_perpop_upper")]], 
                        color = labels),
                    position = position_dodge(width = 0.9), 
                    width = 0.35, 
                    linewidth = 0.7) +
      theme_bw() +
      scale_fill_manual(values = CUcols1) +
      scale_color_manual(values = CUcols_) +
      labs(y = if(variable == 'cases_averted'){'Cumulative clinical cases averted\nper 1,000 population'} 
           else {'Cumulative severe cases averted\nper 1,000 population'},
           x = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
           fill = 'Vaccination strategy'
      ) +
      guides(color = 'none') +
      theme(axis.title = element_text(size = 20),
            plot.title = element_text(size = 22),
            legend.text = element_text(size = 15),
            strip.text.x = element_text(size = 12),
            legend.title = element_text(size = 18),
            plot.caption = element_text(size = 12),
            legend.key.size = unit(0.8, 'cm'),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12)
      )
    
    B <- ggplot(if(strategy == 'catch-up no booster' | strategy == 'catch-up all'){copl} else dfpl) + # Catch-up plots are made with cohorts and age-based booster plots with whole simulation 
      geom_col(aes(x = as.factor(pfpr), 
                   y = .data[[paste0(variable, "_perdose")]], 
                   fill = labels, color = labels), 
               position ='dodge', 
               alpha = 0.7) + 
      geom_errorbar(aes(x = as.factor(pfpr), 
                        ymin = .data[[paste0(variable, "_perdose_lower")]], 
                        ymax = .data[[paste0(variable, "_perdose_upper")]], 
                        color = labels),
                    position = position_dodge(width = 0.9), 
                    width = 0.35, 
                    linewidth = 0.7) +
      theme_bw() +
      scale_fill_manual(values = CUcols1) +
      scale_color_manual(values = CUcols_) +
      labs(y = if(variable == 'cases_averted'){'Cumulative clinical cases averted\nper 1,000 doses'} 
           else {'Cumulative severe cases averted\nper 1,000 doses'},
           x = str2expression(paste("Baseline ", expression(italic(Pf)~PR[2-10]), sep = '~')),
           fill = 'Vaccination strategy'
           ) +
      guides(color = 'none') +
      theme(axis.title = element_text(size = 20),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            plot.caption = element_text(size = 14),
            legend.title = element_text(size = 18),
            legend.text = element_text(size = 14),
            legend.key.size = unit(0.8, 'cm'),
            strip.text.x = element_text(size = 12),
            strip.text.y = element_text(size = 12)
      )
    
    p <- cowplot::plot_grid(A + theme(axis.title = element_text(size = 14), 
                                      legend.position="none"), 
                            B + theme(axis.title = element_text(size = 14),
                                      legend.position="none"), labels = 'AUTO')
    
    # extract the legend from one of the plots
    legend <- cowplot::get_plot_component(
      # create some space to the left of the legend
      A + theme(legend.box.margin = margin(0, 2, 0, 12)),
      'guide-box-right', return_all = TRUE)
    
    # add the legend to the row we made earlier.
    plot <- plot_grid(p, legend, rel_widths = c(4, 0.7))
    
    # Save files to refer to in text 
    if((strategy == 'catch-up no booster' | strategy == 'age-based') & variable == 'cases_averted') {
      write.csv(copl %>% select(pfpr, seasonality, labels, PEVage, PEVstrategy, 
                                contains('perdose')), paste0('plots/cohorts_CAperdose_', strategy, seas,".csv"))
    }
    return(list(plot, A, B))
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
    ggsave(paste0("plots/plot_cumulCA_catchupnobooster_none_", seasonalities[s], ".pdf"), 
           plot = CAcatchup_noboost_none[[1]], width = 16, height = 6)
    
    CAAB_none <- plot_cumul_CA2(df_last15, 
                                strategy = 'age-based',
                                compare = '',
                                variable = 'cases_averted',
                                seas = seasonalities[s])
    ggsave(paste0("plots/plot_cumulCA_AB_none_", seasonalities[s], ".pdf"), 
           plot = CAAB_none[[1]], width = 16, height = 6) 
    
    CAseasonal_routine <- plot_cumul_CA2(df_summ,
                                         strategy = 'seasonal routine',
                                         compare = '',
                                         variable = 'cases_averted',
                                         seas = seasonalities[s])
    ggsave(paste0("plots/plot_cumulCA_SVhybrid_none_", seasonalities[s], ".pdf"), 
           plot = CAseasonal_routine[[1]], width = 16, height = 6)
    
    # CAcombination <- plot_cumul_CA2(df_summ,
    #                                      strategy = 'combination',
    #                                      compare = '',
    #                                      variable = 'cases_averted',
    #                                      seas = seasonalities[s])
    # ggsave(paste0("plots/plot_cumulCA_combination_none_", seasonalities[s], ".pdf"), 
    #        plot = CAcombination[[1]], width = 16, height = 6)
    
    # severe cases
    SAcatchup_noboost_none <- plot_cumul_CA2(df_summ, 
                                             strategy = 'catch-up no booster',
                                             compare = '',
                                             variable = 'severe_averted',
                                             seas = seasonalities[s]) 
    ggsave(paste0("plots/plot_cumulSA_catchupnobooster_none_", seasonalities[s], ".pdf"), 
           plot = SAcatchup_noboost_none[[1]], width = 16, height = 6)
    
    SAAB_none <- plot_cumul_CA2(df_last15, 
                                strategy = 'age-based',
                                compare = '',
                                variable = 'severe_averted',
                                seas = seasonalities[s]) 
    ggsave(paste0("plots/plot_cumulSA_AB_none_", seasonalities[s], ".pdf"), 
           plot = SAAB_none[[1]], width = 16, height = 6)
    
    SAseasonal_routine <- plot_cumul_CA2(df_summ,
                                         strategy = 'seasonal routine',
                                         compare = '',
                                         variable = 'severe_averted',
                                         seas = seasonalities[s])
    ggsave(paste0("plots/plot_cumulSA_SVhybrid_none_", seasonalities[s], ".pdf"), 
           plot = SAseasonal_routine[[1]], width = 16, height = 6)
    
    # SAcombination <- plot_cumul_CA2(df_summ,
    #                                 strategy = 'combination',
    #                                 compare = '',
    #                                 variable = 'severe_averted',
    #                                 seas = seasonalities[s])
    # ggsave(paste0("plots/plot_cumulSA_combination_none_", seasonalities[s], ".pdf"), 
    #        plot = SAcombination[[1]], width = 16, height = 6)
    
    # Make plot with both clinical and severe + doses and pop
    legend <- cowplot::get_plot_component(
      # create some space to the left of the legend
      CAcatchup_noboost_none[[1]] + theme(legend.box.margin = margin(0, 2, 0, 12)),
      'guide-box-right',
      return_all = TRUE)
    
    # Catch-up no boost
    CASAcatchup_noboost_none1 <- cowplot::plot_grid(CAcatchup_noboost_none[[1]]+ theme(legend.position="none"), 
                                                    SAcatchup_noboost_none[[1]] + theme(legend.position="none"), 
                                                    labels = 'AUTO',
                                                    nrow = 2)
    # add the legend to the row we made earlier.
    CASAcatchup_noboost_none <- plot_grid(CASAcatchup_noboost_none1, legend, rel_widths = c(4, 0.7))
    
    ggsave(paste0("plots/plot_cumulCASA_catchupnobooster_none_", seasonalities[s], ".pdf"), CASAcatchup_noboost_none, width = 16, height = 6)
    
    # Age-based
    CASAAB_none1 <- cowplot::plot_grid(CAAB_none[[1]] + theme(legend.position="none"), 
                                       SAAB_none[[1]] + theme(legend.position="none"), 
                                       labels = 'AUTO',
                                       nrow = 2)
    # add the legend to the row we made earlier.
    CASAAB_none <- plot_grid(CASAAB_none1, legend, rel_widths = c(4, 0.7))
    ggsave(paste0("plots/plot_cumulCASA_AB_none_", seasonalities[s], ".pdf"), CASAcatchup_noboost_none, width = 16, height = 6)
    
    # Combination
    # combo1 <- cowplot::plot_grid(CAcombination[[1]] + theme(legend.position="none"), 
    #                             SAcombination[[1]] + theme(legend.position="none"), 
    #                                    labels = 'AUTO',
    #                                    nrow = 2)
    # # add the legend to the row we made earlier.
    # combo <- plot_grid(combo1, legend, rel_widths = c(4, 0.7))
    # ggsave(paste0("plots/plot_cumulCASA_combo_none_", seasonalities[s], ".pdf"), combo, width = 16, height = 6)
    
    
    ####### Make plot with clinical and severe just for the per dose for CATCH-UP
    CASACU <- cowplot::plot_grid(CAcatchup_noboost_none[[3]] + theme(axis.title = element_text(size = 14), 
                                                                   legend.position="none"), 
                                 SAcatchup_noboost_none[[3]] + theme(axis.title = element_text(size = 14),
                                                                   legend.position="none"), labels = 'AUTO')
    
    # extract the legend from one of the plots
    legend <- cowplot::get_plot_component(
      # create some space to the left of the legend
      CAcatchup_noboost_none[[2]] + theme(legend.box.margin = margin(0, 2, 0, 12)),
      'guide-box-right')
    
    # add the legend to the row we made earlier.
    plotCASA_CU <- plot_grid(CASACU, legend, rel_widths = c(4, 0.7))
    ggsave(paste0("plots/plot_cumulCASA_CU_", seasonalities[s], ".pdf"), 
           plot = plotCASA_CU, width = 16, height = 6)
    
    ######### Make plot with clinical and severe just for the per dose for AGE-BASED
    CASAAB <- cowplot::plot_grid(CAAB_none[[3]] + theme(axis.title = element_text(size = 14), 
                                                                   legend.position="none"), 
                                 SAAB_none[[3]] + theme(axis.title = element_text(size = 14),
                                                                   legend.position="none"), labels = 'AUTO')
    
    # extract the legend from one of the plots
    legend <- cowplot::get_plot_component(
      # create some space to the left of the legend
      CAAB_none[[2]] + theme(legend.box.margin = margin(0, 2, 0, 12)),
      'guide-box-right')
    
    # add the legend to the row we made earlier.
    plotCASA_AB <- plot_grid(CASAAB, legend, rel_widths = c(4, 0.7))
    ggsave(paste0("plots/plot_cumulCASA_AB_", seasonalities[s], ".pdf"), 
           plot = plotCASA_AB, width = 16, height = 6)
    
    ####### Make plot with clinical and severe just for the per dose for COMBO
    # combodose1 <- cowplot::plot_grid(CAcombination[[3]] + theme(axis.title = element_text(size = 14), 
    #                                                                  legend.position="none"), 
    #                              SAcombination[[3]] + theme(axis.title = element_text(size = 14),
    #                                                                  legend.position="none"), labels = 'AUTO')
    # 
    # # extract the legend from one of the plots
    # legend <- cowplot::get_plot_component(
    #   # create some space to the left of the legend
    #   CAcombination[[2]] + theme(legend.box.margin = margin(0, 0, 0, 12)),
    #   'guide-box-right')
    # 
    # # add the legend to the row we made earlier.
    # plotCASA_combo <- plot_grid(combodose1, legend, rel_widths = c(4, 0.7))
    # ggsave(paste0("plots/plot_cumulCASA_combo_", seasonalities[s], ".pdf"), 
    #        plot = combodose1, width = 16, height = 6)
    
    ####### Make plot with clinical and severe just for the per POP for CATCH-UP
    CASACU <- cowplot::plot_grid(CAcatchup_noboost_none[[2]] + theme(axis.title = element_text(size = 14), 
                                                                     legend.position="none"), 
                                 SAcatchup_noboost_none[[2]] + theme(axis.title = element_text(size = 14),
                                                                     legend.position="none"), labels = 'AUTO')
    
    # extract the legend from one of the plots
    legend <- cowplot::get_plot_component(
      # create some space to the left of the legend
      CAcatchup_noboost_none[[2]] + theme(legend.box.margin = margin(0, 2, 0, 12)),
      'guide-box-right')
    
    # add the legend to the row we made earlier.
    plotCASA_CU <- plot_grid(CASACU, legend, rel_widths = c(4, 0.7))
    ggsave(paste0("plots/plot_cumulCASAperpop_CU_", seasonalities[s], ".pdf"), 
           plot = plotCASA_CU, width = 16, height = 6)
    
    ######### Make plot with clinical and severe just for the per pop for AGE-BASED
    CASAAB <- cowplot::plot_grid(CAAB_none[[2]] + theme(axis.title = element_text(size = 14), 
                                                        legend.position="none"), 
                                 SAAB_none[[2]] + theme(axis.title = element_text(size = 14),
                                                        legend.position="none"), labels = 'AUTO')
    
    # extract the legend from one of the plots
    legend <- cowplot::get_plot_component(
      # create some space to the left of the legend
      CAAB_none[[2]] + theme(legend.box.margin = margin(0, 2, 0, 12)),
      'guide-box-right')
    
    # add the legend to the row we made earlier.
    plotCASA_AB <- plot_grid(CASAAB, legend, rel_widths = c(4, 0.7))
    ggsave(paste0("plots/plot_cumulCASAperpop_AB_", seasonalities[s], ".pdf"), 
           plot = plotCASA_AB, width = 16, height = 6)
    
    
    ######### Make plot with clinical and severe just for the per pop for COMBO
    # comboperpop <- cowplot::plot_grid(CAcombination[[2]] + theme(axis.title = element_text(size = 14), 
    #                                                     legend.position="none"), 
    #                                   SAcombination[[2]] + theme(axis.title = element_text(size = 14),
    #                                                     legend.position="none"), labels = 'AUTO')
    # 
    # # extract the legend from one of the plots
    # legend <- cowplot::get_plot_component(
    #   # create some space to the left of the legend
    #   CAcombination[[2]] + theme(legend.box.margin = margin(0, 0, 0, 12)),
    #   'guide-box-right')
    # 
    # # add the legend to the row we made earlier.
    # plotCASA_combo <- plot_grid(comboperpop, legend, rel_widths = c(4, 0.7))
    # ggsave(paste0("plots/plot_cumulCASAperpop_combo_", seasonalities[s], ".pdf"), 
    #        plot = plotCASA_combo, width = 16, height = 6)
    
    
    # Check on outcomes averted per FVC
    perFVC <- function(df,
                       strategy,
                       variable,
                       seas) {
      
      CUcols1 <- c('#B03A2E','#6C3483','#1F618D','#00796b','#fbc02d','#CA6F1E','#689f38', '#3498db','tan','#283747','#85929E')
      CUcols_ <- c('black','#6C3483','#1F618D','#00796b','#fbc02d','#CA6F1E','#689f38', '#3498db','tan','#283747','#85929E')
      
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
        theme(axis.title = element_text(size = 20),
              plot.title = element_text(size = 22),
              legend.text = element_text(size = 15),
              strip.text.x = element_text(size = 12),
              legend.title = element_text(size = 18),
              plot.caption = element_text(size = 12),
              legend.key.size = unit(0.8, 'cm'),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12)
        )
      return(A_perfvc)
    }

    # Make and save all versions of plot by seasonality and by severe v clinical
    for(s in seq_along(seasonalities)){
      CACU_fvc <- perFVC(df_summ, 
                         strategy = 'catch-up no booster',
                         variable = 'cases_averted',
                         seas = seasonalities[s]) 
      ggsave(paste0("plots/plot_cumulCA_CU_perFVC", seasonalities[s], ".pdf"), 
             plot = CACU_fvc, width = 16, height = 6)
      
      CASU_fvc <- perFVC(df_summ, 
                         strategy = 'catch-up no booster',
                         variable = 'severe_averted',
                         seas = seasonalities[s]) 
      ggsave(paste0("plots/plot_cumulSA_CU_perFVC", seasonalities[s], ".pdf"), 
             plot = CACU_fvc, width = 16, height = 6)
    }
  }
}




