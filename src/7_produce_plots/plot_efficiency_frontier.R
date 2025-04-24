# Function to plot efficiency frontier 

plot_efficiency_frontier <- function(df,
                                     seas_type = 'seasonal',
                                     pfpr_vec = c(0.05, 0.25, 0.45)){
  
  # If update the efficiency plot, need to make new legend to use with get_eff_frontier_legend.R
  
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
    
    
  # booster_colors <- c("2y" = "#85ecd1" ,"5y" = "#72d5d9", "10y" = "#8abae7", '2y+5y' = "#acace9" ,"2y+10y" = "#6c8bf7", "5y+10y" = "#4d47d5",  '2y+5y+10y' = "#160e6f")
  booster_colors <- c("2y" = "#c1ef7b" ,"5y" = "#85ecd1", "10y" = "#83bae7" , '2y+5y' = "#acace9","2y+10y" = "#6c8bf7", "5y+10y" = "#4d47d5",  '2y+5y+10y' = "#160e6f")
  
  # booster_colors <- c("2y" = "#85ece8" ,"5y" = "#5fc8e0", "10y" = "#83bae7" , '2y+5y' = "#acace9","2y+10y" = "#6c8bf7", "5y+10y" = "#4d47d5",  '2y+5y+10y' = "#160e6f")
  catch_up_colors <- c("6m-2y" = "#90b260", "6m-4y" = "#efc642", "6m-9y" = "#fd7270", "6m-14y" = "#ce5800",
                       "5-9y" = "#991010", "5-14y" = "#65612c")
  
  
  ## can think about setting ylim minimum to 0 for these plots 
  eff_plot <- function(var, eff_var){
    dfpl1 <- df_plot %>% filter(.data[[eff_var]] == 1) %>%
      mutate(dosesper1000 = totaldoses / n *1000)
    # dfpl2 <- df_plot %>% filter(.data[[eff_var]] == 0) %>%
    #   mutate(dosesper1000 = totaldoses / n *1000)
    
    pfpr.labs <- c("5%", "25%", '45%')
    names(pfpr.labs) <- c("0.05","0.25", "0.45")
    

    plt <- ggplot(dfpl1) +
      geom_line(data = dfpl1, 
                aes(x = dosesper1000,
                    y = .data[[var]]),
                linewidth = 0.7) +
      
      ############ With non-dominated scenarios
      # Plot standalone routine interventions
      geom_point(data = dfpl1 %>% filter(category =='Routine age-based'),
                 aes(x = dosesper1000,
                     y = .data[[var]],
                     shape = category),
                 color = '#e71d1d',#CUcols[1],
                 size = 4.5) +
      # standalone boosters
      geom_point(data = dfpl1 %>% filter(category == 'Extra booster(s)'),
                 aes(x = dosesper1000,
                     y = .data[[var]],
                     color = EPIextra,
                     shape = category),
                 size = 4.5,
                 position = position_nudge(x = -50)) +
      # standalone catch-up
      geom_point(data = dfpl1 %>% filter(category == 'Catch-up'),
                 aes(x = dosesper1000,
                     y = .data[[var]],
                     fill = PEVage,
                     shape = category),
                 size = 4,
                 color = '#ffffff00',
                 position = position_nudge(x = -50)) +
      # combined
      geom_point(data = dfpl1 %>% filter(category == 'Combined')%>% mutate(category = 'Extra booster(s)'),
                 aes(x = dosesper1000,
                     y = .data[[var]],
                     color = EPIextra,
                     shape = category),
                 size = 4.5,
                 position = position_nudge(x = -50)) +
      geom_point(data = dfpl1 %>% filter(category == 'Combined') %>% mutate(category = 'Catch-up'),
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
  legend_img <- grid::rasterGrob(readPNG("legend.png"), interpolate=TRUE)
  
  #Make plots 
  CA <- eff_plot(var = 'cases_averted_perpop', eff_var = 'maxCA') + 
    labs(x = 'Doses per 1000 population',
         y = 'Cumulative clinical cases\naverted per 1000 population',
         color = 'Vaccination strategy',
         shape = 'Strategy type')
  CAleg <- plot_grid(CA, legend_img, rel_widths = c(4,1))
  
  ggsave(paste0('plots/CAbytotaldoses', seas_type, '.pdf'), CAleg, width = 14, height = 8)
  
  SA <- eff_plot(var = 'severe_averted_perpop', eff_var = 'maxSA') + 
    labs(x = 'Doses per 1000 population',
         y = 'Cumulative severe cases\naverted per 1000 population',
         color = 'Vaccination strategy',
         shape = 'Strategy type')
  SAleg <- plot_grid(SA, legend_img, rel_widths = c(4,1))
  ggsave(paste0('plots/SAbytotaldoses', seas_type,'.pdf'), SAleg, width = 14, height = 8)
  
  averted_plt <- cowplot::plot_grid(CA + theme(legend.position="none"), 
                                SA + theme(legend.position="none"), 
                                ncol = 1, labels = 'AUTO')
  avertedwleg <- plot_grid(averted_plt, legend_img, 
                           ncol = 2, rel_widths = c(4,1))
  # extract the legend from one of the plots
  # legend <- get_legend(
  #   # create some space to the left of the legend
  #   CA + theme(legend.box.margin = margin(0, 0, 0, 12))
  # )
  # 
  # # add the legend to the row we made earlier. Give it one-third of 
  # # the width of one plot (via rel_widths).
  # averted_plt <- plot_grid(averted, legend, 
  #                          ncol = 2, rel_widths = c(3.2, 1.1))
  
  ggsave(paste0('plots/CASAbytotaldoses', seas_type, '.pdf'), avertedwleg, width = 14, height = 8)
  
  # Cases and severe cases
  cases <- eff_plot(var = 'cases_perpop', eff_var = 'mincases') + 
    labs(x = 'Doses per 1000 population',
         y = 'Cumulative uncomplicated cases\nper 1000 population',
         color = 'Vaccination strategy',
         shape = 'Strategy type')
  casesleg <- plot_grid(cases, legend_img, rel_widths = c(4,1))
  ggsave(paste0('plots/casesbytotaldoses',seas_type,'.pdf'), casesleg, width = 14, height = 8)
  
  severe <- eff_plot(var = 'sevcases_perpop', eff_var = 'minsev') + 
    labs(x = 'Doses per 1000 population',
         y = 'Cumulative severe cases\nper 1000 population',
         color = 'Vaccination strategy',
         shape = 'Strategy type')  
  severeleg <- plot_grid(severe, legend_img, rel_widths = c(4,1))
  ggsave(paste0('plots/sevcasesbytotaldoses',seas_type,'.pdf'), severeleg, width = 14, height = 8)
  
  
  # Find efficiency frontier across all strategies an dall settings 
  df_plot <- df %>%
    filter(PEVstrategy == 'catch-up' | PEVstrategy == 'AB') %>% 
    filter(age_grp == '0-100') %>% ungroup() %>%
    arrange(totaldoses, .by_group = TRUE) %>% 
    mutate(mincases = cummin(cases),
           minsev = cummin(sevcases),
           maxCA = cummax(cases_averted),
           maxSA = cummax(severe_averted)
    ) %>% ungroup() %>%
    mutate(mincases = ifelse(cases == mincases, 1, 0),
           minsev = ifelse(sevcases == minsev, 1, 0),
           maxCA = ifelse(cases_averted == maxCA, 1, 0),
           maxSA = ifelse(severe_averted == maxSA, 1, 0)) %>%
    # Only want this variable for the 0-100 age group
    mutate(across(c(mincases, minsev, maxCA, maxSA), ~ ifelse(age_grp != '0-100', NA, .x))) #%>%
    # mutate(labels = factor(labels, levels = c('Routine age-based', "2y booster","5y booster", "10y booster","5y, 10y boosters",
    #                                           '6m-2y', '6m-2y;\n2y booster', '6m-2y;\n5y booster', '6m-2y;\n10y booster', '6m-2y;\n5y, 10y boosters',
    #                                           '6m-4y', '6m-4y;\n2y booster', '6m-4y;\n5y booster', '6m-4y;\n10y booster', '6m-4y;\n5y, 10y boosters',
    #                                           '6m-9y', '6m-9y;\n2y booster', '6m-9y;\n5y booster', '6m-9y;\n10y booster', '6m-9y;\n5y, 10y boosters',
    #                                           '6m-14y', '6m-14y;\n2y booster', '6m-14y;\n5y booster', '6m-14y;\n10y booster', '6m-14y;\n5y, 10y boosters',
    #                                           '5-9y', '5-9y;\n2y booster', '5-9y;\n5y booster', '5-9y;\n10y booster', '5-9y;\n5y, 10y boosters',
    #                                           '5-14y', '5-14y;\n2y booster', '5-14y;\n5y booster', '5-14y;\n10y booster', '5-14y;\n5y, 10y boosters',
    #                                           'None')))
  
  eff_plot2 <- function(var, eff_var){
    
    df_plot$labels <- paste0(df_plot$labels, " ", df_plot$pfpr)
    
    dfpl1 <- df_plot %>% filter(.data[[eff_var]] == 1) %>%
      mutate(dosesper1000 = totaldoses / n *1000)
    dfpl2 <- df_plot %>% filter(.data[[eff_var]] == 0) %>%
      mutate(dosesper1000 = totaldoses / n *1000)
    
    pfpr.labs <- c("1%", "5%", "25%", '45%', "65%")
    names(pfpr.labs) <- c("0.01","0.05","0.25", "0.45","0.65")
    
    plt <- ggplot(dfpl1) +
      geom_point(data = dfpl2,
                 aes(x = dosesper1000,#totaldoses/n*1000,
                     y = .data[[var]],#/n*1000,
                     color = labels,
                     shape = PEVstrategy),
                 alpha = 0, size = 2) +
      geom_line(aes(x = dosesper1000,#totaldoses/n*1000,
                    y = .data[[var]]),#/n*1000),
                linewidth = 0.7) +
      geom_point(aes(x = dosesper1000,#totaldoses/n*1000, 
                     y = .data[[var]],#/n*1000, 
                     color = labels, 
                     shape = PEVstrategy), size = 4) +
      geom_text_repel(data = dfpl1,
                      aes(x = dosesper1000,#totaldoses/n*1000,
                          y = .data[[var]],#/n*1000,
                          color = labels,
                          label = labels),
                      size = 4,
                      verbose = TRUE,
                      box.padding = 0.5,
                      point.padding = 0.5,
                      max.time = 2,
                      seed = 123,
                      min.segment.length = 0.1,
                      force = 0.7,
                      direction = 'both',
                      fontface = 'bold') +
      # scale_color_manual(values = colors, drop = FALSE) +
      scale_shape_manual(values = c(17, 16, 15, 18), drop = FALSE) +
      scale_x_continuous(labels = scales::label_comma()) +
      facet_wrap(~seasonality) +
      theme_bw(base_size = 12) +
      theme(axis.title = element_text(size = 12),
            plot.title = element_text(size = 18),
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 12),
            legend.position = 'none',
            strip.text.x = element_text(size = 10),
            strip.text.y = element_text(size = 10),
            plot.caption = element_text(size = 12),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10)
      )
    return(plt)
  }
  
  CAALL <- eff_plot2(var = 'cases_averted_perpop', eff_var = 'maxCA') + 
    labs(x = 'Doses per 1000 population',
         y = 'Cumulative clinical cases\naverted per 1000 population',
         color = 'Vaccination strategy',
         shape = 'Strategy type')
  
  ggsave('plots/CAbytotaldoses_ALLSETTINGS.pdf', CAALL, width = 14, height = 8)
  
  
  SAALL <- eff_plot2(var = 'severe_averted_perpop', eff_var = 'maxSA') + 
    labs(x = 'Doses per 1000 population',
         y = 'Cumulative severe cases\naverted per 1000 population',
         color = 'Vaccination strategy',
         shape = 'Strategy type')
  
  ggsave('plots/SAbytotaldoses_ALLSETTINGS.pdf', SAALL, width = 14, height = 8)
  
}
