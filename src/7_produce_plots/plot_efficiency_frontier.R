# Function to plot efficiency frontier 

plot_efficiency_frontier <- function(df,
                                     seas_type = 'seasonal'){
  
  # Filter out hybrid and SV because are not going to actually compare them in the paper 
  df1 <- df %>%
    filter(PEVstrategy != 'SV' & PEVstrategy !='hybrid') %>%
    filter(age_lower == 0 & age_upper == 100) %>%
    filter(PEV != 'none' & !(PEVage == '-' & EPIextra == '-')) %>%
    mutate(idstrategy = paste0(PEVstrategy, "  ", PEVage, ", ", EPIextra))
  
  pfprvals <- c(0.01,0.03,0.05,0.25,0.45,0.65)
  seasonalities <- c('perennial','seasonal')
  combos <- expand.grid(pfprvals, seasonalities) 
  colnames(combos) <- c('pfprval','seas')
  
  icers_all <- get_icers(df1)
  # icers_clinical_all <- pmap(combos, 
  #                            function(pfprval, seas){
  #                              d <- df1 %>% 
  #                                filter(pfpr == pfprval & seasonality == seas)
  #                              
  #                              params <- data.frame(
  #                                placeholder = seq(1,50, 1) # the function to convert to the PSA object requires parameters
  #                              )
  #                              
  #                              effects <- d %>% 
  #                                mutate(cases_averted_routine_perpop = cases_averted_routine / n * 1000) %>%
  #                                select(drawID, idstrategy, cases_averted_routine_perpop) %>%
  #                                pivot_wider(names_from = idstrategy, 
  #                                            values_from = cases_averted_routine_perpop) %>%
  #                                select(-drawID)
  #                              
  #                              dosenums <- d %>%
  #                                mutate(additional_doses_perpop = additional_doses/ n) %>%#) %>%# 
  #                                select(drawID, idstrategy, additional_doses_perpop) %>%
  #                                pivot_wider(names_from = idstrategy, 
  #                                            values_from = additional_doses_perpop) %>% 
  #                                select(-drawID)
  #                              
  #                              psa_obj <- make_psa_obj(cost = dosenums,
  #                                                      effectiveness = effects,
  #                                                      parameters = params,
  #                                                      strategies = unique(d$idstrategy),
  #                                                      currency = "doses per person")
  #                              
  #                              psa_sum <- summary(psa_obj,
  #                                                 calc_sds = TRUE)
  #                              
  #                              icers <- calculate_icers(cost = psa_sum$meanCost,
  #                                                       effect = psa_sum$meanEffect,
  #                                                       strategies = psa_sum$Strategy)
  #                              
  #                              icers$pfpr <- pfprval
  #                              icers$seasonality <- seas
  #                              
  #                              return(icers)
  #                              
  #                            })
  # 
  # icers_severe_all <- pmap(combos, 
  #                          function(pfprval, seas){
  #                            d <- df1 %>% 
  #                              filter(pfpr == pfprval & seasonality == seas)
  #                            
  #                            params <- data.frame(
  #                              placeholder = seq(1,50, 1) # the function to convert to the PSA object requires parameters
  #                            )
  #                            
  #                            effects <- d %>% 
  #                              mutate(severe_averted_routine_perpop = severe_averted_routine / n * 1000) %>%
  #                              select(drawID, idstrategy, severe_averted_routine_perpop) %>%
  #                              pivot_wider(names_from = idstrategy, 
  #                                          values_from = severe_averted_routine_perpop) %>%
  #                              select(-drawID)
  #                            
  #                            dosenums <- d %>%
  #                              mutate(additional_doses_perpop = additional_doses/ n) %>%#) %>%# 
  #                              select(drawID, idstrategy, additional_doses_perpop) %>%
  #                              pivot_wider(names_from = idstrategy, 
  #                                          values_from = additional_doses_perpop) %>% 
  #                              select(-drawID)
  #                            
  #                            psa_obj <- make_psa_obj(cost = dosenums,
  #                                                    effectiveness = effects,
  #                                                    parameters = params,
  #                                                    strategies = unique(d$idstrategy),
  #                                                    currency = "doses per person")
  #                            
  #                            psa_sum <- summary(psa_obj,
  #                                               calc_sds = TRUE)
  #                            
  #                            icers <- calculate_icers(cost = psa_sum$meanCost,
  #                                                     effect = psa_sum$meanEffect,
  #                                                     strategies = psa_sum$Strategy)
  #                            
  #                            icers$pfpr <- pfprval
  #                            icers$seasonality <- seas
  #                            
  #                            return(icers)
  #                            
  #                          })
  # 
  # icers_clinical_all <- bind_rows(icers_clinical_all) %>%
  #   mutate(type = 'clinical')
  # icers_severe_all <- bind_rows(icers_severe_all) %>%
  #   mutate(type = 'severe')
  # 
  # icers_all <- rbind(icers_clinical_all, icers_severe_all) %>%
  #   filter(Status != 'D') %>%
  #   filter(pfpr %in% c(0.05,0.25,0.45)) %>%
  #   separate(Strategy,
  #            into = c("PEVstrategy", "PEVage", "EPIextra"),
  #            sep = "\\.\\.",
  #            remove = FALSE) %>%
  #   mutate(across(c(PEVstrategy, PEVage),
  #                 .fns = ~ str_replace(.x, '\\.', '-'))) %>%
  #   mutate(across(c(EPIextra), ~ .x |>
  #                   str_replace("^\\.", "") |>        # remove leading .
  #                   str_replace("^\\.$", "") |>       # replace "." alone with empty
  #                   str_replace_all("\\.", "+")       # convert remaining . to -
  #   )) %>%
  #   mutate(category = ifelse(EPIextra != '' & PEVage == '', 'Extra booster(s)', 
  #                            ifelse(PEVage != '' & EPIextra == '', 'Catch-up', 'Combined')),
  #          EPIextra = factor(EPIextra, levels = c('2y','5y','10y','2y+5y','2y+10y','5y+10y','2y+5y+10y','')),
  #          PEVage = ifelse(PEVage == '5-9','5-9y', ifelse(PEVage == '5-14','5-14y', PEVage)),
  #          PEVage = factor(as.factor(PEVage), levels = c('6m-2y','6m-4y','6m-9y','6m-14y','5-9y','5-14y',''))) %>%
  #   filter(category !='Routine age-based')
  
  
  # If update the efficiency plot, need to make new legend to use with get_eff_frontier_legend.R
  
  # df_plot <- df %>%
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
    
    dfplot <- icers_all %>%
      filter(type == casetype) %>%
      filter(seasonality == seas_type)
    
    lab_data <- dfplot[dfplot$Status == 'ND', ]
    lab_data <- lab_data %>%
      mutate(labels = case_when(
        EPIextra == '' ~ PEVage,
        PEVage == '' ~ EPIextra, 
        TRUE ~ paste0(PEVage, ", ", EPIextra)))
    
    plt <- ggplot(dfplot) +
      geom_line(data = dfplot %>% filter(Status == 'ND') ,
                aes(x = Cost,
                    y = Effect),
                linewidth = 0.7) +
      
      ############ With extended-dominated scenarios
      # Plot standalone routine interventions
      # geom_point(data = dfplot %>% filter(category =='Routine age-based'),
      #            aes(x = Cost,
      #                y = Effect,
      #                shape = category),
      #            color = '#e71d1d',#CUcols[1],
      #            size = 2.5) +
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
  legend_img <- grid::rasterGrob(readPNG("legend2.png"), interpolate=TRUE)
  
  # per 1000 additional doses
  CAadd <- eff_plot(casetype = 'clinical',
                    seas_type = seas_type) + 
    labs(x = 'Additional doses per person',
         y = 'Cumulative additional clinical cases\naverted per 1000 population',
         color = 'Vaccination strategy',
         shape = 'Strategy type')
  CAlegadd <- plot_grid(CAadd, legend_img, rel_widths = c(4,1))
  
  ggsave(paste0('plots/CAbyadditionaldoses', seas_type, '.svg'), CAlegadd, width = 7.5, height = 4.5, dpi = 500,            
         units = 'in')
  ggsave(paste0('plots/CAbyadditionaldoses', seas_type, '.pdf'), CAlegadd, width = 7.5, height = 4.5, dpi = 500,            
         units = 'in')
  
  SAadd <- eff_plot(casetype = 'severe',
                    seas_type = seas_type) + 
    labs(x = 'Additional doses per person',
         y = 'Cumulative additional severe cases\naverted per 1000 population',
         color = 'Vaccination strategy',
         shape = 'Strategy type')
  SAlegadd <- plot_grid(SAadd, legend_img, rel_widths = c(4,1))
  ggsave(paste0('plots/SAbyadditionaldoses', seas_type,'.svg'), SAlegadd, width = 7.5, height = 4.5, dpi = 500,            
         units = 'in')
  ggsave(paste0('plots/SAbyadditionaldoses', seas_type,'.pdf'), SAlegadd, width = 7.5, height = 4.5, dpi = 500,            
         units = 'in')
  
  averted_pltadd <- cowplot::plot_grid(CAadd + theme(legend.position="none"), 
                                       SAadd + theme(legend.position="none"), 
                                       ncol = 1, labels = 'AUTO')
  avertedwlegadd <- plot_grid(averted_pltadd, legend_img, 
                              ncol = 2, rel_widths = c(4,1))
  ggsave(paste0('plots/CASAbyadditionaldoses', seas_type, '.svg'), avertedwlegadd,
         width = 9, height = 5.75, dpi = 500,            
         units = 'in')
  ggsave(paste0('plots/CASAbyadditionaldoses', seas_type, '.pdf'), avertedwlegadd,
         width = 9, height = 5.75, dpi = 500,            
         units = 'in')
}