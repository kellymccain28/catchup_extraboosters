# figures for AMMnet and MIM  
# source('R:/Kelly/catchupR21/src/6_produce_plots/plot_themes.R')
CUcols <- c('#B03A2E','#B9770E','#1F618D','#148F77','#6C3483','#239B56', 'tan','#283747','#85929E')
CUcols_ <- c('black','#B9770E','#1F618D','#148F77','#6C3483','#239B56', 'tan','#283747','#85929E')
# library(stringr)
# library(ggplot2)
# library(dplyr)
makeplots <- function(){
  # Plot cohorts by age
  plot_cohortsage <- function(var,
                              strat,
                              seas){
    ggplot(data = cohorts_byage %>% 
             filter(pfpr %in% c(0.05, 0.25, 0.45), 
                    PEVstrategy == strat,
                    seasonality == seas)) + 
      geom_point(aes(x = age_lower, 
                     y = .data[[var]], 
                     color = halfyear), 
                 alpha = 0.3, size = 2) + 
      geom_line(aes(x = age_lower, 
                    y = .data[[var]], 
                    color = halfyear, group = halfyear), 
                alpha = 0.3) + 
      facet_grid(rows = vars(pfpr),
                 cols = vars(labels), 
                 scales = 'free_y') +
      geom_hline(aes(yintercept = 0), 
                 color = 'red', 
                 linetype = 3) +
      scale_color_viridis_c(option = 'D') +
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
  # seasonal
  pp <- plot_cohortsage(var = 'cases_averted_perpop',
                        strat = 'catch-up',
                        seas = 'seasonal')
  ggsave("plots/cohorts_CU_CAperpop_AMMnetMIM_seas.tiff", pp, width = 14, height = 8, dpi = 300)
  # ppp <- plot_cohortsage(var = 'cases_averted_perdose',
  #                        strat = 'catch-up',
  #                        seas = 'seasonal')
  # ggsave("plots/cohorts_CU_CAperdose_AMMnetMIM_seas.tiff", ppp, width = 10, height = 6)
  pppp <- plot_cohortsage(var = 'cases_per1000pop',
                         strat = 'catch-up',
                         seas = 'seasonal')
  ggsave("plots/cohorts_CU_casesperpop_AMMnetMIM_seas.tiff", pppp, width = 14, height = 8, dpi = 300)
  
  pp <- plot_cohortsage(var = 'cases_averted_perpop',
                        strat = 'AB',
                        seas = 'seasonal')
  ggsave("plots/cohorts_AB_CAperpop_AMMnetMIM_seas.tiff", pp, width = 14, height = 8, dpi = 300)
  # ppp <- plot_cohortsage(var = 'cases_averted_perdose',
  #                        strat = 'AB',
  #                        seas = 'seasonal')
  # ggsave("plots/cohorts_AB_CAperdose_AMMnetMIM_seas.tiff", ppp, width = 10, height = 6)
  pppp <- plot_cohortsage(var = 'cases_per1000pop',
                          strat = 'AB',
                          seas = 'seasonal')
  ggsave("plots/cohorts_AB_casesperpop_AMMnetMIM_seas.tiff", pppp, width = 14, height = 8, dpi = 300)
  
  #perennial
  pp <- plot_cohortsage(var = 'cases_averted_perpop',
                        strat = 'catch-up',
                        seas = 'perennial')
  ggsave("plots/cohorts_CU_CAperpop_AMMnetMIM_per.tiff", pp, width = 14, height = 8, dpi = 300)
  # ppp <- plot_cohortsage(var = 'cases_averted_perdose',
  #                        strat = 'catch-up',
  #                        seas = 'perennial')
  # ggsave("plots/cohorts_CU_CAperdose_AMMnetMIM_per.tiff", ppp, width = 10, height = 6)
  pppp <- plot_cohortsage(var = 'cases_per1000pop',
                          strat = 'catch-up',
                          seas = 'perennial')
  ggsave("plots/cohorts_CU_casesperpop_AMMnetMIM_per.tiff", pppp, width = 14, height = 8, dpi = 300)
  
  pp <- plot_cohortsage(var = 'cases_averted_perpop',
                        strat = 'AB',
                        seas = 'perennial')
  ggsave("plots/cohorts_AB_CAperpop_AMMnetMIM_per.tiff", pp, width = 14, height = 8, dpi = 300)
  # ppp <- plot_cohortsage(var = 'cases_averted_perdose',
  #                        strat = 'AB',
  #                        seas = 'perennial')
  # ggsave("plots/cohorts_AB_CAperdose_AMMnetMIM_per.tiff", ppp, width = 10, height = 6)
  pppp <- plot_cohortsage(var = 'cases_per1000pop',
                          strat = 'AB',
                          seas = 'perennial')
  ggsave("plots/cohorts_AB_casesperpop_AMMnetMIM_per.tiff", pppp, width = 14, height = 8, dpi = 300)
  
  
  # Plot cohorts by age at vaccination and age 
  ageatvaxhighlights <- c('0.5-1','3-4','7-8','12-13')
  
  plot_cohortsageatvax <- function(var,
                                   strat,
                                   seas){
    df <- cohorts_ageatvaxandage %>% 
      filter(as.numeric(age_lower) < 26, 
             PEVstrategy == strat,
             PEVage == '6m-14y',
             seasonality == seas,
             pfpr %in% c(0.05, 0.25, 0.45)) #%>%
      # mutate(ageatvax = factor(ageatvax, 
      #                          levels = c('0.5-1','0.5-1.5', '1.5-2.5', '2.5-3.5','2.5-3',
      #                                     '3.5-4.5', '4.5-5.5', '4.5-5','5-5.5',
      #                                     '5.5-6.5', '6.5-7.5', '7.5-8.5','8.5-9.5','9.5-10.5','9.5-10',
      #                                     '10.5-11.5', '11.5-12.5', '12.5-13.5', '13.5-14.5', '14.5-15')))#c('0.5-1','1-1.5','1.5-2','2-2.5','2.5-3','3-3.5','3.5-4','4-4.5',
                                          # '4.5-5','5-5.5','5.5-6','6-6.5','6.5-7','7-7.5','7.5-8','8-8.5',
                                          # '8.5-9','9-9.5','9.5-10','10-10.5','10.5-11','11-11.5','11.5-12',
                                          # '12-12.5','12.5-13','13-13.5','13.5-14','14-14.5','14.5-15','no vax')))

    pfpr.labs <- c("5%", "25%", '45%')
    names(pfpr.labs) <- c("0.05","0.25", "0.45")
    
    ggplot() + 
      geom_point(data = df %>%
                   filter(!(ageatvax %in% ageatvaxhighlights)),
                   # filter(ageatvax != '0.5-1.5'  &ageatvax != '3.5-4.5' &ageatvax != '7.5-8.5' &
                   #          ageatvax != '12.5-13.5'), 
                 mapping = aes(x = as.numeric(age_lower), 
                               y = .data[[var]],
                               group = ageatvax),
                 size = 2,
                 color = 'grey90') + 
      geom_line(data = df %>% 
                  filter(!(ageatvax %in% ageatvaxhighlights)),
                  # filter(ageatvax != '0.5-1.5'  &ageatvax != '3.5-4.5' &ageatvax != '7.5-8.5' &
                  #          ageatvax != '12.5-13.5'),
                aes(x = as.numeric(age_lower), 
                    y = .data[[var]],
                    group = ageatvax),
                color = 'grey90')+
      
      geom_point(data = df %>%
                   filter(ageatvax %in% ageatvaxhighlights),
                   # filter(ageatvax == '0.5-1.5'  |ageatvax == '3.5-4.5' |ageatvax == '7.5-8.5' |
                   #          ageatvax == '12.5-13.5'), 
                 aes(x = as.numeric(age_lower), 
                     y = .data[[var]], 
                     color = ageatvax,
                     group = ageatvax
                 ),
                 size = 2) +
      geom_line(data = df %>% 
                  filter(ageatvax %in% ageatvaxhighlights),
                  # filter(ageatvax == '0.5-1.5'  |ageatvax == '3.5-4.5' |ageatvax == '7.5-8.5' |
                  #          ageatvax == '12.5-13.5'),
                aes(x = as.numeric(age_lower), 
                    y = .data[[var]], 
                    color = ageatvax,
                    group = ageatvax) ,
                linewidth = 1) + 
      geom_hline(data = df,
                 aes(yintercept = 0),
                 color = 'darkred',
                 linetype = 3,
                 linewidth = 0.8) +
      facet_grid(rows = vars(pfpr),
                 scales = 'free_y',
                 labeller = labeller(pfpr = pfpr.labs)) +
      scale_color_manual(values = CUcols) +
      scale_fill_manual(values = CUcols) +
      scale_x_continuous(breaks = seq(0,25, by = 5))+
      theme_bw(base_size = 14) +#
      labs(x = 'Age',
           y = if(var == 'cases_averted_perpop'){
             'Cases averted per 1000 population'
             } else if(var == 'cases_averted'){
               'Cases averted'
             } else if(var == 'cases_per1000pop'){
               'Cases per 1000 population'
             } else if(var == 'sevcases_per1000pop'){
               'Severe cases per 1000 population'
             },
           color = 'Age at \nvaccination\n(years)',
           fill = 'Age at \nvaccination\n(years') + 
      theme_bw(base_size = 14) +
      theme(axis.title = element_text(size = 8),
            # plot.title = element_text(size = 22),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 8),
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
  
  
  # Function to get the overall cases averted per age group within cohorts (to go with Figure 2)
  plot_overall_CA_cohort <- function(seas){
    
    co <- cohorts_ageatvax %>%
      filter(seasonality == seas,
             pfpr %in% c(0.05, 0.25, 0.45),
             PEVage == '6m-14y',
             ageatvax!='14.5-15') %>%
      select(c(pfpr, seasonality, PEVage, PEVstrategy, EPIextra, 
               cases_averted_perpop_lower, cases_averted_perpop, cases_averted_perpop_upper)) %>%
      mutate(#ageatvax = factor(ageatvax, levels = 
                               #   c('0.5-1','0.5-1.5', '1.5-2.5', '2.5-3.5','2.5-3',
                               # '3.5-4.5', '4.5-5.5', '4.5-5','5-5.5',
                               # '5.5-6.5', '6.5-7.5', '7.5-8.5','8.5-9.5','9.5-10.5','9.5-10',
                               # '10.5-11.5', '11.5-12.5', '12.5-13.5', '13.5-14.5', '14.5-15')),
             # c('0.5-1', '1-1.5', '1.5-2', '2-2.5', '2.5-3', '3-3.5', '3.5-4', '4-4.5', '4.5-5', '5-5.5', '5.5-6', 
                                                    # '6-6.5', '6.5-7', '7-7.5', '7.5-8', '8-8.5', '8.5-9', '9-9.5', '9.5-10', '10-10.5', '10.5-11', '11-11.5', 
                                                    # '11.5-12', '12-12.5', '12.5-13', '13-13.5', '13.5-14', '14-14.5', '14.5-15')),
             ageatvaxhighlight = ifelse(ageatvax %in% ageatvaxhighlights, as.character(ageatvax), NA),
             ageatvaxhighlight = factor(ageatvaxhighlight, levels = ageatvaxhighlights))
    pfpr.labs <- c("5%", "25%", '45%')
    names(pfpr.labs) <- c("0.05","0.25", "0.45")
    
    totals <- ggplot(co) +
      geom_col(aes(x = ageatvax,
                   y = cases_averted_perpop,
                   fill = ageatvaxhighlight)) +
      geom_errorbar(aes(x = ageatvax,
                        ymin = cases_averted_perpop_lower,
                        ymax = cases_averted_perpop_upper, 
                        color = ageatvaxhighlight)) +
      facet_grid(rows = vars(pfpr),
                 labeller = labeller(pfpr = pfpr.labs)) + 
      scale_fill_manual(values = CUcols, na.value = 'grey80') +
      scale_color_manual(values = rep('black',4), na.value = 'grey70') +
    labs(x = 'Age at vaccination (years)',
         y = 'Cases averted per 1000 population',
         fill = '',
         color = '') + 
      theme_bw(base_size = 14) +
      theme_bw(base_size = 14) +
      theme(axis.title = element_text(size = 8),
            # plot.title = element_text(size = 22),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 8),
            plot.caption = element_text(size = 10),
            legend.key.size = unit(0.3, 'cm'),
            axis.text.x = element_text(size = 6),
            axis.text.y = element_text(angle = 90, size = 8),
            strip.text = element_text(size = 8),
            plot.margin = margin(t = 2,  # Top margin
                                 r = 2,  # Right margin
                                 b = 2,  # Bottom margin
                                 l = 2),
            legend.position = 'none')
    write.csv(co, paste0('plots/cohorts_sum_ageatvax', seas,'.csv'))
    return(totals)
  }
  
  p <- plot_cohortsageatvax(var = 'cases_averted_perpop',
                            strat = 'catch-up',
                            seas = 'perennial')
  ggsave("plots/plot_cohorts_ageatvax_CAperpop_per.tiff", p, height = 5, width = 8.75, dpi = 500)
  totals_per <- plot_overall_CA_cohort(seas = 'perennial')
  # put cases averted per 1000 by age and age at vax next to totals plot 
  ppp <- cowplot::plot_grid(p, totals_per, rel_widths = c(1, 0.8), labels = 'AUTO')
  ggsave("plots/plot_cohorts_ageatvax_CAperpoptotals_per.tiff", ppp, height = 5, width = 8.75, dpi = 500)
  
  
  p2 <- plot_cohortsageatvax(var = 'cases_averted',
                             strat = 'catch-up',
                             seas = 'perennial')
  ggsave("plots/plot_cohorts_ageatvax_CA_per.tiff", p2, height = 5, width = 8.75, dpi = 500)
  
  p3 <- plot_cohortsageatvax(var = 'cases_per1000pop',
                       strat = 'catch-up',
                       seas = 'perennial')
  ggsave("plots/plot_cohorts_ageatvax_cases_per.tiff", p3, height = 5, width = 8.75, dpi = 500)
  
  p4 <- plot_cohortsageatvax(var = 'sevcases_per1000pop',
                       strat = 'catch-up',
                       seas = 'perennial')
  ggsave("plots/plot_cohorts_ageatvax_sevcases_per.tiff", p4, height = 5, width = 8.75, dpi = 500)
  
  
  
  p <- plot_cohortsageatvax(var = 'cases_averted_perpop',
                            strat = 'catch-up',
                            seas = 'seasonal')
  ggsave("plots/plot_cohorts_ageatvax_CAperpop_seas.tiff", p, height = 5, width = 8.75, dpi = 500)
  
  totals_seas <- plot_overall_CA_cohort(seas = 'seasonal')
  # put cases averted per 1000 by age and age at vax next to totals plot 
  ppp <- cowplot::plot_grid(p, totals_seas, rel_widths = c(1, 0.75), labels = 'AUTO')
  ggsave("plots/plot_cohorts_ageatvax_CAperpoptotals_seas.tiff", ppp, height = 8, width = 14, dpi = 300)
  
  
  
  p2 <- plot_cohortsageatvax(var = 'cases_averted',
                             strat = 'catch-up',
                             seas = 'seasonal')
  ggsave("plots/plot_cohorts_ageatvax_CA_seas.tiff", p2, height = 5, width = 8.75, dpi = 500)
  
  p3 <- plot_cohortsageatvax(var = 'cases_per1000pop',
                             strat = 'catch-up',
                             seas = 'seasonal')
  ggsave("plots/plot_cohorts_ageatvax_cases_seas.tiff", p3, height = 5, width = 8.75, dpi = 500)
  
  p4 <- plot_cohortsageatvax(var = 'sevcases_per1000pop',
                             strat = 'catch-up',
                             seas = 'seasonal')
  ggsave("plots/plot_cohorts_ageatvax_sevcases_seas.tiff", p4, height = 5, width = 8.75, dpi = 500)
} 
