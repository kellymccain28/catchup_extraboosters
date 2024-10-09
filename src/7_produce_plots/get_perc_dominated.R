# SCript to output the percent of non-dominated scenarios that are ...


# Get % CU in non-dominated scenarios 
perc_dominant_maxCA <- df_summ %>%
  filter(age_grp == '0-100',
         maxCA == 1) %>%
  select(int_ID, pfpr, seasonality, PEV, PEVstrategy, PEVcov, PEVage, EPIextra, labels, scen_labels, EPIextra_labels, strategytype,
         starts_with('min'), starts_with('max')) %>%
  group_by(pfpr, seasonality) %>%
  summarize(n_CA = n(),
            p_CA_CU = round(sum(strategytype == 'Catch-up') / n_CA * 100, 1),
            p_CA_AB = 100 - p_CA_CU)

perc_dominant_maxSA <- df_summ %>%
  filter(age_grp == '0-100',
         maxSA == 1) %>%
  select(int_ID, pfpr, seasonality, PEV, PEVstrategy, PEVcov, PEVage, EPIextra, labels, scen_labels, EPIextra_labels, strategytype,
         starts_with('min'), starts_with('max')) %>%
  group_by(pfpr, seasonality) %>%
  summarize(n_SA = n(),
            p_SA_CU = round(sum(strategytype == 'Catch-up') / n_SA * 100, 1),
            p_SA_AB = 100 - p_SA_CU)

perc_dominant <- left_join(perc_dominant_maxCA, perc_dominant_maxSA)

write.csv(perc_dominant, 'plots/perc_dominant_bypfprseas.csv')


perc_dominant_CA_overall <- df_summ %>%
  filter(age_grp == '0-100',
         maxCA == 1) %>%
  select(int_ID, pfpr, seasonality, PEV, PEVstrategy, PEVcov, PEVage, EPIextra, labels, scen_labels, EPIextra_labels, strategytype,
         starts_with('min'), starts_with('max')) %>%
  summarize(n_CA = n(),
            p_CA_CU = round(sum(strategytype == 'Catch-up') / n_CA * 100, 2),
            p_CA_AB = 100 - p_CA_CU,
            p_CA_booster = round(sum(EPIextra !='-') / n_CA * 100, 2),
            p_CA_CU_booster = round(sum(EPIextra !='-' | strategytype == 'Catch-up') / n_CA * 100, 2))

perc_dominant_SA_overall <- df_summ %>%
  filter(age_grp == '0-100',
         maxSA == 1) %>%
  select(int_ID, pfpr, seasonality, PEV, PEVstrategy, PEVcov, PEVage, EPIextra, labels, scen_labels, EPIextra_labels, strategytype,
         starts_with('min'), starts_with('max')) %>%
  summarize(n_SA = n(),
            p_SA_CU = round(sum(strategytype == 'Catch-up') / n_SA * 100, 2),
            p_SA_AB = 100 - p_SA_CU,
            p_SA_booster = round(sum(EPIextra !='-') / n_SA * 100, 2),
            p_SA_CU_booster = round(sum(EPIextra !='-' | strategytype == 'Catch-up') / n_SA * 100, 2))
perc_dominant_overall <- cbind(perc_dominant_CA_overall, perc_dominant_SA_overall)
perc_dominant_overall
write.csv(perc_dominant_overall, 'plots/perc_dominant.csv')

