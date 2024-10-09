# Function to add dose variables to run

add_doses <- function(df){
  PEVstrategy <- df$PEVstrategy[1]
  massbooster_rep <- df$massbooster_rep[1]
  EPIextra <- df$EPIextra[1]
  
  # Add in doses 
  if(PEVstrategy=='none'){
    df <- df |> rowwise() |>
      mutate(dose1 = 0,
             dose2 = 0,
             dose3 = 0,
             dose4 = 0,
             dose5 = 0,
             dose6 = 0, 
             dose7 = 0, 
             dose8ormore = 0,
             n_pev_epi_dose_1 = 0,
             n_pev_epi_dose_2 = 0, 
             n_pev_epi_dose_3 = 0, 
             n_pev_epi_booster_1 = 0,
             n_pev_epi_booster_2 = 0, 
             n_pev_epi_booster_3 = 0,
             n_pev_epi_booster_4 = 0, 
             n_pev_mass_dose_1 = 0,
             n_pev_mass_dose_2 = 0,
             n_pev_mass_dose_3 = 0,
             n_pev_mass_booster_1 = 0) |> ungroup()
  }
  
  if((PEVstrategy=='AB' & EPIextra == '-') | PEVstrategy == 'hybrid'){
    df <- df |> rowwise() |>
      mutate(dose1 = n_pev_epi_dose_1,
             dose2 = n_pev_epi_dose_2,
             dose3 = n_pev_epi_dose_3,
             dose4 = n_pev_epi_booster_1,
             dose5 = 0,
             dose6 = 0, 
             dose7 = 0, 
             dose8ormore = 0,
             n_pev_epi_dose_1 = n_pev_epi_dose_1,
             n_pev_epi_dose_2 = n_pev_epi_dose_2, 
             n_pev_epi_dose_3 = n_pev_epi_dose_3, 
             n_pev_epi_booster_1 = n_pev_epi_booster_1,
             n_pev_epi_booster_2 = 0, 
             n_pev_epi_booster_3 = 0,
             n_pev_epi_booster_4 = 0, 
             n_pev_mass_dose_1 = 0,
             n_pev_mass_dose_2 = 0,
             n_pev_mass_dose_3 = 0,
             n_pev_mass_booster_1 = 0) |> ungroup()
  }
  
  if(PEVstrategy == 'SV'){
    df <- df |> rowwise() |>
      mutate(dose1 = n_pev_mass_dose_1,
             dose2 = n_pev_mass_dose_2,
             dose3 = n_pev_mass_dose_3,
             dose4 = n_pev_mass_booster_1,
             dose5 = 0,
             dose6 = 0, 
             dose7 = 0, 
             dose8ormore = 0,
             n_pev_epi_dose_1 = 0,
             n_pev_epi_dose_2 = 0, 
             n_pev_epi_dose_3 = 0, 
             n_pev_epi_booster_1 = 0,
             n_pev_epi_booster_2 = 0, 
             n_pev_epi_booster_3 = 0,
             n_pev_epi_booster_4 = 0, 
             n_pev_mass_dose_1 = n_pev_mass_dose_1,
             n_pev_mass_dose_2 = n_pev_mass_dose_2,
             n_pev_mass_dose_3 = n_pev_mass_dose_3,
             n_pev_mass_booster_1 = n_pev_mass_booster_1) |> ungroup()
  }
  
  if(PEVstrategy=='mass' & (massbooster_rep == '4 annual')){
    df <- df |> rowwise() |>
      mutate(dose1 = n_pev_epi_dose_1 + n_pev_mass_dose_1,
             dose2 = n_pev_epi_dose_2 + n_pev_mass_dose_2,
             dose3 = n_pev_epi_dose_3 + n_pev_mass_dose_3,
             dose4 = n_pev_epi_booster_1 + n_pev_mass_booster_1,
             dose5 = 0 + n_pev_mass_booster_2,
             dose6 = 0 + n_pev_mass_booster_3,
             dose7 = 0 + n_pev_mass_booster_4, 
             dose8ormore = 0,
             n_pev_epi_dose_1 = n_pev_epi_dose_1,
             n_pev_epi_dose_2 = n_pev_epi_dose_2, 
             n_pev_epi_dose_3 = n_pev_epi_dose_3, 
             n_pev_epi_booster_1 = n_pev_epi_booster_1,
             n_pev_epi_booster_2 = n_pev_epi_booster_2, 
             n_pev_epi_booster_3 = n_pev_epi_booster_3,
             n_pev_epi_booster_4 = 0, 
             n_pev_mass_dose_1 = n_pev_mass_dose_1,
             n_pev_mass_dose_2 = n_pev_mass_dose_2,
             n_pev_mass_dose_3 = n_pev_mass_dose_3,
             n_pev_mass_booster_1 = n_pev_mass_booster_1) |> ungroup()
  }
  
  ######################################################################
  # WILL NEED TO UPDATE THIS FOR MASS VACCINATION 
  ######################################################################
  if(PEVstrategy=='mass' & (massbooster_rep == 'annual')){
    df <- df |> rowwise() |>
      mutate(dose1 = n_pev_epi_dose_1 + n_pev_mass_dose_1,
             dose2 = n_pev_epi_dose_2 + n_pev_mass_dose_2,
             dose3 = n_pev_epi_dose_3 + n_pev_mass_dose_3,
             dose4 = n_pev_epi_booster_1 + n_pev_mass_booster_1,
             dose5 = 0 + n_pev_mass_booster_2,
             dose6 = 0 + n_pev_mass_booster_3,
             dose7 = 0 + n_pev_mass_booster_4,
             dose8ormore = n_pev_mass_booster_5 + n_pev_mass_booster_6 + n_pev_mass_booster_7 + n_pev_mass_booster_8 + n_pev_mass_booster_9 +
               n_pev_mass_booster_10 + n_pev_mass_booster_11 + n_pev_mass_booster_12 + n_pev_mass_booster_13 + n_pev_mass_booster_14 +
               n_pev_mass_booster_15 + n_pev_mass_booster_16,
             n_pev_epi_dose_1 = n_pev_epi_dose_1,
             n_pev_epi_dose_2 = n_pev_epi_dose_2, 
             n_pev_epi_dose_3 = n_pev_epi_dose_3, 
             n_pev_epi_booster_1 = n_pev_epi_booster_1,
             n_pev_epi_booster_2 = n_pev_epi_booster_2, 
             n_pev_epi_booster_3 = n_pev_epi_booster_3,
             n_pev_epi_booster_4 = 0, 
             n_pev_mass_dose_1 = n_pev_mass_dose_1,
             n_pev_mass_dose_2 = n_pev_mass_dose_2,
             n_pev_mass_dose_3 = n_pev_mass_dose_3,
             n_pev_mass_booster_1 = n_pev_mass_booster_1) |> ungroup()
  }
  
  if(PEVstrategy=='mass' & massbooster_rep == "-"){
    df <- df |> rowwise() |>
      mutate(dose1 = n_pev_epi_dose_1 + n_pev_mass_dose_1,
             dose2 = n_pev_epi_dose_2 + n_pev_mass_dose_2,
             dose3 = n_pev_epi_dose_3 + n_pev_mass_dose_3,
             dose4 = n_pev_epi_booster_1 + n_pev_mass_booster_1,
             dose5 = 0,
             dose6 = 0, 
             dose7 = 0, 
             dose8ormore = 0,
             n_pev_epi_dose_1 = n_pev_epi_dose_1,
             n_pev_epi_dose_2 = n_pev_epi_dose_2, 
             n_pev_epi_dose_3 = n_pev_epi_dose_3, 
             n_pev_epi_booster_1 = n_pev_epi_booster_1,
             n_pev_epi_booster_2 = 0, 
             n_pev_epi_booster_3 = 0,
             n_pev_epi_booster_4 = 0, 
             n_pev_mass_dose_1 = n_pev_mass_dose_1,
             n_pev_mass_dose_2 = n_pev_mass_dose_2,
             n_pev_mass_dose_3 = n_pev_mass_dose_3,
             n_pev_mass_booster_1 = n_pev_mass_booster_1) |> ungroup()
  }
  
  if(PEVstrategy=='catch-up' & EPIextra == "-"){
    df <- df |> rowwise() |>
      mutate(dose1 = n_pev_epi_dose_1 + n_pev_mass_dose_1,
             dose2 = n_pev_epi_dose_2 + n_pev_mass_dose_2,
             dose3 = n_pev_epi_dose_3 + n_pev_mass_dose_3,
             dose4 = n_pev_epi_booster_1 + n_pev_mass_booster_1,
             dose5 = 0,
             dose6 = 0, 
             dose7 = 0, 
             dose8ormore = 0,
             n_pev_epi_dose_1 = n_pev_epi_dose_1,
             n_pev_epi_dose_2 = n_pev_epi_dose_2, 
             n_pev_epi_dose_3 = n_pev_epi_dose_3, 
             n_pev_epi_booster_1 = n_pev_epi_booster_1,
             n_pev_epi_booster_2 = 0, 
             n_pev_epi_booster_3 = 0,
             n_pev_epi_booster_4 = 0, 
             n_pev_mass_dose_1 = n_pev_mass_dose_1,
             n_pev_mass_dose_2 = n_pev_mass_dose_2,
             n_pev_mass_dose_3 = n_pev_mass_dose_3,
             n_pev_mass_booster_1 = n_pev_mass_booster_1) |> ungroup()
  }
  
  if(PEVstrategy=='catch-up' & (EPIextra == '10y' | EPIextra == '5y' | EPIextra == '2y')){
    df <- df |> rowwise() |>
      mutate(dose1 = n_pev_epi_dose_1 + n_pev_mass_dose_1,
             dose2 = n_pev_epi_dose_2 + n_pev_mass_dose_2,
             dose3 = n_pev_epi_dose_3 + n_pev_mass_dose_3,
             dose4 = n_pev_epi_booster_1 + n_pev_mass_booster_1,
             dose5 = n_pev_epi_booster_2,
             dose6 = 0, 
             dose7 = 0, 
             dose8ormore = 0,
             n_pev_epi_dose_1 = n_pev_epi_dose_1,
             n_pev_epi_dose_2 = n_pev_epi_dose_2, 
             n_pev_epi_dose_3 = n_pev_epi_dose_3, 
             n_pev_epi_booster_1 = n_pev_epi_booster_1,
             n_pev_epi_booster_2 = n_pev_epi_booster_2, 
             n_pev_epi_booster_3 = 0,
             n_pev_epi_booster_4 = 0, 
             n_pev_mass_dose_1 = n_pev_mass_dose_1,
             n_pev_mass_dose_2 = n_pev_mass_dose_2,
             n_pev_mass_dose_3 = n_pev_mass_dose_3,
             n_pev_mass_booster_1 = n_pev_mass_booster_1) |> ungroup()
  }
  
  if(PEVstrategy=='catch-up' & EPIextra %in% c('5y+10y', '2y+5y','2y+10y')){
    df <- df |> rowwise() |>
      mutate(dose1 = n_pev_epi_dose_1 + n_pev_mass_dose_1,
             dose2 = n_pev_epi_dose_2 + n_pev_mass_dose_2,
             dose3 = n_pev_epi_dose_3 + n_pev_mass_dose_3,
             dose4 = n_pev_epi_booster_1 + n_pev_mass_booster_1,
             dose5 = n_pev_epi_booster_2,
             dose6 = n_pev_epi_booster_3, 
             dose7 = 0, 
             dose8ormore = 0,
             n_pev_epi_dose_1 = n_pev_epi_dose_1,
             n_pev_epi_dose_2 = n_pev_epi_dose_2, 
             n_pev_epi_dose_3 = n_pev_epi_dose_3, 
             n_pev_epi_booster_1 = n_pev_epi_booster_1,
             n_pev_epi_booster_2 = n_pev_epi_booster_2, 
             n_pev_epi_booster_3 = n_pev_epi_booster_3,
             n_pev_epi_booster_4 = 0, 
             n_pev_mass_dose_1 = n_pev_mass_dose_1,
             n_pev_mass_dose_2 = n_pev_mass_dose_2,
             n_pev_mass_dose_3 = n_pev_mass_dose_3,
             n_pev_mass_booster_1 = n_pev_mass_booster_1) |> ungroup()
  }
  
  if(PEVstrategy == 'AB' & (EPIextra == '10y' | EPIextra == '5y' | EPIextra == '2y')){
    df <- df |> rowwise() |>
      mutate(dose1 = n_pev_epi_dose_1,
             dose2 = n_pev_epi_dose_2,
             dose3 = n_pev_epi_dose_3,
             dose4 = n_pev_epi_booster_1,
             dose5 = n_pev_epi_booster_2,
             dose6 = 0, 
             dose7 = 0, 
             dose8ormore = 0,
             n_pev_epi_dose_1 = n_pev_epi_dose_1,
             n_pev_epi_dose_2 = n_pev_epi_dose_2, 
             n_pev_epi_dose_3 = n_pev_epi_dose_3, 
             n_pev_epi_booster_1 = n_pev_epi_booster_1,
             n_pev_epi_booster_2 = n_pev_epi_booster_2, 
             n_pev_epi_booster_3 = 0,
             n_pev_epi_booster_4 = 0, 
             n_pev_mass_dose_1 = 0,
             n_pev_mass_dose_2 = 0,
             n_pev_mass_dose_3 = 0,
             n_pev_mass_booster_1 = 0) |> ungroup()
  }
  
  if(PEVstrategy == 'AB' & EPIextra %in% c('5y+10y', '2y+5y','2y+10y')){
    df <- df |> rowwise() |>
      mutate(dose1 = n_pev_epi_dose_1,
             dose2 = n_pev_epi_dose_2,
             dose3 = n_pev_epi_dose_3,
             dose4 = n_pev_epi_booster_1,
             dose5 = n_pev_epi_booster_2,
             dose6 = n_pev_epi_booster_3, 
             dose7 = 0, 
             dose8ormore = 0,
             n_pev_epi_dose_1 = n_pev_epi_dose_1,
             n_pev_epi_dose_2 = n_pev_epi_dose_2, 
             n_pev_epi_dose_3 = n_pev_epi_dose_3, 
             n_pev_epi_booster_1 = n_pev_epi_booster_1,
             n_pev_epi_booster_2 = n_pev_epi_booster_2, 
             n_pev_epi_booster_3 = n_pev_epi_booster_3,
             n_pev_epi_booster_4 = 0, 
             n_pev_mass_dose_1 = 0,
             n_pev_mass_dose_2 = 0,
             n_pev_mass_dose_3 = 0,
             n_pev_mass_booster_1 = 0) |> ungroup()
  }
  
  if(PEVstrategy == 'AB' & EPIextra == '2y+5y+10y'){
    df <- df |> rowwise() |>
      mutate(dose1 = n_pev_epi_dose_1,
             dose2 = n_pev_epi_dose_2,
             dose3 = n_pev_epi_dose_3,
             dose4 = n_pev_epi_booster_1,
             dose5 = n_pev_epi_booster_2,
             dose6 = n_pev_epi_booster_3, 
             dose7 = n_pev_epi_booster_4, 
             dose8ormore = 0,
             n_pev_epi_dose_1 = n_pev_epi_dose_1,
             n_pev_epi_dose_2 = n_pev_epi_dose_2, 
             n_pev_epi_dose_3 = n_pev_epi_dose_3, 
             n_pev_epi_booster_1 = n_pev_epi_booster_1,
             n_pev_epi_booster_2 = n_pev_epi_booster_2, 
             n_pev_epi_booster_3 = n_pev_epi_booster_3,
             n_pev_epi_booster_4 = n_pev_epi_booster_4,
             n_pev_mass_dose_1 = 0,
             n_pev_mass_dose_2 = 0,
             n_pev_mass_dose_3 = 0,
             n_pev_mass_booster_1 = 0) |> ungroup()
  }
  
  if(PEVstrategy=='catch-up' & EPIextra == '2y+5y+10y'){
    df <- df |> rowwise() |>
      mutate(dose1 = n_pev_epi_dose_1 + n_pev_mass_dose_1,
             dose2 = n_pev_epi_dose_2 + n_pev_mass_dose_2,
             dose3 = n_pev_epi_dose_3 + n_pev_mass_dose_3,
             dose4 = n_pev_epi_booster_1 + n_pev_mass_booster_1,
             dose5 = n_pev_epi_booster_2,
             dose6 = n_pev_epi_booster_3, 
             dose7 = n_pev_epi_booster_4, 
             dose8ormore = 0,
             n_pev_epi_dose_1 = n_pev_epi_dose_1,
             n_pev_epi_dose_2 = n_pev_epi_dose_2, 
             n_pev_epi_dose_3 = n_pev_epi_dose_3, 
             n_pev_epi_booster_1 = n_pev_epi_booster_1,
             n_pev_epi_booster_2 = n_pev_epi_booster_2, 
             n_pev_epi_booster_3 = n_pev_epi_booster_3,
             n_pev_epi_booster_4 = n_pev_epi_booster_4, 
             n_pev_mass_dose_1 = n_pev_mass_dose_1,
             n_pev_mass_dose_2 = n_pev_mass_dose_2,
             n_pev_mass_dose_3 = n_pev_mass_dose_3,
             n_pev_mass_booster_1 = n_pev_mass_booster_1) |> ungroup()
  }
  
  return(df)
}
