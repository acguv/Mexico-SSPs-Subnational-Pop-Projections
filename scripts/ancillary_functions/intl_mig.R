
# Derive domestic (rest of NYC) in and out migration to NYC burrows
derive_intl_mig <- function(cur_inputs_path, cur_year, pop_df,
                            int_mig, cur_scenario)
{

  # Scenario data (The Constant_rate scenario)
  scenario_df  <- file.path(cur_inputs_path, paste0(cur_scenario, ".csv")) %>%
    fread() %>%
    .[!is.na(year)]
  

  
  # International migration data (proportions per age group)
  intl_in_mig_df  <- file.path(cur_inputs_path, 'int_in_mig_ent.csv') %>%
    fread() %>%
    .[, Age := as.numeric(str_sub(Age, 1, 3))] 
  
  
  
  # International out migration data (proportions per age group)
  intl_out_mig_df  <- file.path(cur_inputs_path, 'int_out_mig_ent.csv') %>%
    fread() %>%
    .[, Age := as.numeric(str_sub(Age, 1, 3))] 
  
  
  
  # Annual total net international migration counts 
  # International in migrants
  female_in_intl <- as.numeric(scenario_df[year == cur_year, 'iim_F']) # Female
  male_in_intl   <- as.numeric(scenario_df[year == cur_year, 'iim_M']) # Male
  
  
  
  # International out migrants
  female_out_intl <- as.numeric(scenario_df[year == cur_year, 'iom_F']) # Female
  male_out_intl   <- as.numeric(scenario_df[year == cur_year, 'iom_M']) # Male
  
  
  
  
  # Spread international migrant numbers according to the age profile
  if (int_mig == 1){
    # In migration
    intl_in_mig_df[Sex == 'Female',
                   in_intl_mig := International.migration * female_in_intl] 

    intl_in_mig_df[Sex == 'Male',
                   in_intl_mig := International.migration * male_in_intl]
    
    
    
    # Out migration
    intl_out_mig_df[Sex == 'Female',
                    out_intl_mig := International.migration * female_out_intl] 
    
    intl_out_mig_df[Sex == 'Male',
                    out_intl_mig := International.migration * male_out_intl]
 
    
  } else {
    
    intl_in_mig_df[, in_intl_mig := 0] 
    intl_out_mig_df[, out_intl_mig := 0] 
    
  }
  

  intl_mig_df <- intl_in_mig_df[intl_out_mig_df, .(Race, Sex, Age, in_intl_mig,
                                                   out_intl_mig,
                                                   net_intl_mig = in_intl_mig - out_intl_mig),
                                on = c('Sex', 'Age')]
    
  return(intl_mig_df)
  
}
  