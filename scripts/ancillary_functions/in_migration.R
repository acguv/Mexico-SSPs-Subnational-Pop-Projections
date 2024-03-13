
derive_int_in_migration <- function(inputs_path, cur_year, pop_df, cur_scenario, int_mig_factor){
  
  # A data frame to hold internal in-migration values for region pairs
  pairs_in_int_mig_df <- data.table()
  
  
  # A data frame to hold internal in-migration values across all regions
  tot_in_int_mig_df <- data.table()
  
  
  # Retrieve regions
  regions <- unique(pop_df[, Region])
  
  
  for (region in regions){
    
    # Read the csv file holding in migration rates to the current state
    cur_int_mig_rates_df <- file.path(inputs_path, region, 'internal_in_migration_ent.csv') %>%
      fread() %>%
      .[, rate := ifelse(is.na(Internal.migration.rate), 0,
                         Internal.migration.rate)] %>%
      .[, Age := as.numeric(str_sub(Age, 1, 3))] %>%
      .[, Sex := ifelse(Sex == 'f', 'Female', 'Male')]

    
    
    # Retrieve the internal migration factor consistent with the scenario
    scenario_df <- file.path(inputs_path, region, paste0(cur_scenario, ".csv")) %>%
      fread()
    
    
    female_int_mig_factor <- scenario_df %>%
      .[year == cur_year, idm_F] %>%
      as.numeric()
    
    
    male_int_mig_factor <- scenario_df %>%
      .[year == cur_year, idm_M] %>%
      as.numeric()

    
    
    # Retrieve population of all origin regions 
    cur_origin_pop_df <- pop_df[Region != region,] 
    
    
    
    # Apply the scenario factor to the migration table
    cur_int_mig_rates_df[Sex == 'Female', rate := rate * female_int_mig_factor]
    cur_int_mig_rates_df[Sex == 'Male', rate := rate * male_int_mig_factor]
    
    
    
    # Multiplication gives us the in migration numbers from each origin state
    # cur_int_mig_values_df <- cur_origin_pop_df[cur_int_mig_rates_df, 
    #                                          .(Region, Destination, Race, Sex,
    #                                            Age, Population, rate,
    #                                            int_in_mig = Population * rate),
    #                                            on = c('Region' = 'Origin', 
    #                                                   'Sex', 'Age', 'Race')]
    
    cur_int_mig_values_df <- cur_origin_pop_df[cur_int_mig_rates_df, 
                                               .(Region, Destination, Race, Sex,
                                                 Age, Population, rate,
                                                 int_in_mig = Population * rate),
                                               on = c('Region' = 'Origin', 
                                                      'Sex', 'Age', 'Race')]
     
    
    # Sum the total in migration numbers across all contributing regions
    cur_tot_int_mig_values_df <- cur_int_mig_values_df[, .(int_in_mig = sum(int_in_mig)*int_mig_factor),
                                                      by = .(Destination, Race, Sex, Age)]
    
    
    pairs_in_int_mig_df <- rbind(pairs_in_int_mig_df, cur_int_mig_values_df) 
    tot_in_int_mig_df   <- rbind(tot_in_int_mig_df, cur_tot_int_mig_values_df)
  }
  
  
  output_list <- list(region_pair_inmigration = pairs_in_int_mig_df,
                      total_inmigration       = tot_in_int_mig_df)
  
  return(output_list)
}
