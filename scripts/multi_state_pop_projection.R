`
# Header ------------------------------------------------------------------

# Author:     
# Date:       2023-10-01
# Purpose:    Multi-state population projection for NYC.

rm(list = ls())


# Set options -------------------------------------------------------------

options("scipen"=100, "digits"=4) # to force R not to use scientific notations
options(readr.show_progress = F)




# Load packages -----------------------------------------------------------

if (!requireNamespace('tidyverse', quietly = T))   install.packages('tidyverse')
if (!requireNamespace('data.table', quietly = T))  install.packages('data.table')


library(tidyverse)
library(data.table)


# Load the ancillary functions
ancillary_functions <- file.path('scripts', 'ancillary_functions')

for (cur_file in list.files(ancillary_functions, full.names = T)) source(cur_file)




# Inputs and Paths --------------------------------------------------------

# Path to state-level inputs folder
main_region  <- 'Mexico'
inputs_path  <- file.path('inputs') 
outputs_path <- file.path('outputs')


# UN standard life table e0=30; used for linear interpolation of lx values
un_mortality_30_path <- file.path(inputs_path, 'AllRegions_mortality_UNe030.csv') 


# UN standard life table e0=100; used for linear interpolation of lx values
un_mortality_100_path <- file.path(inputs_path, 'AllRegions_mortality_UNe0100.csv')


# Specify regions
regions <- list.dirs(inputs_path, recursive = F, full.names = F)
races   <- c('Total') 


# Specify scenario
cur_scenario <- 'SSP5_ent'


# Enter time and age related parameters
start_year      <- 2020               
end_year        <- 2100                
steps           <- end_year - start_year 
num_ages        <- 101
intl_mig_factor <- 1
int_mig_factor  <- 0


# Path to results folder 
results_path <- file.path(outputs_path, cur_scenario,"Zero_Dom_Mig")
# results_path <- file.path(outputs_path, cur_scenario)

if (!dir.exists(results_path)) {
  
  dir.create(results_path)
} else {
  
  print('The results folder already exists!')
}




# Main program ------------------------------------------------------------

# A comprehensive data table to contain population summaries for all years
total_summary_df <- data.table(Year = rep(start_year:end_year, each = length(races)), 
                               Race = rep(races, time = length(start_year:end_year)),
                               in_migration = 0, 
                               int_migration = 0, 
                               Births = 0, Deaths = 0,
                               Population = 0)

total_summary_df <- rbindlist(replicate(length(regions), total_summary_df, 
                                        simplify = FALSE))

total_summary_df <- data.table(Region = rep(regions, 
                                            each = length(races) * 
                                              length(start_year:end_year)),
                               total_summary_df)



# A data table to hold base year population values before applying migration
tot_base_pop_df <- data.table()


# A data frame to hold population values updated by domestic and international migration
tot_base_upd_pop_df <- data.table()


# A data frame to hold international migration outcomes
tot_intl_mig_df <- data.table()


# A data frame to hold internal migration outcomes
total_in_mig_df <- data.table()


# Data frames to store internal migration between states
total_state_in_mig_df  <- data.table()
total_state_out_mig_df <- data.table()


# Update states base year population with international and state-level migrations
for (region in regions){
  
  cur_inputs_path <- file.path(inputs_path, region) 
  
  
  # Base Population data
  base_pop_df  <- file.path(cur_inputs_path, 'pop_ent.csv') %>%
    fread() %>%
    .[, Age := as.numeric(str_sub(Age, 1, 3))]
  
  
  
  # Derive international migration values
  intl_mig_df <- derive_intl_mig(cur_inputs_path, start_year, 
                                 base_pop_df, intl_mig_factor,
                                 cur_scenario)
  
  intl_mig_df     <- data.table(Year = start_year, Region = region, intl_mig_df)
  tot_intl_mig_df <- rbind(tot_intl_mig_df, intl_mig_df)
  
  
  # Generate a summary of domestic migration 
  sum_intl_mig_df <- intl_mig_df[order(Race), .(net_intl_mig = sum(net_intl_mig)),
                                 by = 'Race']
  
  
  
  # Add domestic and international migration summaries to the population summary file
  total_summary_df[Region == region & Year == start_year, 
                   `:=`(int_migration = sum_intl_mig_df[, net_intl_mig])]
  
  
  
  # Update initial population data frame with international migration values
  base_pop_int_mig_df <- base_pop_df[intl_mig_df, 
                                     .(Race, Sex, Age, 
                                       Population = Population + net_intl_mig),
                                     on = c('Race', 'Sex', 'Age')]
  
  base_pop_int_mig_df <- data.table(Region = region, base_pop_int_mig_df)
  
  
  
  base_pop_df         <- data.table(Region = region, base_pop_df)
  tot_base_pop_df     <- rbind(tot_base_pop_df, base_pop_df)
  tot_base_upd_pop_df <- rbind(tot_base_upd_pop_df, base_pop_int_mig_df)
}



# Calculate the total in out and net migration numbers for all states
internal_in_migration_df  <- derive_int_in_migration(inputs_path, start_year, 
                                                     tot_base_upd_pop_df, cur_scenario, int_mig_factor)

internal_out_migration_df <- derive_int_out_migration(inputs_path, start_year, 
                                                      tot_base_upd_pop_df, cur_scenario, int_mig_factor)


# People entered a state minus those who left
in_migration_df  <- internal_in_migration_df$total_inmigration
out_migration_df <- internal_out_migration_df$total_outmigration
net_migration_df <- in_migration_df[out_migration_df, .(Region, Race, Sex, Age,
                                                        int_in_mig, int_out_mig,
                                                        int_net_mig = int_in_mig - int_out_mig),
                                    on = c('Destination' = 'Region',
                                           'Race', 'Sex', 'Age')]

net_migration_df <- data.table(Year = start_year, net_migration_df)
total_in_mig_df  <- rbind(total_in_mig_df, net_migration_df) 



# Store pair-wise internal migration numbers 
pair_in_mig_df <- data.table(Year = start_year, 
                             internal_in_migration_df$region_pair_inmigration)

pair_out_mig_df <- data.table(Year = start_year, 
                             internal_out_migration_df$region_pair_outmigration)

total_state_in_mig_df  <- rbind(total_state_in_mig_df, pair_in_mig_df)
total_state_out_mig_df <- rbind(total_state_out_mig_df, pair_out_mig_df) 



# Generate a summary of internal migration 
sum_int_net_mig_df <- net_migration_df[order(Region, Race), 
                                       .(int_net_mig = sum(int_net_mig)), 
                                       by = c('Region', 'Race')]

total_summary_df[Year == start_year, in_migration := sum_int_net_mig_df[, int_net_mig]]



# Generate a summary of population 
sum_tot_pop_df <- tot_base_pop_df[order(Region, Race),
                                  .(Population = sum(Population)), 
                                  by = c('Region', 'Race')]

total_summary_df[Year == start_year, Population := sum_tot_pop_df[, Population]]



# Apply internal migration to base year population
tot_base_upd_pop_df <- tot_base_upd_pop_df[net_migration_df, 
                                           .(Region, Race, Sex, Age,
                                             Population = Population + int_net_mig),
                                           on = c('Region', 'Race', 'Sex', 'Age')]

tot_base_upd_pop_df <- data.table(Year = start_year, tot_base_upd_pop_df)




# Fertility ---------------------------------------------------------------

fertility_df <- estimate_fertility(regions, inputs_path, cur_scenario, start_year,
                                   end_year)




# Mortality ---------------------------------------------------------------

mortality_df <- estimate_mortality(regions, inputs_path, cur_scenario, start_year, end_year,
                                   un_mortality_30_path, un_mortality_100_path)




# Population Projection ---------------------------------------------------

tot_proj_pop_df <- NULL
tot_upd_pop     <- NULL 

# The outer loop that goes through years
for (year in (start_year + 1):(end_year + 1)){
  
  cat(paste("\nCurrent Year is", year, "\n")) 
  previous_year <- year - 1
  
  regions_upd_pop_df <- NULL
  
  for (region in regions){
    
    cat(paste0('\nCurrent region is: ', region))
    
    
    if (year == start_year + 1){
      
      region_prev_pop <- tot_base_upd_pop_df[Region == region]
      
    } else {
      
      region_prev_pop <- tot_upd_pop[Region == region & Year == previous_year]
    }
    
    
    
    region_fertility_df  <- fertility_df[Region == region]
    
    region_mortality_df  <- mortality_df[Region == region, 
                                         .(Race, Sex, Age, Year, Sx, lx, Lx)]
    
    
    # Data preparation for Projection of the current state  
    # Input data directory
    cur_inputs_path <- file.path(inputs_path, region)   
    
    
    # Scenario data
    scenario_df <- file.path(cur_inputs_path, paste0(cur_scenario, ".csv")) %>%
      fread()  
    
    # Sex ratio at birth according to the scenario
    cur_srb_df <- scenario_df[year == previous_year, .(year, 
                                                       Total = as.numeric(sr_A))]
    
    
    cur_srb_df <- melt(cur_srb_df, id.vars = 'year', variable.name = 'Race',
                       value.name = 'srb')
    
    cur_srb_df[, Male := .((srb/100) / (1 + (srb/100)))]
    cur_srb_df[, Female := 1 - Male]
    
    
    
    # Fertility and survival values
    cur_fertility_df <- region_fertility_df[, .SD, 
                                            .SDcols = c('Age', 'Race', str_c('rates_', previous_year))]
    cur_survival_df  <- region_mortality_df[Year == previous_year,
                                            .(Race, Sex, Age, Year, Sx)]
    cur_lx_df        <- region_mortality_df[Year == previous_year,
                                            .(Race, Sex, Age, Year, lx)]
    cur_Lx_df        <- region_mortality_df[Year == previous_year,
                                            .(Race, Sex, Age, Year, Lx)]
    
    
    
    # female birth rates
    female_birth_rates <- calculate_birth_rates(cur_fertility_df, cur_srb_df, cur_Lx_df,
                                                cur_lx_df, cur_survival_df, 'Female')  
    # male birth rates
    male_birth_rates   <- calculate_birth_rates(cur_fertility_df, cur_srb_df, cur_Lx_df, 
                                                cur_lx_df, cur_survival_df, 'Male')  
    
    
    
    # Generate transition matrix for each race
    total_transition_matrix <- generate_transition_matrix(cur_survival_df, female_birth_rates,
                                                          male_birth_rates, 'Total')
    
    
    # Apply the transition matrix 
    total_prev_pop <- region_prev_pop[str_detect(Race, 'Total')][order(Sex)][, Population]
    
    
    upd_pop_df <- update_population(total_prev_pop, total_transition_matrix,
                                    'Total')
    
    
    
    # Derive the number of births 
    total_births    <- calculate_birth_counts(region_prev_pop, female_birth_rates,
                                              male_birth_rates, 'Total')
    
    
    # Derive the number of deaths
    total_deaths    <- calculate_death_counts(region_prev_pop, cur_survival_df,
                                              'Total')
    
    
    
    # Add the number of birth and deaths to the summary table
    # Births
    total_summary_df[Region == region & Year == previous_year & Race == 'Total',
                     Births := sum(total_births)]
    
    # Deaths
    total_summary_df[Region == region &Year == previous_year & Race == 'Total',
                     Deaths := sum(total_deaths)]
    
    
    if (year == end_year + 1) {
      
      print('The end of projection reached, exiting the first loop\n')
      next
    } 
    
  
    
    # Migration calculations for the base year has already been done
    upd_pop_df  <- data.table(Year = year, Region = region, upd_pop_df) 
    
    
    
    tot_proj_pop_df <- rbind(tot_proj_pop_df, upd_pop_df)
    
    
    # Migration calculations for the current year
    # Derive international migration values
    intl_mig_df <- derive_intl_mig(cur_inputs_path, year, upd_pop_df, intl_mig_factor,
                                   cur_scenario)
    
    intl_mig_df     <- data.table(Year = year, Region = region, intl_mig_df)
    tot_intl_mig_df <- rbind(tot_intl_mig_df, intl_mig_df)
    
    
    
    # Generate a summary of international migration 
    sum_intl_mig_df <- intl_mig_df[order(Race), .(net_intl_mig = sum(net_intl_mig)),
                                   by = 'Race']
    
    
    
    # Add domestic and international migration summaries to the population summary file
    total_summary_df[Region == region & Year == year, 
                     `:=`(int_migration = sum_intl_mig_df[, net_intl_mig])]
    
    
    
    # Migration calculations for the base year has already been done
    # Update the resulting population data-frame with the international migration
    upd_pop_intl_mig_df <- upd_pop_df[intl_mig_df,
                                      .(Year, Region, Race, Sex, Age,
                                        Population = Population + net_intl_mig),
                                      on = c('Race', 'Sex', 'Age')]
    
    
    regions_upd_pop_df <- rbind(regions_upd_pop_df, upd_pop_intl_mig_df)
    
  }
  
  
  if (year == end_year + 1) {
    
    print('The end of projection reached, exiting the second loop')
    break
  }
  
  # Calculate the total in out and net migration numbers for all regions
  internal_in_migration_df  <- derive_int_in_migration(inputs_path, year, 
                                                       regions_upd_pop_df, cur_scenario, int_mig_factor)
  
  internal_out_migration_df <- derive_int_out_migration(inputs_path, year, 
                                                        regions_upd_pop_df, cur_scenario, int_mig_factor)
  
  in_migration_df  <- internal_in_migration_df$total_inmigration
  out_migration_df <- internal_out_migration_df$total_outmigration
  
  
  # People entered a region minus those who left
  net_migration_df <- in_migration_df[out_migration_df, .(Region, Race, Sex, Age,
                                                          int_in_mig, int_out_mig,
                                                          int_net_mig = int_in_mig - int_out_mig),
                                      on = c('Destination' = 'Region',
                                             'Race', 'Sex', 'Age')]
  
  net_migration_df <- data.table(Year = year, net_migration_df)
  total_in_mig_df  <- rbind(total_in_mig_df, net_migration_df) 
  
  
  
  # Store pair-wise internal migration numbers 
  pair_in_mig_df <- data.table(Year = year, 
                               internal_in_migration_df$region_pair_inmigration)
  
  pair_out_mig_df <- data.table(Year = year, 
                                internal_out_migration_df$region_pair_outmigration)
  
  total_state_in_mig_df  <- rbind(total_state_in_mig_df, pair_in_mig_df)
  total_state_out_mig_df <- rbind(total_state_out_mig_df, pair_out_mig_df) 
  
  
  # Generate a summary of internal migration 
  sum_int_net_mig_df <- net_migration_df[order(Region, Race), 
                                         .(int_net_mig = sum(int_net_mig)), 
                                         by = c('Region', 'Race')]
  
  
  total_summary_df[Year == year, in_migration := sum_int_net_mig_df[, int_net_mig]]
  
  
  
  
  # Apply internal migration to base year population
  regions_upd_pop_df <- regions_upd_pop_df[net_migration_df, 
                                           .(Year, Region, Race, Sex, Age,
                                             Population = Population + int_net_mig),
                                           on = c('Region', 'Race', 'Sex', 'Age')] 
  
  
  # Replace negative values in the population column with 0
  # regions_upd_pop_df[, Population := ifelse(Population < 0, 0, Population)]
  
  tot_upd_pop <- rbind(tot_upd_pop, regions_upd_pop_df)
  
  
  
  # Generate a summary of population 
  sum_tot_pop_df <- tot_proj_pop_df[Year == year,
                                    .(Population = sum(Population)), 
                                    by = c('Region', 'Race')][order(Region, Race)]
  
  total_summary_df[Year == year, Population := sum_tot_pop_df[, Population]]
  
}


# Add the base year population
tot_base_pop_df <- data.table(Year = start_year, tot_base_pop_df)
tot_proj_pop_df <- rbind(tot_base_pop_df, tot_proj_pop_df)
fwrite(tot_proj_pop_df, file.path(results_path, 'population_projections.csv'))


# Aggregate population for the whole city 
tot_proj_pop_agg_df <- tot_proj_pop_df[, .(Population = sum(Population)),
                                       by = .(Year)]
fwrite(tot_proj_pop_agg_df, file.path(results_path, 'pop_projections_agg.csv'))


# Write the summary file
fwrite(total_summary_df, file.path(results_path, 'pop_summary.csv'))


# Write the total migration files
fwrite(tot_intl_mig_df, file.path(results_path, 'intl_migration.csv'))
fwrite(total_in_mig_df, file.path(results_path, 'intertnal_migration.csv'))
fwrite(total_state_in_mig_df, file.path(results_path, 'pair_in_migration.csv'))
fwrite(total_state_out_mig_df, file.path(results_path, 'pair_out_migration.csv'))



# Create a separate csv file per state
for (region in regions){
  
  state_results_path <- file.path(results_path, region)
  
  if (!dir.exists(state_results_path)) {
    dir.create(state_results_path)
  } else {
    print('The folder already exists!')
  }
  
  cur_pop_df           <- tot_proj_pop_df[Region == region]
  cur_intl_mig_df      <- tot_intl_mig_df[Region == region]
  cur_int_mig_df       <- total_in_mig_df[Region == region]
  cur_state_in_mig_df  <- total_state_in_mig_df[Destination == region]
  cur_state_out_mig_df <- total_state_out_mig_df[Region == region]
  
  fwrite(cur_pop_df, file.path(state_results_path, 'pop_df.csv'))
  fwrite(cur_intl_mig_df, file.path(state_results_path, 'intl_mig_df.csv'))
  fwrite(cur_int_mig_df, file.path(state_results_path, 'int_mig.csv'))
  fwrite(cur_state_in_mig_df, file.path(state_results_path, 'state_in_mig.csv'))
  fwrite(cur_state_out_mig_df, file.path(state_results_path, 'state_out_mig.csv'))
}





