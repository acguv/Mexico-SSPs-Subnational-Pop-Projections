
# Header ------------------------------------------------------------------

# Author:     
# Date:       2020-11-20 
# Purpose:    Visualization of contributing factors to U.S. state-level population
#             change including fertility/mortality, international migration, 
#             domestic migration and climate 


rm(list = ls())

# Load packages -----------------------------------------------------------

if (!require("tidyverse"))  install.packages("tidyverse") else library(tidyverse)
if (!require("sf"))         install.packages("sf")        else library(sf)
if (!require(ggpubr))       install.packages('ggpubr')    else library(ggpubr)

options(readr.show_progress = F)



# Functions ---------------------------------------------------------------

pop_scenarios_df_organizer <- function(pop_df){
  
  # Prepare the population data-frame for plotting
  wide_pop_df <- pop_df %>%
    group_by(year, scenario, name) %>%
    summarize(population = sum(population)) %>%
    ungroup() %>%
    unite("scenario_year", c("scenario", "year")) %>%
    pivot_wider(id_cols = name, names_from = scenario_year, values_from = population) %>%
    mutate(No_Mig       = (No_Mig_2 - No_Mig_1) / No_Mig_1 * 100) %>%
    mutate(Zero_Dom_Mig = (Zero_Dom_Mig_2 - Zero_Dom_Mig_1) / Zero_Dom_Mig_1 * 100) %>%
    mutate(SSP          = (SSP_2 - SSP_1) / SSP_1 * 100) %>%
    mutate(name_count   = 1:n()) %>%
    mutate(IntVsNoInt   = Zero_Dom_Mig - No_Mig) %>%
    mutate(DomVsNoDom   = SSP - Zero_Dom_Mig) 
  
  
  # Reformat the table for plot drawing
  long_pop_df <- wide_pop_df %>%
    select(name, name_count, c("No_Mig", "IntVsNoInt", "DomVsNoDom")) %>%
    pivot_longer(cols = c("No_Mig", "IntVsNoInt", "DomVsNoDom"),
                 names_to = "scenario")
  
  
  output_pop_df_list <- list(wide_pop_df = wide_pop_df, long_pop_df = long_pop_df)
  
  return(output_pop_df_list)
}



# Sort regions by the contribution of the specified demographic factor
regions_sorter <- function(state_pop_plotting_df, region_pop_plotting_df, dem_factor){
  
  state_pop_plotting_sort <- state_pop_plotting_df %>%
    filter(scenario == !!dem_factor) %>%
    arrange(desc(value)) %>%
    pull(name)
  
  
  region_pop_plotting_sort <- region_pop_plotting_df %>%
    filter(scenario == !!dem_factor) %>%
    arrange(desc(value)) %>%
    pull(name)
  
  pop_plotting_sort <- c(state_pop_plotting_sort, region_pop_plotting_sort)
  
  return(pop_plotting_sort)
}

state_sorter <- function(state_pop_plotting_df, dem_factor){
  
  state_pop_plotting_sort <- state_pop_plotting_df %>%
    filter(scenario == !!dem_factor) %>%
    arrange(desc(value)) %>%
    pull(name)
  
  pop_plotting_sort <- c(state_pop_plotting_sort)
  
  return(pop_plotting_sort)
}




# Map between the two naming systems of Mexico's states
name_mapper <- function(cur_names, lookup_vec){
  
  new_names <- c()
  for (cur_name in cur_names) {
    
    if (str_count(cur_name) > 3) {
      
      new_name <- cur_name
      
    } else {
      
      new_name <- unname(lookup_vec[cur_name])
    }
    
    new_names <- c(new_names, new_name)
  }

  
  return(new_names)
}


# Inputs and Paths --------------------------------------------------------

# Current SSP scenarios
ssp_scenarios <- c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
country       <- 'Mexico'
years         <- c(2020, 2100)


# Path to state-level inputs folder
inputs_path       <- file.path("inputs") 
state_regions_csv <- file.path(inputs_path, "Regions_Divisions.csv")



# Main program ------------------------------------------------------------

# boundary_file <- file.path(country, 'states.gpkg')

boundary_path <- file.path('states.gpkg') 

# Read the states boundary file
states_df <- boundary_path %>%
  read_sf() %>%
  select(STATE_NAME, STATE_FIPS, STATE_ABBR, abb_en) %>%
  st_drop_geometry()
  
lookup_vec <- states_df$STATE_ABBR
lookup_vec <- setNames(lookup_vec, states_df$abb_en)


# Retrieve states
states <- list.dirs(inputs_path, recursive = F, full.names = F)


# Read the data-frame that contains states regions
states_regions_df <- state_regions_csv %>% read_csv(col_types = cols())
  

plots_list <- vector(mode = 'list', length = length(ssp_scenarios))
plots_list <- setNames(plots_list, ssp_scenarios)

for (ssp_scenario in ssp_scenarios){
  
  results_folder <- file.path("outputs",paste0(ssp_scenario,"_ent"), "factor_decomposition")
  if (!dir.exists(results_folder)) dir.create(results_folder)
  
    
  # Different combinations for population dis-aggregation
  scenarios <- c("No_Mig", "Zero_Dom_Mig", ssp_scenario)
  
  
  # The master data-frame holding population projections based on different conditions
  tot_pop_df           <- data.frame(matrix(data = 0, 
                                            nrow = length(states) * length(scenarios) * length(years),
                                            ncol = 4))
  
  colnames(tot_pop_df) <- c("name", "year", "scenario", "population")
  
  tot_pop_df$name     <- rep(states, length(scenarios) * length(years))
  tot_pop_df$year     <- rep(years, each = length(states), times = length(scenarios))
  tot_pop_df$scenario <- rep(scenarios, each = length(states) * length(years))
  tot_pop_df          <- tot_pop_df %>%
    left_join(states_regions_df[, c("Code", "Region")], by = c("name" = "Code")) %>%
    rename(region = Region) %>%
    relocate(region, .after = name)
  
  
  # For all areas, what is the proportional change of population per scenario?
  for (scenario in scenarios){
    
    results_path <- file.path("outputs", paste0(ssp_scenario,"_ent"))
    
    if (scenario != ssp_scenario) results_path <- file.path(results_path, scenario)

    
    for (state in states){
      
      # Read the current state's csv file to a data-frame 
      state_csv <- file.path(results_path, state, str_c("pop_df.csv"))
      state_df  <- read_csv(state_csv, col_types = cols())
      
      
      for (year in years){
        
        # Populate the total data-frame with the specified population change 
        tot_pop_df[tot_pop_df$name == state & tot_pop_df$year == year &
                     tot_pop_df$scenario == scenario, "population"] <- sum(state_df$Population[state_df$Year == year])
      }
    }
  }
  
  
  # Make some modifications to the data-frame
  tot_pop_df <- tot_pop_df %>%
    # mutate(name = map_chr(name, ~ str_split(.x, '-')[[1]][2])) %>%
    mutate(scenario = if_else(str_detect(scenario, "SSP"), "SSP", .$scenario)) %>%
    mutate(year = if_else(year == years[1], 1, 2))
  
  
  
  # Population data-frames for plotting
  # State 
  state_pop_plotting_df <- tot_pop_df %>% 
    select(-region) %>%
    pop_scenarios_df_organizer() %>%
    .[[2]] 
  # %>%
  #   mutate(name = name_mapper(name, lookup_vec))
  # 
  
  
  # Regional population data-frame for plotting
  region_pop_plotting_df <- tot_pop_df %>%
    select(-name) %>%
    rename(name = region) %>%
    pop_scenarios_df_organizer() %>%
    .[[2]] %>%
    mutate(name_count = name_count + 100)
  
  
  
  # Sort regions by the contribution of the specified demographic factor
  pop_plotting_sort <- regions_sorter(state_pop_plotting_df, region_pop_plotting_df,
                                      'DomVsNoDom')
  # pop_plotting_sort <- state_sorter(state_pop_plotting_df, 'DomVsNoDom')
  
  
  # Population data-frame for bar plotting
  pop_plotting_df <- bind_rows(state_pop_plotting_df, region_pop_plotting_df) %>%
    mutate(name = factor(name, levels = unique(pop_plotting_sort), ordered = T))
  # pop_plotting_df <- state_pop_plotting_df %>%
  #   mutate(name = factor(name, levels = unique(pop_plotting_sort), ordered = T))
    
    
  
  # Data-frames containing the total population change 
  # State
  state_pop_df <- tot_pop_df %>% 
    select(-region) %>%
    pop_scenarios_df_organizer() %>%
    .[[1]] 
  # %>%
  #   mutate(name = name_mapper(name, lookup_vec))
  # 
  
  # Regional
  region_pop_df <- tot_pop_df %>%
    select(-name) %>%
    rename(name = region) %>%
    pop_scenarios_df_organizer() %>%
    .[[1]] %>%
    mutate(name_count = name_count + 100)
  
  
  # Population data-frame for point plotting
  total_point_df <- bind_rows(state_pop_df, region_pop_df) %>%
    mutate(name = factor(name, levels = unique(pop_plotting_sort), ordered = T))
  
  # Population data-frame for point plotting
  # total_point_df <- state_pop_df %>%
  #   mutate(name = factor(name, levels = unique(pop_plotting_sort), ordered = T))
  
  

  
  # Plotting ----------------------------------------------------------------
  cur_plot <- ggplot() +
    geom_col(data = pop_plotting_df,
             aes(x = name, y = value, fill = scenario, col = scenario)) +
    
    geom_point(data = total_point_df, 
               aes(x = name, y = SSP, size = 2), fill = NA) +
    
    scale_x_discrete(expand = c(0.02, 0.02)) +
    # scale_fill_discrete(name = "Effect of:",
    #                     breaks = c("No_Mig", "IntVsNoInt", "DomVsNoDom"),
    #                     labels = c("Fertility/Mortality/Age", "International Migration",
    #                                "Domestic Migration")) +
    scale_fill_manual(name = "Effect of:",
                        breaks = c("No_Mig", "IntVsNoInt", "DomVsNoDom"),
                        labels = c("Fertility/Mortality/Age", "International Migration",
                                   "Domestic Migration"),
                      values = c("grey10","grey60","grey90")) +
    scale_y_continuous(n.breaks = 8) +
    scale_size_identity(name = "", breaks = 2, labels = "Overall % Population Change",
                        guide = "legend") +
    scale_color_manual(breaks = c("No_Mig", "IntVsNoInt", "DomVsNoDom"),
                       values=c('black', 'black', 'black'), guide = NULL) +
    
    labs(x = "", y = "% contribution to\npopulation change", subtitle = ssp_scenario) +
    
    theme_bw() +
    theme(plot.title   = element_text(size = 12, face = "bold", hjust = 0.5), 
          axis.text.x  = element_text(size = 11, angle = 90, hjust = 0.95, vjust=0.3), 
          axis.text.y  = element_text(size = 11, angle = 45),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 11),
          legend.title = element_text(size=11),
          legend.text  = element_text(size=11),
          panel.grid.minor = element_blank(), 
          legend.position  = "bottom",
          legend.box       = "horizontal",
          axis.line  = element_line(colour = "black")) +
    guides(fill = guide_legend(nrow = 4, title = "", size = 11)) 
    
  
    # ggsave(filename = file.path(results_folder, str_c("factor_comp_", ssp_scenario, 
    #                                                   "_IntVsNoInt", ".jpg")),
    #                             plot = cur_plot, width = 10, dpi = 500)
  
  plots_list[[ssp_scenario]] <- cur_plot
}




final_decom_plot <- ggarrange(ggarrange(plots_list[['SSP1']] + rremove('legend'),
                                        plots_list[['SSP2']] + rremove('legend'), ncol = 2),
                              ggarrange(plots_list[['SSP3']] + rremove('legend'), 
                                        plots_list[['SSP4']] + rremove('legend'), ncol = 2),
                              ggarrange(plots_list[['SSP5']] + rremove('legend'), 
                                        get_legend(plots_list[['SSP2']]), ncol = 2),
                              nrow = 3)

ggsave(file.path("outputs",str_c('Fig3_grey.png')), 
       final_decom_plot, dpi = 500, width = 12, height = 10)



