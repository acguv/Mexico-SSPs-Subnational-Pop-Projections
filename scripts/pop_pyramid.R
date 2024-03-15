
# Script Header -----------------------------------------------------------

# Author : 
# Purpose: Create population pyramids based on the region, scenario and year
# Date   : 02/11/2024 (modified)




# Install and Load Required Packages --------------------------------------

if (!requireNamespace('tidyverse', quietly = T))  install.packages('tidyverse')
if (!requireNamespace('scales', quietly = T))     install.packages('scales')
if (!requireNamespace('ggprism', quietly = T))    install.packages('ggprism')
if (!requireNamespace('ggpubr', quietly = T))     install.packages('ggpubr')


library(tidyverse)
library(scales)
library(ggprism)
library(ggpubr)



# Functions ---------------------------------------------------------------

pop_pyramid_plot <- function(pop_df, position = 'bottom'){
  
  ggplot(pop_df) + 
    geom_col(aes(x = Age, y = Population, fill = Sex)) + 
    scale_fill_manual(name = '', values = c('darkred', 'steelblue')) +
    scale_x_continuous(breaks = seq(0, 100, 5), guide = 'prism_minor', 
                       expand = c(0.01, 0.01), minor_breaks = seq(0, 100, 1), position = position) +
    scale_y_continuous(breaks = pretty_breaks(n = 6),
                       labels = function(x) str_c(abs(x/1e3), 'K')) +
    labs(title = year, x = 'Age') +
    coord_flip() + 
    theme_bw() +
    theme(
      legend.position      = 'bottom',
      legend.justification = 'center', 
      plot.title           = element_text(hjust = 0.5),
      panel.grid           = element_blank()
    ) +
    guides(fill = guide_legend(reverse = T))
}




# Inputs and Paths --------------------------------------------------------

cur_scenario <- 'constant_rates_ent'
outputs_path <- file.path('outputs', cur_scenario) 




# Main Body ---------------------------------------------------------------

# List the states
states <- list.dirs(outputs_path, full.names = F, recursive = F)



# Create a population pyramid plot per state
for (state in states){
  
  # Read the population data-frame
  pop_df <- file.path(outputs_path, state, 'pop_df.csv') %>%
    fread()
  
  
  # Retrieve the base year
  base_year <- min(pop_df$Year)
  
  
  # Years for analysis
  years <- c(base_year, '2050', '2100')
  
  
  states_plots_list        <- vector('list', length = length(years))
  names(states_plots_list) <- years
  
  for (year in years) {
    
    # Add necessary elements to the data-frame
    cur_pop_df <- pop_df[Year == year] 
    
    
    # Prepare the data-frame for plotting
    cur_pop_df[, Population := ifelse(Sex == 'Female', Population, Population * -1)] 

    
    # Population pyramid plots for the three years
    if (year == years[1]) {
      cur_plot <- pop_pyramid_plot(cur_pop_df)
      
    } else if (year == '2050') {
      
      cur_plot <- pop_pyramid_plot(cur_pop_df)
      cur_plot <- cur_plot +
        theme(
          axis.ticks.y = element_blank(),
          axis.text.y  = element_blank(),
          axis.title.y = element_blank()
        )
      
    } else {
      cur_plot <- pop_pyramid_plot(cur_pop_df, 'top')
      
    }
    

    # Store the current plot for the current year
    states_plots_list[[year]] <- cur_plot
  }
  
  
  # Combine the plots and Save it
  final_plot <- ggarrange(states_plots_list[[years[1]]], 
                          states_plots_list[[years[2]]],
                          states_plots_list[[years[3]]],
                          common.legend = T, ncol = 3, legend = 'bottom')
  final_plot %>%
    ggsave(file.path(outputs_path, state, str_c(state, '_pop_pyramids.jpg')), .,
           height = 5, width = 12)
}






