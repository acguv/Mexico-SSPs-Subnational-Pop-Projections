
# Script Header -----------------------------------------------------------

# Author : Hamidreza Zoraghein
# Purpose: Create plots of population changes for different age groups 
# Date   : 02/11/2024 (modified)




# Install and Load Required Packages --------------------------------------

if (!requireNamespace('tidyverse', quietly = T))    install.packages('tidyverse')
if (!requireNamespace('scales', quietly = T))       install.packages('scales')
if (!requireNamespace('RColorBrewer', quietly = T)) install.packages('RColorBrewer')
if (!requireNamespace('colorspace', quietly = T))   install.packages('colorspace')
if (!requireNamespace('ggpubr', quietly = T))       install.packages('ggpubr')


library(tidyverse)
library(scales)
library(RColorBrewer)
library(colorspace)
library(ggpubr)



# Functions ---------------------------------------------------------------

# Prepare the age group data-frames 
age_group_df_prepare <- function(population_df, age_group){
  
  # separate the age group string to its elements
  lower_age  <- as.numeric(str_split(age_group, '<', simplify = T)[, 1])
  higher_age <- as.numeric(str_split(age_group, '<', simplify = T)[, 2])
  
  
  # Create the output modified migration data-frame
  output_age_group_df <- population_df[Age >= lower_age & Age <= higher_age, 
                                       .(Population = sum(Population), 
                                         age_group = age_group), 
                                       by = c('Year', 'Race')]
  
  return(output_age_group_df)
} 




# Inputs and Paths --------------------------------------------------------

cur_scenario <- 'SSP1_ent'
outputs_path <- file.path('outputs', cur_scenario)  




# Main Body ---------------------------------------------------------------

# List the states
states <- list.dirs(outputs_path, full.names = F, recursive = F)

# Create the color palette
age_cols <- darken(brewer.pal(4, "Oranges"), 0.2)



# Create the natural growth plot per state
for (state in states){
  
  # Read the population projection csv file
  pop_df <- file.path(outputs_path, state, 'pop_df.csv') %>%
    fread()
  
  
  # Read and prepare data-frames for different age groups
  # First age group (0, 15)
  first_age_group_df <- pop_df %>%
    age_group_df_prepare('0<15')
  
  
  # Second age group (16, 64)
  second_age_group_df <- pop_df %>%
    age_group_df_prepare('16<64')
  
  
  # Third age group (65, 80)
  third_age_group_df <- pop_df %>%
    age_group_df_prepare('65<80')
  
  
  # Fourth age group (81, 100)
  fourth_age_group_df <- pop_df %>%
    age_group_df_prepare('81<100')
                              

  # Combine all data-frames
  age_df <- bind_rows(first_age_group_df, second_age_group_df, third_age_group_df, 
                      fourth_age_group_df)
  age_df[, age_group := factor(age_group, 
                               levels = c('81<100', '65<80', '16<64', '0<15'), 
                               ordered = T)]

  
  
  
  # Create the area plot
  cur_area_plot <- ggplot(age_df) + 
    geom_area(aes(x = Year, y = Population, fill = age_group)) + 
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    scale_y_continuous(breaks = pretty_breaks(n = 6),
                       labels = function(x) str_c(x/1e6, 'M')) +
    scale_fill_manual(name = 'Age Group:', values = age_cols,
                      labels = c('80+', '65 - 80', '16 - 64', '<15')) +
    labs(x = 'Year', y = 'Population') +
    theme_classic() +
    theme(
      legend.position      = 'bottom',
      legend.justification = 'center',
      panel.grid           = element_blank()
    ) +
    guides(fill = guide_legend(reverse = T))
  
  
  # Create the bar-plot
  cur_prop_plot <- ggplot(age_df) + 
    geom_col(aes(x = Year, y = Population, fill = age_group), position = position_fill()) + 
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    scale_y_continuous(breaks = pretty_breaks(n = 10),
                       labels = percent_format(accuracy = 1), position = 'right') +
    scale_fill_manual(name = 'Age Group:', values = age_cols,
                      labels = c('80+', '65 - 80', '16 - 64', '<15')) +
    labs(x = 'Year', y = 'Percentage of Total Population') +
    theme_classic() +
    theme(
      legend.position      = 'bottom',
      legend.justification = 'center',
      panel.grid           = element_blank()
    ) +
    guides(fill = guide_legend(reverse = T))
  
  
  
  # Combine the two plots and Save it
  final_plot <- ggarrange(cur_area_plot, cur_prop_plot, common.legend = T, 
                          ncol = 2, legend = 'bottom')
  final_plot %>%
    ggsave(file.path(outputs_path, state, str_c(state, '_age_group_change.jpg')), .,
           width = 10)
    
}






