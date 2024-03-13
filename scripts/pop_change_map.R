
# Header ------------------------------------------------------------------

# Purpose: Visualize population change for Mexico over 2020-2100
# Author:  Hamidreza Zoraghein
# Date:    03/07/2024


rm(list = ls())

# Install and Load Packages  ----------------------------------------------

if (!requireNamespace('tidyverse', quietly = T))  install.packages('tidyverse')
if (!requireNamespace('data.table', quietly = T)) install.packages('data.table')
if (!requireNamespace('sf', quietly = T))         install.packages('sf')
if (!requireNamespace('scales', quietly = T))     install.packages('scales')



library(tidyverse)
library(data.table)
library(sf)
library(scales)



# Functions ---------------------------------------------------------------

# Mapping function
dem_map <- function(state_sf, summary_df, join_col, variable, legend_title){
  
  # Join the summary data-frame to the sf
  state_sf <- state_sf %>%
    left_join(summary_df, join_col)
  
  
  # Create the map
  output_map <- ggplot() +
    geom_sf(data = state_sf, aes(fill = .data[[variable]])) +
    coord_sf(xlim = c(st_bbox(state_sf)[1] - 1000, st_bbox(state_sf)[3] + 1000),
             ylim = c(st_bbox(state_sf)[2] - 1000, st_bbox(state_sf)[4] + 1000),
             expand = F) +
    # scale_fill_viridis_c()+
    scale_fill_gradient2(name = legend_title, breaks = pretty_breaks(6),
                        low = 'red', mid = 'white', high = 'blue') +
    theme(legend.position   = c(0.2, 0.1), 
          legend.box        = 'horizontal',
          axis.text         = element_blank(),
          axis.ticks        = element_blank(),
          legend.title      = element_text(size = 12),
          legend.text       = element_text(size = 11),
          legend.background = element_rect(fill = 'white', color = 'black'),
          panel.background  = element_rect(fill = 'white'),
          panel.grid.major  = element_line(color = "transparent"),
          plot.margin       = margin(0, 0, 0, 0, "mm")) +
    guides(fill = guide_colorbar(title.position = 'top', direction = 'horizontal',
                                 barwidth = unit(3.5, 'in'), barheight = 0.5, order = 1))
  
  return(output_map)
}




# Inputs and Paths --------------------------------------------------------

scenario     <- 'SSP5_ent'
legend_title <- '% Population change between 2020 and 2100'
map_name     <- 'popchange20202100.jpg'


outputs_path        <- file.path('outputs', scenario) 
state_boundary_path <- file.path('states.gpkg') 



# Main Program ------------------------------------------------------------

states <- list.dirs(outputs_path, full.names = F, recursive = F)


# Read the state-level spatial file
state_sf <- state_boundary_path %>%
  read_sf() %>%
  st_transform(6372)


# A summary data-frame that will hold all state-level results
summary_df <- state_sf%>%
  st_drop_geometry() %>%
  dplyr::select(STATE_NAME, STATE_ABBR) %>%
  mutate(per_change = 0)


for (state in states) {
  
  # Read the projected population file for the state
  proj_df <- file.path(outputs_path, state, 'pop_df.csv') %>%
    read_csv(col_types = cols()) %>%
    filter(Year %in% c(2020, 2100)) %>%
    group_by(Year) %>%
    summarize(Population = sum(Population)) %>%
    pivot_wider(names_from = 'Year', values_from = 'Population')
  
  
  # Retrive the total population in the projection year 
  pop_change <- (proj_df[, '2100'] - proj_df[, '2020']) / proj_df[, '2020'] 
  
  
  summary_df <- summary_df %>%
    mutate(per_change = if_else(STATE_ABBR == state, pop_change[,1] * 100,
                                per_change))
  
}



# Create the map
dem_map <- dem_map(state_sf, summary_df, 'STATE_ABBR',
                   'per_change', legend_title)

# Save the map
plot_path    <- file.path(outputs_path, map_name)

dem_map %>%
  ggsave(plot_path, ., height = 6, width = 9)


# Create the map
dem_map_SSP5 <- dem_map


ggarrange(dem_map_SSP1, dem_map_SSP2, dem_map_SSP3, dem_map_SSP4, dem_map_SSP5,
          ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom", 
          labels = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5"))

dem_map_SSP1 <- dem_map_SSP1 + theme(legend.position = "none")
dem_map_SSP2 <- dem_map_SSP2 + theme(legend.position = "none")
dem_map_SSP3 <- dem_map_SSP3 + theme(legend.position = "none")
dem_map_SSP4 <- dem_map_SSP4 + theme(legend.position = "none")
dem_map_SSP5 <- dem_map_SSP5 + theme(legend.position = "none")

library(cowplot)

ggdraw() +
  draw_plot(dem_map_SSP1, x = 0, y = .67, width = 1/3, height = 1/3) + 
  draw_plot(dem_map_SSP2, x = .5, y = .67, width = 1/3, height = 1/3) +
  draw_plot(dem_map_SSP3, x = 0, y = .67, width = 1/3, height = 1/3) + 
  draw_plot(dem_map_SSP4, x = .5, y = .67, width = 1/3, height = 1/3) +
  draw_plot(dem_map_SSP5, x = 0, y = .67, width = 1/3, height = 1/3) 


legend <- get_legend(dem_map_SSP1, position = )

plot_grid(dem_map_SSP1, dem_map_SSP2, dem_map_SSP3, dem_map_SSP4, dem_map_SSP5,
          ncol = 2, nrow = 3, labels = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5", ""),
          label_x = .5) + 
  draw_grob(legend, x = .55, y = .08)

ggsave("outputs/pop_change_map.png")
