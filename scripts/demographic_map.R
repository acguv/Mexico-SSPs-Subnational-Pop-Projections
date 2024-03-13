
# Header ------------------------------------------------------------------

# Purpose: Generate socioeconomic projection maps for Mexico at the state level
# Author:  Hamidreza Zoraghein
# Date:    11/29/2021

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
dem_map_func <- function(state_sf, summary_df, join_col, variable, legend_title){
  
  # Join the summary data-frame to the sf
  state_sf <- state_sf %>%
    left_join(summary_df, join_col)
  
  
  # Create the map
  output_map <- ggplot() +
    geom_sf(data = state_sf, aes(fill = .data[[variable]]), color = "black") +
    coord_sf(xlim = c(st_bbox(state_sf)[1] - 1000, st_bbox(state_sf)[3] + 1000),
             ylim = c(st_bbox(state_sf)[2] - 1000, st_bbox(state_sf)[4] + 1000),
             expand = F) +
    scale_fill_distiller(name = legend_title, type = "seq", palette = "Greens", direction = 1,
                         labels = comma, guide = "colourbar", aesthetics = "fill") +
    # scale_fill_gradient(name = legend_title, breaks = pretty_breaks(6), 
    #                     low = 'white', high = 'red2') +
    # scale_fill_gradient(name = legend_title, breaks = pretty_breaks(6),
    #                     low = 'white', high = 'grey25') +
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

scenario     <- 'SSP3_ent'
legend_title <- 'Percentage of population aged 65 and above\nin 2100'
map_name     <- 'age65above.jpg'


outputs_path        <- file.path('outputs', scenario) 
state_boundary_path <- file.path('states.gpkg') 



# Main Program ------------------------------------------------------------

states <- list.dirs(outputs_path, full.names = F, recursive = F)

states <- states[states != "factor_decomposition" & states != "Zero_Dom_Mig" & states != "No_Mig"]


# Read the state-level spatial file
state_sf <- state_boundary_path %>%
  read_sf() %>%
  st_transform(6372)


# A summary data-frame that will hold all state-level results
summary_df <- state_sf%>%
  st_drop_geometry() %>%
  dplyr::select(STATE_NAME, STATE_ABBR) %>%
  mutate(proportion = 0)


for (state in states) {
  
  # Read the projected population file for the state
  proj_df <- file.path(outputs_path, state, 'pop_df.csv') %>%
    read_csv(col_types = cols()) %>%
    filter(Year == 2100)
  
  
  # Retrive the total population in the projection year 
  tot_pop <- proj_df %>% pull(Population) %>% sum()
  
  
  # Retrive the population based on the age group in the projection year 
  age_pop <- proj_df %>% 
    filter(Age >= 65) %>%
    pull(Population) %>%
    sum()
  
  
  summary_df <- summary_df %>%
    mutate(proportion = if_else(STATE_ABBR == state, (age_pop / tot_pop) * 100,
                                proportion))
  
}



# Create the map
dem_map      <- dem_map_func(state_sf, summary_df, 'STATE_ABBR',
                        'proportion', legend_title)

# Save the map
plot_path    <- file.path(outputs_path, map_name)

dem_map %>%
  ggsave(plot_path, ., height = 6, width = 9)


# Create the map
dem_map_SSP3 <- dem_map

legend <- get_legend(dem_map_SSP3)

plot_grid(dem_map_SSP1 + theme(legend.position = "none"), 
          dem_map_SSP2 + theme(legend.position = "none"), 
          dem_map_SSP3 + theme(legend.position = "none"), 
          dem_map_SSP4 + theme(legend.position = "none"),
          dem_map_SSP5+ theme(legend.position = "none"),
          ncol = 2, nrow = 3, labels = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5", ""),
          label_x = .5) + 
  draw_grob(legend, x = .55, y = 0)


## Maps for Figure S5
legend <- get_legend(dem_map_SSP3)

plot_grid(dem_map_SSP3 + theme(legend.position = "none"), 
          dem_map_SSP5+ theme(legend.position = "none"),
          ncol = 2, labels = c("SSP3", "SSP5"),
          label_x = .5) + 
  draw_grob(legend, x = .55, y = 0)

ggarrange(dem_map_SSP3, dem_map_SSP5, common.legend = TRUE, legend = "bottom")

