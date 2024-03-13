
# Header -------------------------------------------------------------

# Author : Hamidreza Zoraghein
# Purpose: Create bilateral accumulated internal migration maps 
# Date   : 2/24/2023

rm(list = ls())

# Install and Load Packages -----------------------------------------------

if (!requireNamespace('tidyverse', quietly = T))  install.packages('tidyverse')
if (!requireNamespace('data.table', quietly = T)) install.packages('data.table')
if (!requireNamespace('sf', quietly = T))         install.packages('sf')
if (!requireNamespace('scales', quietly = T))     install.packages('scales')



library(tidyverse)
library(data.table)
library(sf)
library(scales)



# Define functions --------------------------------------------------------

# Extract centroids of states
centroids_extract <- function(states_sf){
  
  centroids_df <- states_sf$geom %>%
    st_centroid() %>%
    do.call(rbind, .) %>%
    as.data.frame()
  
  colnames(centroids_df) <- c('x', 'y')
  
  centroids_df <- states_sf %>%
    st_drop_geometry() %>%
    bind_cols(centroids_df)
  
  return(centroids_df)
}


 
# # Prepare and summarize migration values
# mig_summarize <- function(raw_values_csv){
#   
#   summarized_values_df <- raw_values_csv %>%
#     read_csv(col_types = cols()) %>%
#     summarize(across(everything(), sum)) %>%
#     t() %>%
#     as.data.frame() %>%
#     rownames_to_column() %>%
#     rename(migrants = V1)
#   
#   return(summarized_values_df)
# }



# Add locations to states and prepare a a data-frame for mapping in the specified year
mapping_prepare <- function(mig_df, centroids_df){
  
  mig_df <- mig_df %>%
    left_join(centroids_df, by = c('Origin' = 'STATE_ABBR')) %>%
    left_join(centroids_df, by = c('Destination' = 'STATE_ABBR')) %>%
    rename_with(~ str_replace(., '.x', '_origin'), ends_with('.x')) %>%
    rename_with(~ str_replace(., '.y', '_dest'), ends_with('.y'))
  
  return(mig_df)
}



# Make offsets for the arrowheads in migration maps
arrow_offset <- function(data, x1, y1, x2, y2, shorten_start, shorten_end){
  
  data$dx   <- data[, data[[x2]]] - data[, data[[x1]]]
  data$dy   <- data[, data[[y2]]] - data[, data[[y1]]]
  data$dist <- sqrt( data$dx^2 + data$dy^2 )
  data$px   <- data$dx/data$dist
  data$py   <- data$dy/data$dist
  
  data[, x1] <- data[, data[[x1]]] + data$px * shorten_start
  data[, y1] <- data[, data[[y1]]] + data$py * shorten_start
  data[, x2] <- data[, data[[x2]]] - data$px * shorten_end
  data[, y2] <- data[, data[[y2]]] - data$py * shorten_end
  
  return(data)
}



# Create the migration map
migration_map <- function(fill_df, arrow_df, legend_title, arrow_color,
                          end_color){
  
  # Determine if this mapping is for in-migration or out-migration
  if (length(unique(arrow_df$Origin)) == 1){
    
    top_10_label     <- str_flatten(arrow_df$Destination, collapse = '\n')
    mig_dir_label    <- 'Out-migration'
    annotation_title <- 'Top 10 states\n of destination:'
    
  } else {
    
    top_10_label     <- str_flatten(arrow_df$Origin, collapse = '\n')
    mig_dir_label    <- 'In-migration'
    annotation_title <- 'Top 10 states\n of origin:'
  } 
  
  
  # Generate the map
  output_map <- ggplot(fill_df) +
    geom_sf(aes(fill = Mig)) +
    labs(x = "", y = "") +
    
    scale_fill_gradient(name = legend_title, breaks = pretty_breaks(8), 
                        low = 'white', high = end_color, 
                        labels = function(x) str_c(x/1000, 'K')) +
    
    geom_segment(data = arrow_df,
                 aes(x = x_origin, y = y_origin, 
                     xend = x_dest, yend = y_dest, col = arrow_color), 
                 size = arrow_df$width, 
                 arrow = arrow(length = unit(0.2, 'cm'))) +
    
    scale_color_identity(guide = 'legend', name = 'Migration Direction', 
                         labels = mig_dir_label) +
    
    annotate(geom = 'text', x = bounding_box$xmax * 1.1, y = bounding_box$ymax,
             label = annotation_title, fontface = 2, size = 2.5, color = arrow_color) +
    
    annotate(geom  = 'text', x = bounding_box$xmax * 1.1, y = bounding_box$ymin + 0.75 * (bounding_box$ymax - bounding_box$ymin),
             label = top_10_label, size = 2.5, color = arrow_color) +
    
    coord_sf(clip = "off") +
    
    theme(legend.position  = 'bottom', legend.box = 'horizontal',
          legend.justification = c(0.1, 0),
          plot.background  = element_blank(),
          axis.text        = element_blank(),
          axis.ticks       = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "transparent")) +
    
    guides(fill = guide_colorbar(title.position = 'top', direction = 'horizontal', 
                                 barwidth = unit(4, 'in'), order = 1),
           col  = guide_legend(title.position = 'top', order = 2),
           size = guide_legend(title.position = 'top', order = 3, override.aes = list(key= NULL)))
  
  
  return(output_map)
}



# # Map between the two naming systems of Mexico's states
# name_mapper <- function(cur_names, lookup_vec){
#   
#   new_names <- c()
#   for (cur_name in cur_names) {
#     
#     if (str_count(cur_name) > 3) {
#       
#       new_name <- cur_name
#       
#     } else {
#       
#       new_name <- unname(lookup_vec[cur_name])
#     }
#     
#     new_names <- c(new_names, new_name)
#   }
#   
#   
#   return(new_names)
# }



# Inputs and Paths --------------------------------------------------------

# base_year     <- '2015'
# end_year      <- '2100'

cur_scenario  <- 'SSP3_ent'
outputs_path  <- file.path('outputs', cur_scenario)  
cur_year      <- 2100 
boundary_file <- file.path('states.gpkg') 




# Main Program ------------------------------------------------------------

# List all states
states <- list.dirs(outputs_path, full.names = F, recursive = F)


# Read the states boundary file
states_sf <- boundary_file %>%
  read_sf() %>%
  select(STATE_NAME, STATE_ABBR)

lookup_vec <- states_sf$STATE_ABBR
# lookup_vec <- setNames(lookup_vec, states_sf$abb_en) # Hamid's, returns an error
# lookup_vec <- setNames(lookup_vec, states_sf$abb_en)


# Extract the centroid and bounding box
centroids_df <- centroids_extract(states_sf)
bounding_box <- st_bbox(states_sf)



# Read in-flow values for each state
for (state in states){
  
  # Rearrange initial bilateral values and prepare them for mapping
  mig_df <- file.path(outputs_path, state, 'state_in_mig.csv') %>%
    fread()
  
  setnames(mig_df, 'Region', 'Origin')
  setnames(mig_df, 'int_in_mig', 'Mig')
  
  mig_df <- mig_df[, .(Mig = sum(Mig)), by = c('Origin', 'Destination')] %>%
    mapping_prepare(centroids_df) 
  
  
  # Create the data-frame for filling the map
  fill_df <- states_sf %>%
    left_join(mig_df, by = c('STATE_ABBR' = 'Origin'), keep = T) 
  
  
  # Create the data-frame for arrows
  arrow_df <- mig_df %>%
    slice_max(order_by = Mig, n = 10) %>%
    mutate(width = rescale(Mig, to = c(1, 4))) %>%
    arrow_offset('x_origin', 'y_origin', 'x_dest', 'y_dest', 0, 50000)
  
  
  # Create the map
  state_name   <- states_sf %>% 
    filter(STATE_ABBR == state) %>%
    pull(STATE_NAME)
  
  legend_title <- str_c('Accumulated migrants to ', state_name, ' under ',
                        cur_scenario)
  arrow_color  <- 'blue4'
  end_color    <- 'blue1'
  mig_map      <- migration_map(fill_df, arrow_df, legend_title, arrow_color, end_color)
  

  # Save the map
  map_file <- file.path(outputs_path, state, str_c(state, '_bilateral_accum_in_mig.jpg'))
  mig_map %>% ggsave(map_file, ., width = 8, height = 6)
  saveRDS(mig_map, str_replace(map_file, '.jpg', '.rdata'))
} 



# Read out-flow values for each state
for (state in states){
  
  # Rearrange initial bilateral values and prepare them for mapping
  mig_df <- file.path(outputs_path, state, 'state_out_mig.csv') %>%
    fread()
  
  setnames(mig_df, 'Region', 'Origin')
  setnames(mig_df, 'int_out_mig', 'Mig')
  
  mig_df <- mig_df[, .(Mig = sum(Mig)), by = c('Origin', 'Destination')] %>%
    mapping_prepare(centroids_df) 
  
  
  # Create the data-frame for filling the map
  fill_df <- states_sf %>%
    left_join(mig_df, by = c('STATE_ABBR' = 'Destination'), keep = T)  
  
  
  # Create the data-frame for arrows
  arrow_df <- mig_df %>%
    slice_max(order_by = Mig, n = 10) %>%
    mutate(width = rescale(Mig, to = c(1, 4))) 
  
  
  # Create the map
  state_name   <- states_sf %>% 
    filter(STATE_ABBR == state) %>% 
    pull(STATE_NAME)
  
  legend_title <- str_c('Accumulated migrants from ', state_name, ' under ',
                        cur_scenario)
  arrow_color  <- 'red4'
  end_color    <- 'red1'
  mig_map      <- migration_map(fill_df, arrow_df, legend_title,
                                arrow_color, end_color)
  
  
  # Save the map
  map_file <- file.path(outputs_path, state, str_c(state, '_bilateral_accum_out_mig.jpg'))
  mig_map %>% ggsave(map_file, ., width = 8, height = 6)
  saveRDS(mig_map, str_replace(map_file, '.jpg', '.rdata'))
  
} 



# Read net-flow values for each state
for (state in states){
  
  # Rearrange initial bilateral values and prepare them for mapping
  in_mig_df <- file.path(outputs_path, state, 'state_in_mig.csv') %>%
    fread()
  
  out_mig_df <- file.path(outputs_path, state, 'state_out_mig.csv') %>%
    fread()
  
  setnames(in_mig_df, c('Region', 'Destination'), c('Origin', 'State'))
  setnames(out_mig_df, 'Region', 'State')
  
  mig_df <- in_mig_df[out_mig_df, .(Year, Origin, State, Race, Sex, Age,
                                    Mig = int_in_mig - int_out_mig),
                          on = c('Year', 'Origin' = 'Destination', 'State',
                                 'Race', 'Sex', 'Age')]
  
  setnames(mig_df, 'State', 'Destination')
  
  mig_df <- mig_df[, .(Mig = sum(Mig)), by = c('Origin', 'Destination')] %>%
    mapping_prepare(centroids_df) 
  
  
  # Create the data-frame for filling the map
  fill_df <- states_sf %>%
    left_join(mig_df, by = c('STATE_ABBR' = 'Origin'), keep = T) 
  
  
  # Create the data-frames for arrows
  arrow_df <- mig_df %>%
    slice_max(order_by = Mig, n = 5) %>%
    bind_rows(slice_min(mig_df, order_by = Mig, n = 5)) %>%
    mutate(width = rescale(abs(Mig), to = c(1, 4))) 
  
  
  # Five states with the highest in and out-migration numbers
  in_arrow_df  <- arrow_df %>% 
    slice_max(order_by = Mig, n = 5) %>%
    arrow_offset('x_dest', 'y_dest', 'x_origin', 'y_origin', 50000, 0)
  
  out_arrow_df <- slice_min(arrow_df, order_by = Mig, n = 5)
  
  
  # Create the map
  state_name   <- states_sf %>% 
    filter(STATE_ABBR == state) %>% 
    pull(STATE_NAME)
  
  legend_title <- str_c('Accumulated net migrants for ', state_name,
                        ' under ', str_sub(cur_scenario,1,4))
  
  
  mig_map <- ggplot(fill_df) +
    geom_sf(aes(fill = Mig)) +
    labs(x = "", y = "") +
    
    scale_fill_gradient2(name = legend_title,
                         breaks = pretty_breaks(8), low = "red1", high = "blue1",
                         labels = function(x) str_c(x/1000, 'K')) +
    
    geom_segment(data = in_arrow_df,
                 aes(x = x_origin, y = y_origin, xend = x_dest, yend = y_dest, col = 'blue4'),
                 size = in_arrow_df$width, arrow = arrow(length = unit(0.3, "cm"))) +
    
    geom_segment(data = out_arrow_df,
                 aes(x = x_dest, y = y_dest, xend = x_origin, yend = y_origin, col = 'red4'), 
                 size = out_arrow_df$width, arrow = arrow(length = unit(0.3, "cm"))) +
    
    
    scale_color_identity(guide = 'legend', name = 'Migration Direction', 
                         labels = c('In-migration', 'Out-migration')) +
  
    annotate(geom = 'text', x = bounding_box$xmax * 1.1, y = bounding_box$ymax * 1.05,
           label = 'Top 5 states \nlost migrants:', fontface = 2, size = 2.5, color = 'blue4') +

    annotate(geom  = 'text', x = bounding_box$xmax * 1.1, y = bounding_box$ymin + 0.9 * (bounding_box$ymax - bounding_box$ymin),
             label = str_flatten(in_arrow_df$Origin, collapse = '\n'), size = 2.5, color = 'blue4') +

    annotate(geom = 'text', x = bounding_box$xmax * 1.1, y = bounding_box$ymin + 0.7 * (bounding_box$ymax - bounding_box$ymin),
             label = 'Top 5 states \ngained migrants:', fontface = 2, size = 2.5, color = 'red4') +

    annotate(geom  = 'text', x = bounding_box$xmax * 1.1, y = bounding_box$ymin + 0.55 * (bounding_box$ymax - bounding_box$ymin),
             label = str_flatten(out_arrow_df$Origin, collapse = '\n'), size = 2.5, color = 'red4') +
    
    coord_sf(clip = 'off') + 
    
    theme(legend.position  = 'bottom', legend.box = 'horizontal',
          plot.background  = element_blank(),
          axis.text        = element_blank(),
          axis.ticks       = element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_line(colour = "transparent")) +
    
    guides(size = 'none', 
           fill = guide_colorbar(title.position = 'top', direction = 'horizontal', 
                                 barwidth = unit(4, 'in'), order = 1),
           col = guide_legend(title.position = 'top', order = 2))
  
  
  # Save the map
  map_file <- file.path(outputs_path, state, str_c(state, '_bilateral_accum_net_mig.jpg'))
  
  mig_map %>% ggsave(map_file, ., width = 8, height = 6)
  saveRDS(mig_map, str_replace(map_file, '.jpg', '.rdata'))
} 




