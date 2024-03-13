
### Map with state grouping

state_boundary_path <- file.path('states.gpkg') 

# Read the state-level spatial file
state_sf <- state_boundary_path %>%
  read_sf() %>%
  st_transform(6372)

state_regions_csv <- file.path("inputs_v2/Regions_Divisions.csv") 

states_regions_df <- state_regions_csv %>% read_csv(col_types = cols()) %>%
  rename(STATE_ABBR = Code)

names(state_sf)

state_sf <- state_sf %>%
  left_join(states_regions_df, "STATE_ABBR")


output_map <- ggplot(data = state_sf) +
  geom_sf(aes(fill = Region), color = "black") +
  coord_sf(xlim = c(st_bbox(state_sf)[1] - 1000, st_bbox(state_sf)[3] + 1000),
           ylim = c(st_bbox(state_sf)[2] - 1000, st_bbox(state_sf)[4] + 1000),
           expand = F) +
  geom_sf_text(aes(label = STATE_ABBR), colour = "black", fontface = "bold", size = 3) +
  scale_fill_manual(name = "Group",  values = c("#FFC107", "#449AE4")) +
  theme(legend.position   = c(0.2, 0.1), 
        legend.box        = 'horizontal',
        axis.text         = element_blank(),
        axis.title         = element_blank(),
        axis.ticks        = element_blank(),
        legend.title      = element_text(size = 12),
        legend.text       = element_text(size = 11),
        legend.background = element_rect(fill = 'white'),
        panel.background  = element_rect(fill = 'white'),
        panel.grid.major  = element_line(color = "transparent"),
        plot.margin       = margin(0, 0, 0, 0, "mm")) 


output_map %>%
  ggsave("outputs/Figures/FigS4.png", ., height = 6, width = 9)
