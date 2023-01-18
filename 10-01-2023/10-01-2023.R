# Setting up libraries----
pacman::p_load(tidytuesdayR,
               tidyverse, janitor, 
               MetBrewer, sysfonts, showtext, showtextdb,
               ggrepel, ggtext, ggthemes, gganimate,
               gridExtra, png,
               mapdata, maps)
  
# Getting data----

tuesdata <- tidytuesdayR::tt_load('2023-01-10')

feederwatch <- tuesdata$PFW_2021_public
sitedata <- tuesdata$PFW_count_site_data_public_2021

# Get extra data from earlier years (downloaded from FeederWatch)---
feederwatch_old <- read_csv("data/PFW_2016_2020_public.csv")

# Read-in the hummingbird image to use later
icon <- readPNG("data/hummer_R.png")
icon <- rasterGrob(icon, interpolate=TRUE)


# Filter the species codes for hummingbirds only----

# Find only hummingbird Species codes (downloaded from FeederWatch Dictionary)
speciescodes_hummers <- readxl::read_excel("data/FeederWatch_Data_Dictionary.xlsx",
                                   sheet = "Species Codes",
                                   skip = 1 ) %>% 
  clean_names() %>%
  select(species_code, primary_com_name, family) %>% 
  filter(family == "Trochilidae (Hummingbirds)")

# Filter the observations for hummingbirds only----

# TidyTuesday data
hummers_tt <- feederwatch %>%
  filter(species_code %in% speciescodes_hummers$species_code) %>% 
  dplyr::select(latitude, longitude, obs_id, Month, Day, Year, species_code,
         how_many, valid)

# Old FeederWatch data
feederwatch_old <-  feederwatch_old[1:(length(feederwatch_old)-1)]

# The column names differed in capitalization, standardizing them
colnames(feederwatch_old) <- colnames(feederwatch)

hummers_old <- feederwatch_old %>%
  filter(species_code %in% speciescodes_hummers$species_code) %>% 
  dplyr::select(latitude, longitude, obs_id, Month, Day, Year, species_code,
         how_many, valid)

# Join both observations----
colnames(hummers_old) <- colnames(hummers_tt)

hummers_tidy <- bind_rows(hummers_tt, hummers_old)

# Join the observations with the species names----
hummers_tidy %<>% left_join(speciescodes_hummers)

# Find the five most-observed species of hummingbirds----

top_5_hummers <- hummers_tidy %>% group_by(species_code, Year) %>%
  dplyr::summarize(num_obs = n()) %>% 
  ungroup() %>% 
  group_by(Year) %>% 
  top_n(5) 


# Create the plot object and store it in p

p <- hummers_tidy %>%
  
  filter(species_code %in% top_5_hummers$species_code) %>%  # Only top 5 species
  
  mutate(obsdate = make_date(Year, Month)) %>%  # Add observation date
  
  filter(latitude > 0) %>%  # Remove some odd observations in New Zealand (!?)
  
  ggplot(aes(x = longitude, y = latitude, 
             colour = primary_com_name, size = how_many))+
  
  borders("world", xlim = c(-200, -60), ylim = c(10, 50),
          colour =  "#CDB79E", fill =  "#CDB79E", size = 0.3)+  # map details
  
  geom_point(alpha = 0.5)+
  
  theme_map() +
  
  theme(
    text = element_text(family = "Oswald"),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.background = element_rect(fill = "#FFFDE5FD",),
    plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
    plot.caption = element_text(size = 14),
    plot.subtitle = element_text(size = 14),
    plot.title =  element_text(size = 18, hjust = 0.5,
                               margin = margin(10, 0, 10, 0)),
    legend.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.position = c(0.25,0),
    legend.key = element_blank(),
    legend.background = element_rect(color = "black",
                                     fill = "transparent",
                                     size = 2, linetype="blank"))+
  
  labs(title = "Where were the five most observed hummingbirds in the USA seen?",
       subtitle = 'Date: {closest_state}',
       caption = "Dataviz: @SaileeSakhalkar | Source: Project FeederWatch | Image: rawpixel.com on FreePik")+
  
  guides(color=guide_legend(ncol=1, byrow=F),
         (title="Five most observed Hummingbird species"))+
  
  scale_color_met_d("Isfahan1")+
  
  annotation_custom(icon, xmin = -160, xmax = -140, ymin = 30, ymax = 55)+
  
  transition_states(obsdate)

# Animate the plot----
animate(p, height = 800, width =800, device = "ragg_png")

# Save the animation----
anim_save("Hummers.gif")
