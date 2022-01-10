
#**# After feed from case study leaders the Lofoten management area and North sea 
#**# management area should be removed from the model domain

#### Set up ####

packages <- c("tidyverse", "sf", "stars") # List handy data packages
lapply(packages, library, character.only = TRUE)      # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

domains <- readRDS("./Objects/Domains.rds")

polygons <- read_sf("./Data/Norway management areas/") 

ggplot() +
  geom_sf(data = domains) +
  geom_sf(data = polygons, fill = NA, aes(colour = navn_en)) +
  theme_minimal() +
  labs(colour = NULL) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 3, byrow = TRUE))

ggsave("./Figures/bathymetry/Management areas.png")

#### Limit domain ####

crop <- filter(polygons, navn_en == "The Norwegian Sea") %>% 
  st_transform(crs = crs) %>% 
  transmute(Region = "Norwegian Shelf")

Expanded <- matrix(c(1.7, 62,
                  10, 62,
                  20, 64.6,
                  14.95204, 67.96636,
                  14.67399, 68.1533,
                  1.7, 62),
                  ncol = 2, byrow = T) %>% 
  shape() %>% 
  st_union(crop) 

ggplot() +
  geom_sf(data = domains) +
  geom_sf(data = Expanded, fill = NA) +
  theme_minimal() +
  labs(colour = NULL) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(nrow = 3, byrow = TRUE))

new_domain <- st_intersection(Expanded, st_transform(domains)) %>% 
  select(Region, Shore) %>% 
  mutate(Elevation = exactextractr::exact_extract(raster::raster("../Shared data/GEBCO_2020.nc"), ., fun = "mean"),
         area = as.numeric(st_area(.)))                                 # Measure the size of each cell

saveRDS(new_domain, "./Objects/Domains.rds")
