library(tidyverse)
library(tigris)
library(sf)

phl_crs = 2272
phl_states <- c("PA", "NJ")

phl <- counties(state = "PA") %>%
  filter(NAME == "Philadelphia")%>%
  st_transform(crs=phl_crs)

phl_bbox <- st_bbox(phl)
padding <- 8000
phl_bbox[1] <- phl_bbox[1] - padding
phl_bbox[2] <- phl_bbox[2] - padding
phl_bbox[3] <- phl_bbox[3] + padding
phl_bbox[4] <- phl_bbox[4] + padding

bbox_sf <- st_as_sfc(phl_bbox)

region_counties_water <- counties(state = phl_states)%>%
  st_transform(crs=phl_crs)%>%
  st_intersection(., bbox_sf) %>%
  erase_water(.)

region_roads_PA <- primary_secondary_roads(state = "PA")%>%
  st_transform(crs=phl_crs)%>%
  st_intersection(., bbox_sf)
region_roads_NJ <- primary_secondary_roads(state = "NJ")%>%
  st_transform(crs=phl_crs)%>%
  st_intersection(., bbox_sf)
region_primary_roads <-primary_roads()%>%
  st_transform(crs=phl_crs)%>%
  st_intersection(., bbox_sf)

region_roads <- rbind(region_roads_PA, region_roads_NJ)

create_basemap <- function() {
  ggplot() +
    geom_sf(data = bbox_sf, fill = alpha("lightblue", .5), color = NA) +
    geom_sf(data = region_counties_water, color = NA, fill = "white") +
    geom_sf(data = region_roads, color = alpha("grey20", .6), lwd = .2) +
    geom_sf(data = region_roads, color = alpha("grey90", .6), lwd = .1) +
    geom_sf(data = region_primary_roads, color = alpha("grey20", .6), lwd = .5) +
    geom_sf(data = region_primary_roads, color = alpha("grey90", .6), lwd = .3) +
    coord_sf(xlim = c(phl_bbox[1], phl_bbox[3]), 
             ylim = c(phl_bbox[2], phl_bbox[4]), 
             expand = TRUE)
}

phl_border <- function() {
  geom_sf(data = phl, color = "grey10", fill = NA, lwd = .8)
}