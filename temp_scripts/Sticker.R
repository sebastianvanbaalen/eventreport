library(hexSticker)
library(ggplot2)
library(tidyverse)
library(wesanderson)
library(sf)

shapefile_ke <- st_read("/Users/sebva935/Documents/Datasets and scripts/Shapefiles/Kenya/ke_district_boundaries.shp")


colors <- wes_palette("Darjeeling1", 4, type = "discrete")

data <- data.frame(
 y = c(5, 6, 5, 7, 8, 7, 6, 9, 6, 2, 3),
 x = c(1, 1, 2, 2, 2, 3, 1, 1, 3, 8, 9),
 size = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3),
 color = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4)
)

data <- data.frame(
  y = c(-2, -2, 0, 0.3, 0, -4, -3.7, -1.3, -1.1, -0.8),
  x = c(36, 36.4, 35.7, 35.9, 35.2, 39.5, 39.8, 37.5, 37.8, 37.1),
  size = c(2, 2, 2, 2, 2, 3, 3, 2, 2, 3),
  color = c(1, 1, 2, 2, 2, 4, 4, 3, 3, 3)
)

p <- data %>%
  ggplot() +
  geom_sf(data = shapefile_ke, linewidth = 0.15, fill = "black", color = scales::alpha("#F2AD00", 0.5)) +
  coord_sf(xlim = c(33.8, 42), ylim = c(-5, 5.7), expand = FALSE) +
  geom_point(aes(x = x, y = y, size = size, color = as.factor(color))) +
  scale_size(range = c(1, 2)) +
  scale_color_manual(values = wes_palette("Darjeeling1", 4, type = "discrete"), name = NULL) +
  theme_void() +
  guides(color = "none", size = "none")


sticker(
  p,
  package = "eventreport",
  p_x = 1.2,
  p_y = 1,
  h_fill = "black",
  h_color = "#F2AD00",
  p_size = 11,
  s_width = 1.5,
  s_height = 1.5,
  s_y = 0.9,
  s_x = 0.9,
  filename = "/Users/sebva935/Documents/Datasets and scripts/Projects/eventreport/man/figures/logo.png",
  url = "https://cran.r-project.org/package=eventreport",
  u_color = "white",
  u_size = 3
)


