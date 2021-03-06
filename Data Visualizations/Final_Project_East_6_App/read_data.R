library(readr)

# can comment these out when sourced  if libraries loaded in app.R 
library(dplyr) 
library(sf)

#setwd('~/notre-dame/data_viz/East6/Data Visualizations/Final_Project_East_6_App/')


## Function for custom legends 
addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5, position){
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                           sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, 
                   labels = labelAdditions, opacity = opacity, position = position))
}


### Read in Data 
demographics_sf <- read_rds("./data/demographics.rds")
bus_in_district_sf <- read_rds("./data/business_license.rds")
school_boundaries_sf <- read_rds("./data/schools.rds")
parks_sf <- read_rds("./data/parks.rds")
public_facilities_sf <- read_rds("./data/public_facilities.rds")


### Aggregate as necessary

bus_summary <- bus_in_district_sf %>% 
  tibble() %>% 
  group_by(Dist, Council_Me, business_type) %>% 
  summarise(n = n())

school_summary <- school_boundaries_sf %>% 
  tibble() %>% 
  group_by(SchoolType, Council_Me) %>% 
  summarise(n = n())

parks_summary <- parks_sf %>% 
  tibble() %>% 
  group_by(Park_Type, Council_Me) %>% 
  summarise(n = n())

public_facilities_summary <- public_facilities_sf %>% 
  tibble() %>% 
  group_by(POPL_TYPE, Council_Me) %>% 
  summarise(n = n())



### Create Population Density Palette 
pop_pal <- colorBin(
  palette = "Blues",
  domain = filter(demographics_sf, Council_Me != "All")$pop_density,
  bins = 6, pretty = TRUE)


