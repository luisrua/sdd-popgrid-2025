# POPULATION GRID MICRONESIA - FSM - 2025 UPDATE

# Use of census data and country projections to generate 33m and 100m resolution 
# population grids
# Input data last 2021 census dataset

## Luis de la Rua - luisr@spc.int - MAY 2024
# ============================================================================ #

# 1. SETTINGS ================================================================
# setup script that loads libraries and resolve conflict between functions 
source("setup.R")

# No need to establish conditions to avoid expensive process, small datasets.

# Set paths to data input / output

wd <- "C:/git/spc/sdd-popgrid-2025"
setwd(wd)

# Country code to name output data and other files
country <- 'FSM'

# Data directory
dd <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2023/",country,"/")
output <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2025/",country,"/")
dmap <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2025/",country,"/maps/")
layer <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2023/",country,"/layers/")

# 2. PREPARE ALL DATA INPUT ===================================================

# 2.1 Import dataset input --------------
# EA boundaries with population data from 2011 census
ab <- vect(paste0(layer,"FSM_EA_2010_pop.gpkg"))

# Clean att table
ab <- ab %>% 
  select(c(edid, totpop2010))
sum(ab$totpop2010)

# Building locations from OSM Geofabrik (downloaded 07/05/2024) that we are using distribute population
hhloc_osm <- vect(paste0(layer,"micronesia-latest-free.shp/gis_osm_buildings_a_free_1.shp"))

# Convert to centroids and clean att table.
hhloc_osm <- hhloc_osm %>%
  centroids(.,inside = T) %>% 
  mutate(hh_source = 'osm') %>% 
  select(hh_source)

# Building location from 2010 Listing
hhloc_2010 <- vect(paste0(layer,"FSM_GPSHH_AHS_2010_4326.shp"))
# Simplify and prepare
hhloc_2010 <- hhloc_2010 %>%  
  mutate(hh_source = '2010list') %>% 
  select(hh_source)


# 2.2 Process HH locations and alocate Average Household size to each point ---- 

# Merge both building source, no problem if they overlap as this is solved as soon as we aggregate
# up to pixel zones.
hhloc <- rbind(hhloc_2010,hhloc_osm)

# Count number of points in each EA to calculate Average Household Size.
join <- terra::intersect(hhloc,ab)

ab_join <- join %>% 
  as.data.frame() %>% 
  group_by(edid) %>% 
  summarise(hhcount = length(edid))

# Merge with ab to retrieve rest of the data
ab_join <- merge(ab, ab_join, by="edid", all.x=T)
View(as.data.frame(ab_join))

# Calculate Average Household size per EA
ab_join$ahs <- ab_join$totpop2010 / ab_join$hhcount

# Assign ahs value to each point via table join
hhloc_ahs <- terra::intersect(hhloc, ab_join)

pop_2010 <- sum(hhloc_ahs$ahs)
pop_2010

# 2.3. Define Population Growth Rates parameters and grid parametes ============

popstat <- read.csv("pop_stat.csv") # population from .STAT with the year projected population

prjpop <- popstat[popstat$ISO3 == country, "obsValue"]


# 2.3 Create blank raster to support population grid -----

# Bring Zone layer to avoid points excluded
zone <- st_read(paste0(layer,"zone_4326.shp"))

# generate blank raster 100m

rast100m <- raster()
extent(rast100m) <- extent(zone)
res(rast100m) <- 0.001
rast100m

# set projection
crs(rast100m) <- (crs=4326)
rast100m

# 3. PROCESS DATA TO GENERATE THE POPULATION GRID ============================

pts <- hhloc_ahs

pts <- pts %>% 
  st_as_sf() %>% 
  rename(t_pop = ahs)

# 3.1 RASTERIZE census dataset (Check that pop field is integer) ----
# To check that rasterize and pop projection works creating census year population grid
rastpop2021_100m <- rasterize(pts,rast100m,'t_pop',fun=sum)
rastpop2021_100m
totpop2021_count <- cellStats(rastpop2021_100m, 'sum')
totpop2021_count

plot(rastpop2021_100m)

# optional draw histogram ignoring NA values
# hist(na.omit(getValues(rastpop2021_100m)))

# Save census raster
# writeRaster(rastpop2021_100m ,paste0(dd,'raster/',country,'_pop2021.tif'), overwrite=TRUE)

# 3.2 Project population in dataframe ----
# Projecting population data up to current year

# project population on GPS location dataset
pts_2025 <- pts
pts_2025

# Create Standard Distribution table by dividing the population at HH level by the
# total population census 

stand_d <- pts_2025 %>% 
  mutate_at(vars(t_pop),function(x)x/pop_2010)

# Check
sum(stand_d$t_pop)

# Project population on GPS location dataset using stand_d and multiplying it by the 
# projected population

pts_2025 <- stand_d %>% 
  mutate_at(vars(t_pop),function(x)x*prjpop) %>% 
  rename_at(vars(t_pop),~ paste0(. , "_2025")) # rename variables

# Check
sum(pts_2025$t_pop_2025)

# 3.4 Rasterize projected dataset ----

rastpop2025rps_100m<- rasterize(pts_2025,rast100m,'t_pop_2025',fun=sum)
rastpop2025rps_100m
cellStats(rastpop2025rps_100m, 'sum')

writeRaster(rastpop2025rps_100m ,paste0(output,"raster/", country,"_t_pop_2025.tif"), overwrite=TRUE)
plot(rastpop2025rps_100m)

# Clean workspace
rm(rastpop2021_100m,pts_df,pts)
gc()

# 4. GENERATE GENERIC MAP DISPLAYING THE RASTER ===============================

# 4.1 Create interactive map with leaflet -----

raster_data <- rastpop2025rps_100m
ab <- st_read(paste0(layer,"sid_background.shp"))
ab <- st_transform(ab, 4326)
# options(viewer = NULL) # force map on browser

# Create a color palette with transparent NA values
colorPalette <- colorNumeric(
  palette = "Greens",  
  domain = values(raster_data),
  na.color = "transparent"  # Specify transparent color for NA values
)
# title
title_text <- "<div style='font-family: Calibri, sans-serif; font-size: 22px; font-weight: bold;'>FSM - 2025 Population Grid</div>"

# to check tile providers https://leaflet-extras.github.io/leaflet-providers/preview/

map_labels <- leaflet() %>%
  addControl(html = title_text, position = "topright") %>% 
  addProviderTiles("CartoDB.DarkMatter") %>%  # Choose your preferred tile provider
  addRasterImage(raster_data, 
                 colors = colorPalette, 
                 opacity = 0.5) %>%
  addLegend(
    position = "topright",  # Change legend position to "topright"
    pal = colorPalette,  # Use the reversed color palette
    values = values(raster_data),
    title = "Population Density <br> (pers./ha)",
    opacity = 0.5) %>% 
  addPolygons(data = ab,
              fillColor = "transparent",  # Set fill color to transparent
              color = "white",  # Set border color
              weight = 1, # Set border weight
              label = ~s_name,
              labelOptions = labelOptions(noHide = TRUE  ,
                                          textOnly = TRUE ,
                                          style = list(color = "white")))

map_labels

map_sat <- leaflet((options = leafletOptions(viewer = NULL))) %>%
  addControl(html = title_text, position = "topright") %>% 
  addProviderTiles("Esri.WorldImagery") %>%  # Choose your preferred tile provider
  addRasterImage(raster_data, 
                 colors = colorPalette, 
                 opacity = 0.8) %>%
  addLegend(
    position = "topright",  # Change legend position to "topright"
    pal = colorPalette,  # Use the reversed color palette
    values = values(raster_data),
    title = "Population Density <br> (pers./ha)",
    opacity = 0.8) %>% 
  addPolygons(data = ab,
              fillColor = "transparent",  # Set fill color to transparent
              color = "white",  # Set border color
              weight = 1, # Set border weight
              label = ~s_name,
              labelOptions = labelOptions(noHide = TRUE  ,
                                          textOnly = TRUE ,
                                          style = list(color = "white")))
map_sat

saveWidget(map_sat, file = paste0(dmap,"fsm_ppg_2025.html"), selfcontained = TRUE)  # Save the map

# 4.2 Create atlas map to display population grid at State level so we can better visualize it
# Iterate maps over Province / Divisions

for (i in 1:nrow(ab)) {
  div <- ab[i, ]  # Subset the administrative boundaries data for the current country
  
  # Set extent of each map
  extent <- extent(div)
  # Get division name
  division_name <- toupper(unique(div$s_name))
  
  # Create a map for the current country
  map <- leaflet() %>%
    addControl(html = paste("<h3>FSM - 2025 Population Grid</h3>", "<h4>", paste0('State: ',division_name) , "</h4>"),
               position = "topright") %>%
    addProviderTiles("Esri.WorldImagery") %>%  # Choose your preferred tile provider
    addRasterImage(raster_data,
                   colors = colorPalette,
                   opacity = 0.8) %>%
    addPolygons(data = div,
                fillColor = "transparent",
                color = "white",
                label = ~s_name,
                labelOptions = labelOptions(noHide = TRUE,
                                            textOnly = TRUE,
                                            textsize = "18px",
                                            style = list(color = "black",
                                                         font.weight = "bold",
                                                         textShadow = "1px 1px #FFFFFF"))) %>%  # Administrative boundaries
    addLegend(
      position = "topright",  # Change legend position to "topright"
      pal = colorPalette,  # Use the reversed color palette
      values = values(raster_data),
      title = "Population Density <br> (pers./ha)",
      opacity = 0.8
    )
  
  # Set the map extent around current division
  map <- map %>% fitBounds(extent[1], extent[3], extent[2], extent[4])
  
  # Save the map to a separate file
  filename <- paste0(dmap,country,"_", division_name, "_map.html")  # Construct the filename
  saveWidget(map, file = filename, selfcontained = TRUE)  # Save the map
}
