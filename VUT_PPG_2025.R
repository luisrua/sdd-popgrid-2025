# POPULATION GRID VANUATU - VUT - 2025 UPDATE

# Use of census data and country projections to generate 33m and 100m resolution 
# population grids
# Input data last 2020 census dataset

## Luis de la Rua - luisr@spc.int - MAY 2025
# ============================================================================ #

# 1. SETTINGS ================================================================
# setup script that loads libraries and resolve conflict between functions 
source("setup.R")

# No need to establish conditions to avoid expensive process, small datasets.

# Set paths to data input / output

wd <- "C:/git/spc/sdd-popgrid-2025"
setwd(wd)

# Country code to name output data and other files
country <- 'VUT'
year <- "2025"

# we keep using what we have in 2023 update as it is the latest data available for the input
dd <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2023/",country,"/")
output <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2025/",country,"/")
dmap <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2025/",country,"/maps/")
layer <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2023/",country,"/layers/")


# 2. PREPARE ALL DATA INPUT ===================================================

# 2.1 Create dataframe with total population + coordinates from PHC dataset. ----
# Population from Census report / PopGIS at AC level from 2020 census - Imputations were done to cover missing population
pop <- read_xls(paste0(dd,"p1_sex.xls"))

pop <- pop %>% 
  select(c(acid,tpop)) %>% 
  rename(t_pop = tpop)

# Spatial data.
# HH locations from 2020 census 
hhloc_cen <- st_read(paste0(layer,"GPS_PHC2020clean_4326.shp"))
hhloc_cen <- hhloc_cen  %>% 
  mutate(source = 'census') %>% 
  select(source)

# Building footprints from OSM
hhloc_osm <- st_read(paste0(layer,"gis_osm_buildings_a_free_1.shp"))
hhloc_osm <- hhloc_osm  %>% 
  mutate(source = 'osm') %>% 
  select(source) %>% 
  st_centroid()

# Merge both layers so that we can cover hh locations gaps from census.
hhloc <- rbind(hhloc_cen, hhloc_osm)   
nrow(hhloc)
# plot(hhloc)

# Area council framework
ac <- st_read(paste0(layer,"AC2022_topology_clean.shp"))

# Merge ac population with framework
ac_pop <- merge(ac, pop, by = 'acid')

# Clean
ac_pop <- ac_pop %>% 
  select(c(acid, t_pop))

# Assign Average HH size to each hh location using population data at ac level.
# First retrieve wid code 
hhloc_ahs <- st_join(hhloc, ac_pop, join = st_within )
# Remove points falling out framework (acid = NA)
hhloc_ahs <- hhloc_ahs %>% 
  filter(!is.na(acid))

# Count number of points in each ac
ac_hhcount <- hhloc_ahs %>% 
  as.data.frame() %>% 
  select(acid) %>% 
  group_by(acid) %>% 
  summarise(n=n())

# Merge with pop table and calculate Average HH size for each ac
hhloc_ahs <- merge(hhloc_ahs,ac_hhcount, by = "acid")

hhloc_ahs <- hhloc_ahs %>% 
  mutate(ahs = t_pop / n) %>% 
  select(c(acid,t_pop,n,ahs)) %>% 
  rename(hhnum = n)

# check if totals matches
tpop_hhlocahs <- sum(hhloc_ahs$ahs, na.rm=T)


# 2.2 Create blank raster to support population grid

# Bring Zone layer to avoid points excluded

# generate blank raster 100m

rast100m <- raster()
extent(rast100m) <- extent(ac)
res(rast100m) <- 0.001
rast100m

# set projection
crs(rast100m) <- (crs=4326)
rast100m

# 2.4 Define Population population growth parameters ----

pop_2020 <- sum(hhloc_ahs$ahs)  # calculated from latest census dataset

popstat <- read.csv("pop_stat.csv") # population from .STAT with the year projected population

prjpop <- popstat[popstat$ISO3 == country, "obsValue"]

# 3. PROCESS DATA TO GENERATE THE POPULATION GRID ============================

pts <- hhloc_ahs

# 3.1 RASTERIZE census dataset (Check that pop field is integer) ----
# To check that rasterize and pop projection works creating census year population grid
rastpop2020_100m <- rasterize(pts,rast100m,'ahs',fun=sum)
rastpop2020_100m
totpop2020_count <- cellStats(rastpop2020_100m, 'sum')
totpop2020_count

plot(rastpop2020_100m)

# optional draw histogram ignoring NA values
hist(na.omit(getValues(rastpop2020_100m)))

# Save census raster
# writeRaster(rastpop2020_100m ,paste0(dd,'raster/',country,'_pop2020.tif'), overwrite=TRUE)

# 3.2 Project population in dataframe ----

# project population on GPS location dataset
pts_2025 <- pts
pts_2025

# Remove rows with null ahs
pts_2025 <- pts_2025[!is.na(pts_2025$ahs),]

# Clear dataset
pts_2025 <- pts_2025 %>% 
  select(c(ahs, geometry)) %>% 
  rename(t_pop = ahs)

# Create Standard Distribution table by dividing the population at HH level by the
# total population census 

stand_d <- pts_2025 %>% 
  mutate_at(vars(t_pop),function(x)x/pop_2020)

# Check
sum(stand_d$t_pop)

# Project population on GPS location dataset using stand_d and multiplying it by the 
# projected population

pts_2025 <- stand_d %>% 
  mutate_at(vars(t_pop),function(x)x*prjpop) %>% 
  rename_at(vars(t_pop),~ paste0(. , "_2025")) # rename variables

# Check
sum(pts_2025$t_pop_2025)


# 3.4 Rasterize projected dataset -----
rastpop2025rps_100m<- rasterize(pts_2025,rast100m,'t_pop_2025',fun=sum)
rastpop2025rps_100m
cellStats(rastpop2025rps_100m, 'sum')

writeRaster(rastpop2025rps_100m ,paste0(output,"/raster/", country,"_t_pop_2025.tif"), overwrite=TRUE)
plot(rastpop2025rps_100m)

# Clean workspace
rm(rastpop2020_100m,hhloc,hhloc_ahs,hhloc_cen, hhloc_osm)
gc()


# 4. GENERATE GENERIC MAP DISPLAYING THE RASTER ===============================

# 4.1 Create interactive map with leaflet ----

raster_data <- rastpop2025rps_100m
ab <- st_read(paste0(layer,"pid_4326.gpkg"))
# options(viewer = NULL) # force map on browser

# Create a color palette with transparent NA values
colorPalette <- colorNumeric(
  palette = "Greens",  
  domain = values(raster_data),
  na.color = "transparent"  # Specify transparent color for NA values
)
# title
title_text <- "<div style='font-family: Calibri, sans-serif; font-size: 22px; font-weight: bold;'>VANUATU - 2025 Population Grid</div>"

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
              label = ~pname,
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
    opacity = 0.8)  %>% 
  addPolygons(data = ab,
              fillColor = "transparent",  # Set fill color to transparent
              color = "white",  # Set border color
              weight = 1, # Set border weight
              label = ~pname,
              labelOptions = labelOptions(noHide = TRUE  ,
                                          textOnly = TRUE ,
                                          style = list(color = "white")))
map_sat

saveWidget(map_sat, file = paste0(dmap,"vut_ppg_2025.html"), selfcontained = TRUE)  # Save the map

# 4.2 Create static map to be exported as image file ----
library(tmaptools)

# We need to convert the raster to a SpatialPixelsDataFrame
spatial_data <- as(raster_data, "SpatialPixelsDataFrame")
ab <- st_read(paste0(dd,"layers/pid_4326.gpkg"))
ab <- st_transform(ab, crs(raster_data))
# Plot the population grid map

# Set up the tm_style with desired font for the title
my_style <- tm_style("classic", title.fontface = "bold", title.fontsize = 22, title.fontfamily = "Calibri")

# Create the map
tm_map <- tm_shape(ab)+
  tm_polygons() + 
  tm_shape(spatial_data) +
  tm_raster(style = "cont", 
            palette = "Greens", 
            title = "Population Density\n(pers./ha)",
            alpha = 0.8) +
  tm_layout(main.title = "VANUATU - 2025 Population Grid",
            main.title.fontface = 'bold',
            legend.position = c("right", "bottom"),  
            legend.bg.color = "white", 
            legend.bg.alpha = 0.7,
            legend.outside = T) +  
  tm_scale_bar(width = 0.2,
               position = c('left','bottom'),
               color.dark = 'black')+
  tm_layout(frame = TRUE, bg.color = "lightblue")
tm_map

tmap_save(tm_map, paste0(dd,country,"_PPG.png")) #  ok this saves the static map now I need to reproduce what is in leaflet with tmap

# tmap_mode("plot")            
# tmap_mode("view")


# 4.3 Create atlas map to display population grid at Division level so we can better visualize it ----
# Iterate maps over Province / Divisions

for (i in 1:nrow(ab)) {
  div <- ab[i, ]  # Subset the administrative boundaries data for the current country
  
  # Set extent of each map
  extent <- extent(div)
  # Get division name
  division_name <- toupper(unique(div$pname))
  
  # Create a map for the current country
  map <- leaflet() %>%
    addControl(html = paste("<h3>Vanuatu - 2025 Population Grid</h3>", "<h4>", paste0('Province: ',division_name) , "</h4>"), 
               position = "topright") %>% 
    addProviderTiles("Esri.WorldImagery") %>%  # Choose your preferred tile provider
    addRasterImage(raster_data, 
                   colors = colorPalette, 
                   opacity = 0.8) %>%
    addPolygons(data = div, 
                fillColor = "transparent",
                color = "transparent",
                label = ~pname,
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
  filename <- paste0(dmap,country,"_",division_name, "_map.html")  # Construct the filename
  saveWidget(map, file = filename, selfcontained = TRUE)  # Save the map
}
 