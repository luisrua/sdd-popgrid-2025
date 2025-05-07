# POPULATION GRID COOK ISLANDS - COK - 2025 UPDATE

# Use of census data and country projections to generate 33m and 100m resolution 
# population grids
# Input data last 2021 census dataset

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
country <- 'COK'
year <- "2025"

# Data directory 
# we keep using what we have in 2023 update as it is the latest data available for the input
dd <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2023/",country,"/")
output <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2025/",country,"/")
dmap <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2025/",country,"/maps/")
layer <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2023/",country,"/layers/")


# 2. PREPARE ALL DATA INPUT ===================================================
# We follow a hybrid approach as the hh datasets are incompleted. 
# Two sources RAS https://sdd.spc.int/collection/2021-2022-covid-19-ras-rapid-assessment-survey-cook-islands and
# HH listing carried out prior to HIES to harmonise EA framework

# 2.1 OUTTER ISLANDS LISTING ------

# Admin boundaries 
ab <- vect(paste0(layer,"2023_CK_EA_update.gpkg" ))
names(ab)

# HH listing 2023 for outter islands. We have a clean hh locations that we are going to connect
# with the population information contained in the hh listing dataset
hh_list <- vect(paste0(layer,"outter_islands_hhloc_clean.gpkg")) 

# Open the dataset, connect it with the corrected locations and keep population field
dset_list <- read_stata("C:/Users/luisr/SPC/SDD GIS - Documents/Census/2023/2023_CK_HHListing/GPS_chcks/SSdata/CK_HML2023_2_STATA_All/CK_HML2023.dta")

# Filter occupied private households
dset_list <- dset_list %>% 
  select(c(interview__key, dwelling_type, numTotal)) %>% 
  filter(dwelling_type==1) %>% 
  filter(!is.na(numTotal))

sum(dset_list$numTotal)

# Merge with the correct hh locations
hh_list <- merge(hh_list , dset_list , by = "interview__key" )
head(hh_list)
# View(as.data.frame(hh_list))

# Clean variables and rename population field
hh_list <- hh_list %>% 
  rename(tpop = numTotal) %>% 
  select(interview__key, tpop)
sum(hh_list$tpop)

# Identify the EAs enumerated in this outter island listing
# Extract the EAs that have listing points

ea_list <- terra::intersect( ab, hh_list )
#a_list <- summarize(ea_list, sum = sum(tpop, na.rm = T))s by EA code
ea_list <- ea_list %>% 
  group_by(ea_2021) %>%
  summarize(tpop = sum(tpop, na.rm = T)) %>% 
  mutate(ea_list = 1)


# Identify in Admin boundaries layer the EAs that have listing points
ab <- merge(ab, ea_list, by = "ea_2021", all.x =T)
head(ab)
sum(ab$ea_list, na.rm =T)

# 2.2 RAS survey -----
ras_hh <- vect(paste0(layer,"EMCI Covid Listing.gpkg"))

# EAs not covered in listing
ab_ras <- ab %>% 
  filter(is.na(ea_list)) %>% 
  select(ea_2021)

# Crop hhlocations with this ab_ras EAs
ras_hh_crop <- crop(ras_hh, ab_ras)

ras_pop <- ras_hh_crop %>% 
  mutate(tpop=1) %>% 
  select(tpop)

sum(ras_pop$tpop)

plot(ras_pop, col = "red", pch = 16)
# Add points2 with blue color
points(hh_list, col = "blue", pch = 16)

# Combine both datasets
hhloc <- rbind(ras_pop,hh_list)

hhloc <- hhloc %>% 
  select(tpop)



# 2.3. Define Population Growth Rates parameters and grid parametes ============

pop_2021 <- sum(hhloc$tpop)  # calculated from latest census dataset

popstat <- read.csv("pop_stat.csv") # population from .STAT with the year projected population

prjpop <- popstat[popstat$ISO3 == country, "obsValue"]

# 2.4 Create blank raster to support population grid ==========================

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

pts <- hhloc

# 3.1 RASTERIZE census dataset (Check that pop field is integer) ----
# To check that rasterize and pop projection works creating census year population grid
rastpop2021_100m <- rasterize(st_as_sf(pts),rast100m,'tpop',fun=sum)
rastpop2021_100m
totpop2021_count <- cellStats(rastpop2021_100m, 'sum')
totpop2021_count

plot(rastpop2021_100m)

# optional draw histogram ignoring NA values
hist(na.omit(getValues(rastpop2021_100m)))

# Save census raster
# writeRaster(rastpop2021_100m ,paste0(dd,'raster/',country,'_pop2021.tif'), overwrite=TRUE)

# 3.2 Project population in dataframe ----
# Projecting population data up to current year

# project population on GPS location dataset
pts_2025 <- st_as_sf(pts)
pts_2025

# Create Standard Distribution table by dividing the population at HH level by the
# total population census 

stand_d <- pts_2025 %>% 
  mutate_at(vars(tpop),function(x)x/pop_2021)

# Check
sum(stand_d$tpop)

# Project population on GPS location dataset using stand_d and multiplying it by the 
# projected population

pts_2025 <- stand_d %>% 
  mutate_at(vars(tpop),function(x)x*prjpop) %>% 
  rename_at(vars(tpop),~ paste0(. , "_2025")) # rename variables

# Check
sum(pts_2025$tpop_2025)

# 3.4 Rasterize projected dataset ----

rastpop2025rps_100m<- rasterize(pts_2025,rast100m,'tpop_2025',fun=sum)
rastpop2025rps_100m
cellStats(rastpop2025rps_100m, 'sum')

writeRaster(rastpop2025rps_100m ,paste0(output,"/raster/", country,"_t_pop_2025.tif"), overwrite=TRUE)
plot(rastpop2025rps_100m)

# Clean workspace
rm(rastpop2021_100m,hhloc,raster_data)
gc()

# 4. GENERATE GENERIC MAP DISPLAYING THE RASTER ===============================

# 4.1 Create interactive map with leaflet ----

raster_data <- rastpop2025rps_100m
ab <- st_read(paste0(layer,"CK_EA_PHC_2021_FINAL.gpkg"),  layer="CK_IID_PHC_2021_FINAL") 
# options(viewer = NULL) # force map on browser

# Create a color palette with transparent NA values
colorPalette <- colorNumeric(
  palette = "Greens",  
  domain = values(raster_data),
  na.color = "transparent"  # Specify transparent color for NA values
)
# title
title_text <- "<div style='font-family: Calibri, sans-serif; font-size: 22px; font-weight: bold;'>COOK ISLANDS - 2025 Population Grid</div>"

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
              label = ~i_name ,
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
              label = ~i_name,
              labelOptions = labelOptions(noHide = TRUE  ,
                                          textOnly = TRUE ,
                                          style = list(color = "white")))
map_sat

saveWidget(map_sat, file = paste0(dmap,country,"_ppg_2025.html"), selfcontained = TRUE)  # Save the map

# 4.2 Create static map to be exported as image file ----
library(tmaptools)

# We need to convert the raster to a SpatialPixelsDataFrame
spatial_data <- as(raster_data, "SpatialPixelsDataFrame")
ab <- st_read(paste0(layer,"WSM_HIES_Region_4326.gpkg"))
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
            title = "Population Density/n(pers./ha)",
            alpha = 0.8) +
  tm_layout(main.title = "COOK ISLANDS - 2025 Population Grid",
            main.title.fontface = 'bold',
            legend.position = c("right", "bottom"),  
            legend.bg.color = "white", 
            legend.bg.alpha = 0.7,
            legend.outside = T) +  
  tm_scale_bar(width = 0.2,
               position = c('right','top'),
               color.dark = 'black')+
  tm_layout(frame = TRUE, bg.color = "lightblue")
tm_map

#tmap_save(tm_map, paste0(dd,country,"_PPG.png")) #  ok this saves the static map now I need to reproduce what is in leaflet with tmap

# tmap_mode("plot")            
# tmap_mode("view")


# 4.3 Create atlas map to display population grid at Division level so we can better visualize it
# Iterate maps over Province / Divisions

for (i in 1:nrow(ab)) {
  div <- ab[i, ]  # Subset the administrative boundaries data for the current country
  
  # Set extent of each map
  extent <- extent(div)
  # Get division name
  division_name <- toupper(unique(div$i_name))
  
  # Create a map for the current country
  map <- leaflet() %>%
    addControl(html = paste("<h3>Cook Islands - 2025 Population Grid</h3>", "<h4>", paste0('Islands: ',division_name) , "</h4>"),
               position = "topright") %>%
    addProviderTiles("Esri.WorldImagery") %>%  # Choose your preferred tile provider
    addRasterImage(raster_data,
                   colors = colorPalette,
                   opacity = 0.8) %>%
    addPolygons(data = div,
                fillColor = "transparent",
                color = "white",
                label = ~i_name,
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

