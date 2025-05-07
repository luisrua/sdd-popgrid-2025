# POPULATION GRID KIRIBATI - KIR - 2025 UPDATE

# Use of census data and country projections to generate 33m and 100m resolution 
# Population grids
# Input data: 2020 Population and Housing Census dataset

# GENERATING RASTERS BY AGE AND SEX !!!!

## Luis de la Rua - luisr@spc.int - MARCH 2024
# ============================================================================ #

# 1. SETTINGS ================================================================
# setup script that loads libraries and resolve conflict between functions 
source("setup.R")

# No need to establish conditions to avoid expensive process, small datasets.

# Set paths to data input / output

wd <- "C:/git/spc/sdd-popgrid-2025"
setwd(wd)

# Country code to name output data and other files
country <- 'KIR'
year <- "2025"

# Data directory
# we keep using what we have in 2023 update as it is the latest data available for the input
dd <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2023/",country,"/")
output <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2025/",country,"/")
dmap <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2025/",country,"/maps/")


# 2. PREPARE ALL DATA INPUT ===================================================

# 2.1 Create dataframe with total population + coordinates from PHC dataset. ----

# Import datasets directly from NADA 
stata_files <- list.files (glue("C:/Users/{user}/OneDrive - SPC/NADA/Kiribati/SPC_KIR_2020_PHC_v01_M/Data/Original/Raw data/03_clean_data"),
                           pattern = "*.dta", full.names = T)
stata_files

hous <- read_stata(stata_files[1])
pop <- read_stata(stata_files[2])

# Get labels function is defined in setup script
view(get_labels(hous))
view(get_labels(pop))

# Dataset hhid + coordinates
hous_lite <- hous %>% 
  select(c(interview__key,buildingGPS__Longitude,buildingGPS__Latitude)) %>% 
  rename(x =buildingGPS__Latitude,
         y = buildingGPS__Longitude) # coordinates are swapped in the raw dataset

# Take a look at what we are counting (priv hh / institutions)
hous_priv <- hous %>% 
  select(c(interview__key,dwelling_type,occupancy)) %>% 
  group_by(occupancy,dwelling_type) %>% 
  summarize(dwell = n()) %>% 
  print(n=100)

# Calculate number of occupied dwellings that respond to interview to compare later the merge
dwell_subset <- data.frame(dwell = hous_priv$dwell[1:8])
occupd <- sum(rowSums(dwell_subset, na.rm = TRUE)) 

# We are taking into account priv HH and institutions
# Collapse to calculate total population and population by age range and sex
pop_df_hh <- pop %>% 
  select(c(interview__key,sex, age, age_grp5)) %>% 
  count(interview__key,age_grp5,sex) %>% 
  pivot_wider(.,names_from = c(sex,age_grp5), values_from = n) %>% # collapse by age and sex
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>%  # remove NAs
  select(sort(names(.))) %>% # arrange variables
  relocate(`1_5`, .after = `1_0`) %>% 
  relocate(`2_5`, .after = `2_0`) %>% 
  relocate(interview__key , .before = `1_0`) %>% 
  mutate(t_pop = rowSums(select(.,`1_0`:`2_95`)), # Calculate totals
         m_pop = rowSums(select(.,`1_0`:`1_95`)),
         f_pop = rowSums(select(.,`2_0`:`2_95`))
  ) %>% 
  rename_with(~gsub("^1_", "m_", .x), starts_with("1_")) %>% 
  rename_with(~gsub("^2_", "f_", .x), starts_with("2_")) %>% 
  relocate(c(t_pop,m_pop,f_pop), .after = interview__key) 

names(pop_df_hh)
nrow(pop_df_hh)

# Merge with hh locations to retrieve hh coordinates
pop_df_hh <- merge(pop_df_hh, hous_lite, by = "interview__key", all.x = T)
nrow(pop_df_hh)
sum(is.na(pop_df_hh))

sum(pop_df_hh$t_pop)
# Remove NAs from dataset
pop_df_hh <- na.omit(pop_df_hh)

# Convert into spatial using coordinates
# This is our resource to distribute spatially the population
# Remove NAs prior conversion to spatial df

pts <- pop_df_hh %>%
  st_as_sf(. , coords = c("x", "y"), crs = 4326)

# Remove empty geometries
pts = pts[!st_is_empty(pts),]
plot(pts)

# Convert dataset into pacific centric projection

pts <- st_transform(pts, crs = 3832)
crs(pts)
plot(pts)

# Remove empty geometries
pts = pts[!st_is_empty(pts),]
plot(pts)


# 2.2 Extent ----
# Bring Zone layer to avoid points excluded
zone <- st_read(paste0(dd,"layers/zone.gpkg"))
crs(zone)

ggplot() +
  geom_sf(data = zone) +
  geom_sf(data = pts, color = "red", size = 2)

# 2.3 Blank raster ----
# generate blank raster 100m

rast100m <- raster()
extent(rast100m) <- extent(zone)
res(rast100m) <- 100
rast100m

# set Pacific Centric projection
crs(rast100m) <- crs(pts)
rast100m

# 2.4 Define Population projection parameters to use in the distribution table ----

# Calculated from latest census dataset, if little discrepancies appear because we ignore few hh with no coordinates
# Not a huge problem as we are projecting population to 2023 only if amount of HH without coordinates is not relevant
pop_2021 <- sum(pts$t_pop)  


popstat <- read.csv("pop_stat.csv") # population from .STAT with the year projected population

prjpop <- popstat[popstat$ISO3 == country, "obsValue"]


# 3. PROCESS DATA TO GENERATE THE POPULATION GRID ============================

# 3.1 RASTERIZE census dataset (Check that pop field is integer) ----
# to check that rasterize and pop projection works (we can skip this maybe)

rastpop2020_100m <- rasterize(pts,rast100m,'t_pop',fun=sum)
rastpop2020_100m
totpop2020_count <- cellStats(rastpop2020_100m, 'sum')
totpop2020_count

plot(rastpop2020_100m)

# optional draw histogram ignoring NA values
# hist(na.omit(getValues(rastpop2020_100m)))

# Save census raster
# writeRaster(rastpop2020_100m ,paste0(dd,'raster/',country,'_pop2020.tif'), overwrite=TRUE)

# 3.2 Project population in dataframe ----
# Projecting population data up to current year using Standard distribution approach
# We assume that population
# grows to same degree across all population groups.

# project population on GPS location dataset
pts_prj <- pts
pts_prj

# Create Standard Distribution table by dividing the population at HH level by the
# total population census 

stand_d <- pts_prj %>% 
  mutate_at(vars(-1, -geometry),function(x)x/pop_2021)
# Check
sum(stand_d$t_pop)

# Project population on GPS location dataset using stand_d and multiplying it by the 
# projected population

pts_prj <- stand_d %>% 
  mutate_at(vars(-1, -geometry),function(x)x*prjpop) %>% 
  rename_at(vars(-1, -geometry),~ paste0(. , "_", year)) # rename variables

# Check
sum(pts_prj$t_pop_2025)

# 3.3 Rasterize projected dataset ----

# locate again the fields we are going to use
fields <- grep("_2025$", names(pts_prj), value = T)

# Rasterize 
tic()
for (field in fields){
  rast_field <- rasterize(pts_prj,rast100m, field, fun=sum)
  writeRaster(rast_field, paste0(output,"raster/",country,"_",field,".tif"), overwrite=TRUE)
}
toc()

# 3.4 Create rasters for target population groups ----
# Load rasters.
youth_rasters <- c(paste0(output,"raster/",country,"_f_0_2025",".tif"),
                   paste0(output,"raster/", country,"_f_5_2025",".tif"),
                   paste0(output,"raster/", country,"_f_10_2025",".tif"),
                   paste0(output,"raster/", country,"_f_15_2025",".tif"),
                   paste0(output,"raster/", country,"_m_0_2025",".tif"),
                   paste0(output,"raster/", country,"_m_5_2025",".tif"),
                   paste0(output,"raster/", country,"_m_10_2025",".tif"),
                   paste0(output,"raster/", country,"_m_15_2025",".tif")
)

wfag_rasters <- c(paste0(output,"raster/",country,"_f_15_2025",".tif"),
                  paste0(output,"raster/",country,"_f_20_2025",".tif"),
                  paste0(output,"raster/",country,"_f_25_2025",".tif"),
                  paste0(output,"raster/",country,"_f_30_2025",".tif"),
                  paste0(output,"raster/",country,"_f_35_2025",".tif"),
                  paste0(output,"raster/",country,"_f_40_2025",".tif"),
                  paste0(output,"raster/",country,"_f_45_2025",".tif")
)

old_rasters <- c(paste0(output,"raster/",country,"_f_65_2025",".tif"),
                 paste0(output,"raster/", country,"_f_70_2025",".tif"),
                 paste0(output,"raster/", country,"_f_75_2025",".tif"),
                 paste0(output,"raster/", country,"_f_80_2025",".tif"),
                 paste0(output,"raster/", country,"_f_85_2025",".tif"),
                 paste0(output,"raster/", country,"_f_90_2025",".tif"),
                 paste0(output,"raster/", country,"_f_95_2025",".tif"),
                 paste0(output,"raster/",country,"_m_65_2025",".tif"),
                 paste0(output,"raster/", country,"_m_70_2025",".tif"),
                 paste0(output,"raster/", country,"_m_75_2025",".tif"),
                 paste0(output,"raster/", country,"_m_80_2025",".tif"),
                 paste0(output,"raster/", country,"_m_85_2025",".tif"),
                 paste0(output,"raster/", country,"_m_90_2025",".tif"),
                 paste0(output,"raster/", country,"_m_95_2025",".tif")
)
youth_stack <- rast(youth_rasters)
wfag_stack <- rast(wfag_rasters)
old_stack <- rast(old_rasters)

youth_popgrid <- sum(youth_stack, na.rm = T)
wfag_popgrid <- sum(wfag_stack, na.rm = T)
old_popgrid <- sum(old_stack, na.rm = T)

# Check with database
# sum for total population
popgrid_list <- c(youth_popgrid, wfag_popgrid, old_popgrid)

sums <- sapply(popgrid_list, function(r) global(r, sum, na.rm = TRUE)[1,1])
names(sums) <- c("Youth", "wfag", "old")


# Calculate from census database
# calculate populations for target groups for later checks
poprate <- pop_2021 / prjpop

ypop <- sum(pop$age < 20, na.rm = TRUE)
wfagpop <- sum(pop$age >= 15 & pop$age <= 49 & pop$sex == 2, na.rm = TRUE)
oldpop <- sum(pop$age >= 65, na.rm = TRUE)

yprjpop <- ypop/poprate
wprjpop <- wfagpop/poprate
oldprjpop <- oldpop/poprate

row_prjpop <- c(yprjpop, wprjpop, oldprjpop)

# Compare results sums with projected population
compare_pop <- rbind(
  Actual = sums,
  Projected = row_prjpop
)
compare_pop

# Export target population rasters
writeRaster(youth_popgrid, paste0(output,"raster/",country,"_","t_pop_0_19",".tif"), overwrite=TRUE) 
writeRaster(wfag_popgrid, paste0(output,"raster/",country,"_","w_pop_15_49",".tif"), overwrite=TRUE) 
writeRaster(old_popgrid, paste0(output,"raster/",country,"_","t_pop_65_plus",".tif"), overwrite=TRUE) 

# Clean workspace
rm(hous, hous_lite, pop, pop_df_hh)
gc()

# 4. GENERATE GENERIC MAP DISPLAYING THE RASTER ===============================

# 4.1 Create interactive map with leaflet ----

raster_data <- rast(paste0(dd,"raster/KIR_t_pop_2025.tif"))

# options(viewer = NULL) # force map on browser

# Create a color palette with transparent NA values
colorPalette <- colorNumeric(
  palette = "Greens",  
  domain = values(raster_data),
  na.color = "transparent"  # Specify transparent color for NA values
)
# title
title_text <- "<div style='font-family: Calibri, sans-serif; font-size: 22px; font-weight: bold;'>KIRIBATI - 2025 Population Grid</div>"

# to check tile providers https://leaflet-extras.github.io/leaflet-providers/preview/
extent <- ext(raster_data)

map_labels <- leaflet() %>%
  addControl(html = title_text, position = "topright") %>% 
  addProviderTiles("CartoDB.DarkMatter") %>%  # Choose your preferred tile provider
  addRasterImage(raster_data, 
                 colors = colorPalette, 
                 opacity = 0.5,
                 project = T) %>%
  addLegend(
    position = "topright",  # Change legend position to "topright"
    pal = colorPalette,  # Use the reversed color palette
    values = values(raster_data),
    title = "Population Density <br> (pers./ha)",
    opacity = 0.5
  ) 

map_labels

map_sat <- leaflet((options = leafletOptions(viewer = NULL))) %>%
  addControl(html = title_text, position = "topright") %>% 
  addProviderTiles("Esri.WorldImagery") %>%  # Choose your preferred tile provider
  addRasterImage(raster_data, 
                 colors = colorPalette, 
                 opacity = 0.8) %>%
  setView(-180, 0, zoom=2) %>% 
  addLegend(
    position = "topright",  # Change legend position to "topright"
    pal = colorPalette,  # Use the reversed color palette
    values = values(raster_data),
    title = "Population Density <br> (pers./ha)",
    opacity = 0.8
  )
map_sat

# Produce a map as html that can be shared as it is selfcontained using "htmlwidgets" library

saveWidget(map_sat, file = paste0(dd, country,"_ppg_2025.html"), selfcontained = TRUE)


# 4.2 Create static map to be exported as image file ----
library(tmaptools)

# We need to convert the raster to a SpatialPixelsDataFrame
spatial_data <- raster_data
ab <- st_read(paste0(dd,"layers/KIR_IID2018_4326.shp"))
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
  tm_layout(main.title = "Kiribati \n2025 Population Grid",
            main.title.fontface = 'bold',
            legend.position = c("right", "bottom"),  
            legend.bg.color = "white", 
            legend.bg.alpha = 0.7,
            legend.outside = T) +  
  tm_scale_bar(width = 0.25,
               position = c('right','bottom'),
               color.dark = 'black')+
  tm_layout(frame = TRUE, bg.color = "lightblue")
tm_map

tmap_save(tm_map, paste0(dd,country,"_PPG.png")) #  ok this saves the static map now I need to reproduce what is in leaflet with tmap

# tmap_mode("plot")            
# tmap_mode("view")

# 4.3 Create atlas map to display population grid at Division level so we can better visualize it
# Iterate maps over Province / Divisions

for (i in 1:length(ab)) {
  div <- ab[i, ]  # Subset the administrative boundaries data for the current country
  
  # Set extent of each map
  extent <- extent(div)
  # Get division name
  division_name <- toupper(unique(div$iid_name))
  
  # Create a map for the current country
  map <- leaflet() %>%
    addControl(html = paste("<h3>KIRIBATI - 2025 Population Grid</h3>", "<h4>", paste0('Island: ',iid_name) , "</h4>"), 
               position = "topright") %>% 
    addProviderTiles("Esri.WorldImagery") %>%  # Choose your preferred tile provider
    addRasterImage(raster_data, 
                   colors = colorPalette, 
                   opacity = 0.8) %>%
    addPolygons(data = div, 
                fillColor = "transparent",
                color = "transparent",
                label = ~iid_name,
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
  filename <- paste0(dd,division_name, "_map.html")  # Construct the filename
  saveWidget(map, file = filename, selfcontained = TRUE)  # Save the map
}




