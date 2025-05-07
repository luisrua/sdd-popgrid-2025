# POPULATION GRID TONGA - TON - 2025 UPDATE

# Use of census data and country projections to generate 33m and 100m resolution 
# population grids

# DATA INPUT
# 2021 TONGA PHC / Population projections and estimates in PDH.stat 
# https://stats-data-viewer.pacificdata.org/?chartId=f492d0e9-8668-4fa9-bfb4-fdc6170e2533

# GENERATING RASTERS BY AGE AND SEX !!!!

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
country <- 'TON'
year <- "2025"

# Data directory 
# we keep using what we have in 2023 update as it is the latest data available for the input
dd <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2023/",country,"/")
output <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2025/",country,"/")
dmap <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2025/",country,"/maps/")


# 2. PREPARE ALL DATA INPUT ===================================================

# 2.1 Create dataframe with total population + coordinates from PHC dataset. ----

# Import datasets directly from NADA 
stata_files <- list.files (glue("C:/Users/{user}/OneDrive - SPC/NADA/Tonga/SPC_TON_2021_PHC_v01_M/Data/Original"),
                           pattern = "*.dta", full.names = T)
stata_files

hous <- read_stata(stata_files[3])
pop <- read_stata(stata_files[4])


# Get labels function is defined in setup script
get_labels(hous)
view(get_labels(pop))

# Dataset hhid + coordinates
hous_lite <- hous %>% 
  select(c(interview__key,buildingGPS__Longitude,buildingGPS__Latitude)) %>% 
  rename(x =buildingGPS__Longitude,
         y = buildingGPS__Latitude)

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

# Convert into spatial using coordinates
# This is our resource to distribute spatially the population
pts <- st_as_sf(pop_df_hh, coords = c("x", "y"), crs = 4326)

# 2.2 Extent ----
# Bring Zone layer to avoid points excluded
zone <- st_read(paste0(dd,"layers/zone.gpkg"))

# 2.3 Blank raster ----
# generate blank raster 100m

rast100m <- raster()
extent(rast100m) <- extent(zone)
res(rast100m) <- 0.001
rast100m

# set 4326 projection
crs(rast100m) <- crs(pts)
rast100m

# 2.4 Define Population population growth parameters ----

pop_2021 <- sum(pts$t_pop)  # calculated from latest census dataset

popstat <- read.csv("pop_stat.csv") # population from .STAT with the year projected population

prjpop <- popstat[popstat$ISO3 == country, "obsValue"]


# 3. PROCESS DATA TO GENERATE THE POPULATION GRID ============================

# 3.1 RASTERIZE census dataset (Check that pop field is integer) ----
# to check that rasterize and pop projection works (we can skip this maybe)
rastpop2025_100m <- rasterize(pts,rast100m,'t_pop',fun=sum)
rastpop2025_100m
totpop2025_count <- cellStats(rastpop2025_100m, 'sum')
totpop2025_count

plot(rastpop2025_100m)

# optional draw histogram ignoring NA values
# hist(na.omit(getValues(rastpop2023_100m)))

# Save census raster
# writeRaster(rastpop2023_100m ,paste0(dd,'raster/',country,'_pop2023.tif'), overwrite=TRUE)

# 3.2 Project Population using Standard Distribution Method ------

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

# Clean workspace
rm(hous, hous_lite, pop, pop_df_hh)
gc()

# Export target population rasters
writeRaster(youth_popgrid, paste0(output,"raster/",country,"_","t_pop_0_19",".tif"), overwrite=TRUE) 
writeRaster(wfag_popgrid, paste0(output,"raster/",country,"_","w_pop_15_49",".tif"), overwrite=TRUE) 
writeRaster(old_popgrid, paste0(output,"raster/",country,"_","t_pop_65_plus",".tif"), overwrite=TRUE) 

# 4. GENERATE GENERIC MAP DISPLAYING THE RASTER ===============================

# 4.1 Create interactive map with leaflet ----
raster_data <- rast(paste0(output,"raster/TON_t_pop_2025.tif"))

# options(viewer = NULL) # force map on browser

# Create a color palette with transparent NA values
colorPalette <- colorNumeric(
  palette = "Greens",  
  domain = values(raster_data),
  na.color = "transparent"  # Specify transparent color for NA values
)
# title
title_text <- "<div style='font-family: Calibri, sans-serif; font-size: 22px; font-weight: bold;'>TONGA - 2025 Population Grid</div>"

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
    opacity = 0.5
  )

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
    opacity = 0.8
  )
map_sat

# Produce a map as html that can be shared as it is selfcontained using "htmlwidgets" library

saveWidget(map_sat, file = paste0(dmap, "ton_ppg_2025.html"), selfcontained = TRUE)

# 4.2 Create static map to be exported as image file ----
library(tmaptools)

# We need to convert the raster to a SpatialPixelsDataFrame
spatial_data <- raster_data
ab <- st_read(paste0(dd,"layers/TON_dvid_4326.gpkg"))
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
  tm_layout(main.title = "Tonga \n2025 Population Grid",
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

tmap_save(tm_map, paste0(dmap,country,"_PPG.png")) #  ok this saves the static map now I need to reproduce what is in leaflet with tmap

# tmap_mode("plot")            
# tmap_mode("view")

# 4.3 Create atlas map to display population grid at Division level so we can better visualize it
# Iterate maps over Province / Divisions

for (i in 1:nrow(ab)) {
  div <- ab[i, ]  # Subset the administrative boundaries data for the current country
  
  # Set extent of each map
  extent <- extent(div)
  # Get division name
  division_name <- toupper(unique(div$dv_name))
  
  # Create a map for the current country
  map <- leaflet() %>%
    addControl(html = paste("<h3>TONGA - 2025 Population Grid</h3>", "<h4>", paste0('Division: ',division_name) , "</h4>"), 
               position = "topright") %>% 
    addProviderTiles("Esri.WorldImagery") %>%  # Choose your preferred tile provider
    addRasterImage(raster_data, 
                   colors = colorPalette, 
                   opacity = 0.8) %>%
    addPolygons(data = div, 
                fillColor = "transparent",
                color = "transparent",
                label = ~dv_name,
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
  filename <- paste0(dmap,division_name, "_map.html")  # Construct the filename
  saveWidget(map, file = filename, selfcontained = TRUE)  # Save the map
}
