# POPULATION GRID PALAU - PLW - 2025 UPDATE

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
country <- 'PLW'

# Data directory
dd <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2023/",country,"/")
dmap <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2023/",country,"/maps/")
output <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2025/",country,"/")


# 2. PREPARE ALL DATA INPUT ===================================================

# 2.1 Create dataframe with total population + coordinates from PHC dataset. ----
# Population at EA level from 2020 Census dataset in NADA
stata_files <- list.files (glue("C:/Users/{user}/OneDrive - SPC/NADA/Palau/SPC_PLW_2020_PHC_v01_M/Data/Original"),
                           pattern = "*.dta", full.names = T)
stata_files

hous <- read_stata(stata_files[2])
cover <- read_stata(stata_files[1])
pop <- read_stata(stata_files[3])

# Housing that contains hhsize
# Simplify the dataset
hous <- hous %>%
  select(c(cs7, cs8_3, cs8_1, cs8_2)) %>%
  rename(hhid = cs7,
         tpop = cs8_3,
         mpop = cs8_1,
         fpop = cs8_2)

# We use population as we are producing pop raster by age and sex.
pop <- pop %>% 
  select(c(cs7, sex, age, age_grp5)) %>% 
  rename(hhid = cs7)

# Pivot table to obtain population by age and sex at hh level
pop_disag <- pop %>% 
  select(c(hhid,sex, age, age_grp5)) %>% 
  count(hhid,age_grp5,sex) %>% 
  pivot_wider(.,names_from = c(sex,age_grp5), values_from = n) %>% # collapse by age and sex
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>%  # remove NAs
  select(sort(names(.))) %>% # arrange variables
  mutate(`2_85` = `2_85` + `2_90` + `2_95`,
         `1_85` = `1_85` + `1_90` + `1_95`) %>% # Harmonise top age groups
  select(-c(`2_90`,`2_95`,`1_90` , `1_95` )) %>% # and remove top age group to avoid overcount
  relocate(`1_5`, .after = `1_0`) %>% 
  relocate(`2_5`, .after = `2_0`) %>% 
  relocate(hhid , .before = `1_0`) %>% 
  mutate(t_pop = rowSums(select(.,`1_0`:`2_85`)), # Calculate totals
         m_pop = rowSums(select(.,`1_0`:`1_85`)),
         f_pop = rowSums(select(.,`2_0`:`2_85`))
  ) %>% 
  rename_with(~gsub("^1_", "m_", .x), starts_with("1_")) %>% 
  rename_with(~gsub("^2_", "f_", .x), starts_with("2_")) %>% 
  relocate(c(t_pop,m_pop,f_pop), .after = hhid) 

names(pop_disag)
nrow(pop_disag)

# # Cover that will help me to check if table merge is working
# cover <- cover %>% 
#   select(c(cs1_a, cs1_b, cs7)) %>% 
#   rename(lastname = cs1_a,
#          firstname = cs1_b,
#          hhid = cs7) 
# 
# # Merge cover and hous to retrieve head names
# hous <- merge(hous, cover, by="hhid")
# hous <- hous %>% 
#   filter(!is.na(tpop))

# Population from Census dataset
sum(pop_disag$t_pop)

# HH locations from listing they contain hhid that likely to connect one to one with census data
hhloc <- st_read(paste0(dd,"layers/PLW_HHListing_2020_4326.gpkg"))
hhloc <- hhloc %>% 
  rename(hhid = HHID) %>% 
  select(c(hhid,HOHH,x_4326,y_4326))

# There is a possibility of linking Census data with hh listing using hh codes
# Clean codes in States T and Y to ensure perfect match between two datasets
hhloc_match <- hhloc %>% 
  mutate(hhid = str_replace(hhid, "T15", "T01"),
         hhid = str_replace(hhid, "Y16", "Y01"))

hhloc_pop <- merge(hhloc_match, pop_disag, by = "hhid")
hhloc_pop <- hhloc_pop %>% 
  filter(!is.na(t_pop))

# Population obtained after connecting hhlocations / We loss 9 persons with respect the census dataset
# (official figures - 17614 - https://www.palaugov.pw/wp-content/uploads/2022/09/2020-Census-of-Population-and-Housing.pdf
# ) but worth it as we are having more granular information about age/sex pop spatial distribution
sum(hhloc_pop$t_pop)

# rename hhloc data to follow convention names below
pts <- hhloc_pop

# 2.2 Extent ----
# Bring Zone layer or admin boundaries to avoid points excluded
ab <- st_read(paste0(dd,"layers/GSID_2015_4326.shp"))

# 2.3 Blank raster ----
# generate blank raster 100m

rast100m <- raster()
extent(rast100m) <- extent(ab)
res(rast100m) <- 0.001
rast100m

# set 4326 projection
crs(rast100m) <- crs(pts)
rast100m

# 2.4 Define Population Growth Rates parameters ----

# 2.4 Define Population Growth Rates parameters ----

pop_2020 <- sum(pts$t_pop)  

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
# hist(na.omit(getValues(rastpop2025_100m)))

# Save census raster
# writeRaster(rastpop2025_100m ,paste0(dd,'raster/',country,'_pop2025.tif'), overwrite=TRUE)

# 3.2 Project population in dataframe ---------

# Project all population for all the population variables. We assume that population
# grows to same degree across all population groups. For the moment this is a simplified
# methodology that we could improve in the future when more demographic input is 
# available
# Create Standard Distribution table by dividing the population at HH level by the
# total population census 


# project population on GPS location dataset
pts_2025 <- pts
pts_2025

stand_d <- pts_2025 %>% 
  select(-c(HOHH, x_4326, y_4326)) %>% 
  mutate_at(vars(-1,-geometry,),function(x)x/pop_2020)

# Check
sum(stand_d$t_pop)

# Project population on GPS location dataset using stand_d and multiplying it by the 
# projected population

pts_2025 <- stand_d %>% 
  mutate_at(vars(-1, -geometry),function(x)x*prjpop) %>% 
  rename_at(vars(-1, -geometry),~ paste0(. , "_2025")) # rename variables

# Check
sum(pts_2025$t_pop_2025)


# 3.3 Rasterize projected dataset ----

# locate again the fields we are going to use
fields <- grep("_2025$", names(pts_2025), value = T)

tic()
for (field in fields){
  rast_field <- rasterize(pts_2025,rast100m, field, fun=sum)
  writeRaster(rast_field, paste0(output,"raster/",country,"_",field,".tif"), overwrite=TRUE)
}
toc()



# 3.4 Create rasters for target population groups ----
# Load rasters.
youth_rasters <- c(paste0(output,"raster/", country,"_f_0_2025",".tif"),
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

old_rasters <- c(paste0(output,"raster/", country,"_f_65_2025",".tif"),
                 paste0(output,"raster/", country,"_f_70_2025",".tif"),
                 paste0(output,"raster/", country,"_f_75_2025",".tif"),
                 paste0(output,"raster/", country,"_f_80_2025",".tif"),
                 paste0(output,"raster/", country,"_f_85_2025",".tif"),
                 paste0(output,"raster/", country,"_m_65_2025",".tif"),
                 paste0(output,"raster/", country,"_m_70_2025",".tif"),
                 paste0(output,"raster/", country,"_m_75_2025",".tif"),
                 paste0(output,"raster/", country,"_m_80_2025",".tif"),
                 paste0(output,"raster/", country,"_m_85_2025",".tif")
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
poprate <- pop_2020 / prjpop

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

raster_data <- rast(paste0(dd,"raster/",country,"_tpop_2025.tif"))

# options(viewer = NULL) # force map on browser

# Create a color palette with transparent NA values
colorPalette <- colorNumeric(
  palette = "Greens",  
  domain = values(raster_data),
  na.color = "transparent"  # Specify transparent color for NA values
)
# title
title_text <- "<div style='font-family: Calibri, sans-serif; font-size: 22px; font-weight: bold;'>PALAU -2025 Population Grid</div>"

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
    opacity = 0.5 ) %>% 
  addPolygons(data = ab,
              fillColor = "transparent",  # Set fill color to transparent
              color = "white",  # Set border color
              weight = 1, # Set border weight
              label = ~sname,
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
              label = ~sname,
              labelOptions = labelOptions(noHide = TRUE  ,
                                          textOnly = TRUE ,
                                          style = list(color = "white")))
map_sat

# Produce a map as html that can be shared as it is selfcontained using "htmlwidgets" library

saveWidget(map_sat, file = paste0(dmap, "plw_ppg_2025.html"), selfcontained = TRUE)

# 4.2 Create static map to be exported as image file ----
library(tmaptools)

# We need to convert the raster to a SpatialPixelsDataFrame
spatial_data <- raster_data
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
  tm_layout(main.title = "Palau \n2025 Population Grid",
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

# tmap_save(tm_map, paste0(dmap,country,"_PPG.png")) #  ok this saves the static map now I need to reproduce what is in leaflet with tmap

# tmap_mode("plot")            
# tmap_mode("view")

# 4.3 Create atlas map to display population grid at Division level so we can better visualize it
# Iterate maps over Province / Divisions

for (i in 1:nrow(ab)) {
  div <- ab[i, ]  # Subset the administrative boundaries data for the current country
  
  # Set extent of each map
  extent <- extent(div)
  # Get division name
  division_name <- toupper(unique(div$sname))
  
  # Create a map for the current country
  map <- leaflet() %>%
    addControl(html = paste("<h3>Palau -2025 Population Grid</h3>", "<h4>", paste0('Group of States: ',division_name) , "</h4>"), 
               position = "topright") %>% 
    addProviderTiles("Esri.WorldImagery") %>%  # Choose your preferred tile provider
    addRasterImage(raster_data, 
                   colors = colorPalette, 
                   opacity = 0.8) %>%
    addPolygons(data = div, 
                fillColor = "transparent",
                color = "white",
                label = ~sname,
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

