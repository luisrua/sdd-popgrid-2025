# POPULATION GRID FIJI - FJI - 2025 UPDATE

# population grids
# Input data last 2017 census dataset

# GENERATING DISSAGGREGATED POPULATION RASTERS BY AGE, SEX AND ETHNICITY !!!!

## Luis de la Rua - luisr@spc.int - MAY 2024
## Fiu Penjueli - pfiu@statsfiji.gov.fj
# ============================================================================ #


# 1. SETTINGS ================================================================
# setup script that loads libraries and resolve conflict between functions 
source("setup.R")

# No need to establish conditions to avoid expensive process, small datasets.

# Set paths to data input / output

wd <- "C:/git/spc/sdd-popgrid-2025"
setwd(wd)

# Country code to name output data and other files
country <- 'FJI'
year <- "2025"

# we keep using what we have in 2023 update as it is the latest data available for the input
dd <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2023/",country,"/")
output <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2025/",country,"/")
dmap <- paste0("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2025/",country,"/maps/")

# And check on all the paths included and replace them by the ones that work for your machine, if paste0 function gives you problems you can just replace that by the complete
# path to the input and output data.



# 2. PREPARE ALL DATA INPUT ===================================================

# 2.1 Create dataframe with total population + coordinates from PHC dataset. ----

pop <- read_xls("C:/Users/luisr/SPC/SDD GIS - Documents/Pacific PopGrid/UPDATE_2023/FJI/tables/EA_Tables.xls") # UPDATE THE PATH TO THE LOCATION OF THE COMPLETE DATASET

## THIS STEP IS NOT NECESSARY BUT IT SHOWS HOW TO PRODUCE THE POPULATION TABLES FROM THE ORIGINAL DATASET.
# # Collapse to calculate total population and population by age range and sex
# pop_df_hh <- pop %>% 
#   select(c(interview__key,sex, age, age_grp5)) %>% # FIU: CHANGE FIELDS NAMES IF NECESSARY and replace them across this operation
#   count(interview__key,age_grp5,sex) %>% 
#   pivot_wider(.,names_from = c(sex,age_grp5), values_from = n) %>% # collapse by age and sex
#   mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) %>%  # remove NAs
#   select(sort(names(.))) %>% # arrange variables
#   relocate(`1_5`, .after = `1_0`) %>% 
#   relocate(`2_5`, .after = `2_0`) %>% 
#   relocate(interview__key , .before = `1_0`) %>% 
#   mutate(tpop = rowSums(select(.,`1_0`:`2_95`)), # Calculate totals
#          mpop = rowSums(select(.,`1_0`:`1_95`)),
#          fpop = rowSums(select(.,`2_0`:`2_95`))
#   ) %>% 
#   rename_with(~gsub("^1_", "m_", .x), starts_with("1_")) %>% 
#   rename_with(~gsub("^2_", "f_", .x), starts_with("2_")) %>% 
#   relocate(c(tpop,mpop,fpop), .after = interview__key) 
# 
# names(pop_df_hh)
# nrow(pop_df_hh)

# 2.2 Spatial input import -----

## hh locations from census
hhloc <- vect(paste0(dd,"layers/Fiji_HH_2017.shp"))
crs(hhloc)

## We are making all the spatial analysis using Fiji Map Grid projection EPSG 3460, grabbing it from the first layer loaded
fji_crs <- crs(hhloc)

## EA boundaries layer including census population and hh counts
ea <- vect(paste0(dd,"layers/FJI_EA2017_PPGwork.gpkg"))
plot(ea)

## OSM building footprints
bf <- vect(paste0(dd,"layers/OSM/gis_osm_buildings_a_free_1.shp"))
bf <- project(bf,fji_crs) #reproject to Fiji CRS
bf_points <- centroids(bf,inside=T) # converting the layer into centroids


## 2.3  IDENTIFY EAs WITH HH LOCATIONS GAPS ----
## Extract the EAs with huge difference between census hh counts and number of HH locations to know where the data gaps can be found
eagap <- subset(ea,ea$diff > 0.5 | ea$diff < -0.5 )

# Visualize where are the EAs with data gaps
plot(eagap, border='red')
plot(ea)
plot(eagap, border='red', add=T)

## take building footprints that within the EAs with gaps
bf_ingaps <- crop(bf_points, eagap)
nrow(bf_points)
nrow(bf_ingaps)

## 2.4 FILL THE GAPS WITH OSM BUILDING FOOTPRINTS AND CALCULATE AVERAGE HH SIZE PER EA ----

## Merge hh locations from census with the points from the building footprints that are going to complete hh locations information gaps
hhloc_merged <- terra::union(hhloc,bf_ingaps) 
nrow(hhloc_merged)
hhloc_merged$val <- 1 # this is useful later to be able to count points in polygon
hhloc_merged <- hhloc_merged[,"val"] # simplify the layer
head(hhloc_merged)

## Count number of hhlocations within each EA 
ea_simpl <- ea[,"ea2017"] # before we simplify dataset

tic()
i <- st_join(st_as_sf(hhloc_merged),st_as_sf(ea_simpl)) # run intersection between EAs and hhloc so we have EA code in each hh point
toc()

## Tabulate to calculate the hhcounts by EA (collapse number of hh/EA)
hhcount <- i %>%
  as.data.frame() %>% 
  group_by(ea2017) %>% 
  summarize(hhcount = n()) 


## Merge hh count with the population table input
pop_hhcount <- merge(pop,hhcount,by.x = 'ea2017', by.y = 'ea2017')

## 2.5 Calculate Average Population per building for all the population columns ----

ea_avpop <- pop_hhcount %>% 
  rename(eaid = ea2017) %>% 
  as.data.frame() %>% 
  mutate_at(vars(-eaid, -hhcount),~ . / hhcount) %>% 
  rename_at(vars(-eaid, -hhcount),~ paste0(. , "_ahs")) # rename variables

## 2.6 Assign Average Population to each of the points for all the age groups -----
hhcoun_avpop <- merge(i,ea_avpop, by.x = "ea2017", by.y = 'eaid', all.x = T)

# test - for the moment is ok we need to include checks for all the fields 
sum(hhcoun_avpop$tpop_ahs, na.rm=T)
sum(pop$tpop)


# 3. PROJECTION PROCESS. -----
pts_2025 <- hhcoun_avpop %>% 
  select(-c(val,hhcount))

pop_2017 <- sum(pts_2025$tpop_ahs, na.rm = T) 

popstat <- read.csv("pop_stat.csv") # population from .STAT with the year projected population this file is generated running 01_Population_projection_dotstat.R script first

prjpop <- popstat[popstat$ISO3 == country, "obsValue"]


# 3.1 Draw the distribution matrix -----
stand_d <- pts_2025 %>% 
  mutate_at(vars(-1, -geometry),function(x)x/pop_2017) 

# Check
sum(stand_d$tpop, na.rm=T)

# 3.2 Project Population using distribution matrix
pts_2025 <- stand_d %>% 
  mutate_at(vars(-1, -geometry),function(x)x*prjpop) %>% 
  rename_at(vars(-1, -geometry),~ paste0(. , "_2025")) %>% # rename variables
  mutate(across(everything(), ~ replace_na(., 0))) # replacing all NAs by 0 just for the dummy dataset

# Check
sum(pts_2025$tpop_ahs_2025, na.rm=T)


# 3.3 Round Preserve Sum projected population in dataframe and check results ----

# Locate fields to be rounded
fields <- grep("_2025$", names(pts_2025), value = T)

### WE SKIP ROUNDING PROCESS AS IT SEEMS IS PRODUCING THE ADDING BY SEX GROUPS ISSUE

# # Appy RPS function over the fields
# for(field in fields){
#   new_field <- paste0(field,"_rps")
#   pts_2025[[new_field]] <- round_preserve_sum(pts_2025[[field]])
# }
# 
# # Check that RPS preserves totals
# pts_df <- as.data.frame(pts_2025)
# 
# sum_2025 <- colSums(pts_df[fields])
# 
# rounded_fields <- paste0(fields, "_rps")
# sum_rounded <- colSums(pts_df[rounded_fields], na.rm = TRUE)
# 
# comparison <- data.frame(Original = sum_2025, Rounded = sum_rounded)
# comparison <- comparison %>%
#   mutate(diff = Original - Rounded)
# comparison

# 4. GENERATE RASTERS

# 4.1 Create blank Raster ----
# Extent ----
# Bring Zone layer to avoid points excluded
zone <- st_read(paste0(dd,"layers/zone.gpkg"))
zone <- st_transform(zone, crs = st_crs(pts_2025))


# Generate blank raster 100m with local CRS
rast100m <- raster()
extent(rast100m) <- extent(zone)
res(rast100m) <- 100
rast100m

# Set local projection
crs(rast100m) <- crs(pts_2025)
rast100m

# 4.2 Rasterize projected dataset ----

# Locate fields to rename them and remove the _rps piece
# clean some fields to ensure consistency
pts_2025 <- pts_2025 %>% 
  rename(t_pop_ahs_2025 = tpop_ahs_2025,
         m_pop_ahs_2025 = mpop_ahs_2025,
         f_pop_ahs_2025 = fpop_ahs_2025)

names(pts_2025)


fields <- grep("_ahs_2025$", names(pts_2025), value = TRUE)

# Keep fields we are going to rasterize and rename them to name rasters properly
pts_torast <- pts_2025 %>% 
  select(all_of(fields),geometry)

# locate again the fields we are going to use
fields <- grep("_2025$", names(pts_torast), value = T)

tic()
for (field in fields){
  rast_field <- rasterize(pts_torast,rast100m, field, fun=sum)
  writeRaster(rast_field, paste0(output,"raster/",country,"_",field,".tif"), overwrite=TRUE)
}
toc()


# 4.3 Create rasters for target population groups ----
# Check all rasters created
raster_list <- list.files(path = paste0(output,"raster/"), pattern = ".tif$", full.names = TRUE)

# and adapt below the file names to create the target population groups
# Load rasters.
youth_rasters <- c(paste0(output,"raster/",country,"_f_0_4_ahs_2025",".tif"),
                   paste0(output,"raster/", country,"_f_5_9_ahs_2025",".tif"),
                   paste0(output,"raster/", country,"_f_10_14_ahs_2025",".tif"),
                   paste0(output,"raster/", country,"_f_15_19_ahs_2025",".tif"),
                   paste0(output,"raster/", country,"_m_0_4_ahs_2025",".tif"),
                   paste0(output,"raster/", country,"_m_5_9_ahs_2025",".tif"),
                   paste0(output,"raster/", country,"_m_10_14_ahs_2025",".tif"),
                   paste0(output,"raster/", country,"_m_15_19_ahs_2025",".tif")
)

wfag_rasters <- c(paste0(output,"raster/",country,"_f_15_19_ahs_2025",".tif"),
                  paste0(output,"raster/",country,"_f_20_24_ahs_2025",".tif"),
                  paste0(output,"raster/",country,"_f_25_29_ahs_2025",".tif"),
                  paste0(output,"raster/",country,"_f_30_34_ahs_2025",".tif"),
                  paste0(output,"raster/",country,"_f_35_39_ahs_2025",".tif"),
                  paste0(output,"raster/",country,"_f_40_44_ahs_2025",".tif"),
                  paste0(output,"raster/",country,"_f_45_49_ahs_2025",".tif")
)

old_rasters <- c(paste0(output,"raster/",country,"_f_65_69_ahs_2025",".tif"),
                 paste0(output,"raster/", country,"_f_70_74_ahs_2025",".tif"),
                 paste0(output,"raster/", country,"_f_75_79_ahs_2025",".tif"),
                 paste0(output,"raster/", country,"_f_80_84_ahs_2025",".tif"),
                 paste0(output,"raster/", country,"_f_85_89_ahs_2025",".tif"),
                 paste0(output,"raster/", country,"_f_90_94_ahs_2025",".tif"),
                 paste0(output,"raster/", country,"_f_95_99_ahs_2025",".tif"),
                 paste0(output,"raster/",country,"_m_65_69_ahs_2025",".tif"),
                 paste0(output,"raster/", country,"_m_70_74_ahs_2025",".tif"),
                 paste0(output,"raster/", country,"_m_75_79_ahs_2025",".tif"),
                 paste0(output,"raster/", country,"_m_80_84_ahs_2025",".tif"),
                 paste0(output,"raster/", country,"_m_85_89_ahs_2025",".tif"),
                 paste0(output,"raster/", country,"_m_90_94_ahs_2025",".tif"),
                 paste0(output,"raster/", country,"_m_95_99_ahs_2025",".tif")
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
sums
