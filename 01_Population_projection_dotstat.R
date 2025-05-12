# POPULATION PROJECTIONS FROM PDH .STAT 
# Luis de la rua - luisr@spc.int - May 2025

# Retrieve population counts from .STAT https://stats.pacificdata.org/
# to generate the population grids - this will make things faster in future iterations.

# 1. SETTINGS ==================================================================
install.packages("rsdmx")
library(rsdmx)
library(tidyverse)
source("setup.R")

# 2. ACCESS TO POPULATION DATASET ==============================================

# Define the SDMX URL
url <- "https://stats-sdmx-disseminate.pacificdata.org/rest/data/SPC,DF_POP_PROJ,3.0/A..MIDYEARPOPEST._T._T?startPeriod=2017&endPeriod=2027"

# Read the SDMX data
sdmx_data <- readSDMX(url)

# Convert the SDMX object to a dataframe
pop_proj_df <- as.data.frame(sdmx_data)

# View the structure
str(pop_proj_df)

# Example: filter the dataframe by a specific year
target_year <- "2025"
pop_proj_filtered <- subset(pop_proj_df, obsTime == target_year) %>% 
  rename(ISO2 = GEO_PICT)

# View the filtered dataframe
print(pop_proj_filtered)

# 3. MANIPULATE ===============================================================
# ISO 2 and ISO 3 Equivalence with country names
pacific_lookup <- data.frame(
  ISO2 = c(
    "AS", "CK", "FJ", "FM", "GU", "KI", "MH", "MP", "NC",
    "NR", "NU", "PF", "PG", "PN", "PW", "SB", "TK", "TO",
    "TV", "VU", "WF", "WS"
  ),
  ISO3 = c(
    "ASM", "COK", "FJI", "FSM", "GUM", "KIR", "MHL", "MNP", "NCL",
    "NRU", "NIU", "PYF", "PNG", "PCN", "PLW", "SLB", "TKL", "TON",
    "TUV", "VUT", "WLF", "WSM"
  ),
  Country = c(
    "American Samoa", "Cook Islands", "Fiji", "Micronesia (Federated States of)",
    "Guam", "Kiribati", "Marshall Islands", "Northern Mariana Islands",
    "New Caledonia", "Nauru", "Niue", "French Polynesia",
    "Papua New Guinea", "Pitcairn Islands", "Palau", "Solomon Islands",
    "Tokelau", "Tonga", "Tuvalu", "Vanuatu", "Wallis and Futuna",
    "Samoa"
  )
)

# Merge with population from .STAT
pop_stat <- merge(pacific_lookup, pop_proj_filtered , by = "ISO2")

# 4. ExPORT INTO WORK DIRECTORY
write.csv(pop_stat, "pop_stat.csv")
