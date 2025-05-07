# This script is for functionality that is very commonly used across
# the actual analysis scripts 

# counter for checking if we have run setup.R before
if(!exists(".setup_counter")){
  .setup_counter <- 0
  quiet <- FALSE
} else {
  quiet <- TRUE
}

# user name - for locating the data on your local system
# Useful when you have synced folders from SharePoint to local.
user <- Sys.info()['login']

source("R/library2.R")
# LIBRARIES ---- 
# General tools

library2("dplyr")
library2("tidyr")
library2("tidyverse")
library2("readxl")
library2("lubridate")
library2("janitor")
library2("tictoc") # processing time
library2("purrr")
library2("haven") # manipulate stata files
library2("glue")
library(stringr)


# Mapping results
library2("colorspace")
library2("leaflet")
library2("tmap")
library2("htmlwidgets") 

# For spatial analysis
library2("exactextractr")
library2("terra")
library2("sp")
library2("sf")
library2("tidyterra")
library2("conflicted")
library2("raster")

# library("spcstyle") # See https://github.com/PacificCommunity/sdd-spcstyle-r-package
# library2("scales")
# library2("rsdmx")
# library2("glue")

# library2("ISOcodes")
# library2("patchwork")   # layout multiple charts in one image
# library2("ggrepel")     # add tet labels to points without overlapping

# CONFLICT WITH FUNCTIONS ----
conflict_prefer("select", "dplyr", quiet = quiet)
conflict_prefer("filter", "dplyr", quiet = quiet)
conflict_prefer("year", "lubridate", quiet = quiet)
conflict_prefer("first", "dplyr", quiet = quiet)
conflict_prefer("lag", "dplyr", quiet = quiet)
conflict_prefer("intersect","base", quiet=quiet)

# silence a ubiquitous and annoying message
options(dplyr.summarise.inform = FALSE)

# CUSTOM FUNCTIONS ----

## Function to get labels from stata datasets
get_labels <- function(d){
  tibble(short = names(d),
         long = as.character(sapply(d, function(x){attributes(x)$label}))
  ) |>
    mutate(column_number = 1:n())
}

## ROUND PRESERVE SUM projecting population over the point layer as source
# define formula for the round preserving sum
round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

