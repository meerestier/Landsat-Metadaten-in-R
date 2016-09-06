# Script to consume Surface Reflectancy metadata for bulk ordering
# WRS-2 Path and Row pairs;
# Developed by Ph. Gärtner (gaertner.p@gmail.com), in Berlin, Germany, 19th of July 2016.
# Extended by Lars Schulz (lars@larsschulz.info), 16-07-22
# Inputs:  Collection of Metadata provided as .csv dataset via
#  --- http://landsat.usgs.gov/metadatalist.php; 
# Output: A consolidated .csv of scenes to download

# Cyprus covers path 176 til 176 on row 36

# Installations ----

#install.packages("XML")
#install.packages("devtools")
#install.packages("dplyr")
#install.packages("lubridate")
#devtools::install_github("hadley/readr")

# Activate ----
library(XML)
library(devtools)
library(readr)
library(dplyr)
require(lubridate)
library(plyr)
library(ggplot2)

# Configure ----

# set path to folder, where .csv of Surface Reflectance reside
setwd('data/sr/')
getwd()

# Functions ---- 
get_metadata_csv <- function (csv, path.min, path.max, row.min, row.max) {
  
  x <- read_csv(csv)
  
  meta.sel <- subset(x, 
                     path >= path.min & path <= path.max & row >= row.min & row <= row.max, 
                     select=c(sensor, sceneID, acquisitionDate, path, row, 
                              sceneCenterLatitude, sceneCenterLongitude, 
                              cloudCover, cloudCoverFull, browseURL)) 
  
  #meta.sel$cloudfree <- as.numeric(meta.sel$cloudCoverFull)
  
  meta.sel$year <- format(meta.sel$acquisitionDate, "%Y")
  meta.sel$doy  <- yday(meta.sel$acquisitionDate)
  
  
  return(meta.sel)
}

# Consume csv from earth explorer search results
get_metadata_ee_landsat <- function (csv, path.min, path.max, row.min, row.max) {
  
  x <- read_csv(csv)
  
  meta.sel <- x 
  
  #meta.sel$cloudfree <- as.numeric(meta.sel$cloudCoverFull)
  
  #meta.sel$year <- format(meta.sel$Date Acquired, "%Y")
  #meta.sel$doy  <- yday(meta.sel$Date Acquired)
  
  
  return(meta.sel)
}

# Read data ----

# select file in folder
# a <- file.choose()
# a <- dirname(a)

# loop csv files in directory
files_csv <- list.files(getwd(), pattern = "csv")
count <- length(files_csv)
for (i in 1:count){
  file <- files_csv[i]
  assign(file, get_metadata_ee_landsat(file, 176, 176, 36, 36))
  print(file)
}


# Combine sources ----
# fill empty columns
# TODO: Parameterisieren
m <- rbind.fill(LSR_LANDSAT_TM.csv,
           LSR_LANDSAT_ETM_COMBINED.csv,
           LSR_LANDSAT_8.csv           
)

attach(m)

# m$sensor <- gsub("LANDSAT_","",m$sensor)
# m$sensor <- gsub("OLI_TIRS","OLI",m$sensor)

# reorder path from high to low
neworder <- c(177:175)

m <- arrange(transform(m, path=factor(path,levels=neworder)),path)

# Removing scenes
# m <- subset(m, sensor !=  'TIRS') # remove scenes from 'TIRS' sensor
# m <- subset(m, cloudCover <  8) # remove scenes with a higher cloud cover then 80%


# Output files ----

# save as csv
write.csv(m, file = "output/output_sr_combined.csv") # Consolidated sensor data
write.csv(summary(m), file = "output/output_sr_summary.csv") # Summary of data

# save scene list for http://earthexplorer.usgs.gov/filelist or bulk orders http://espa.cr.usgs.gov/ordering/new/
scenes_sr <- m[,1]
  write.csv(scenes_sr, file = "usgs_sr_scenes.csv") # sceneID 

