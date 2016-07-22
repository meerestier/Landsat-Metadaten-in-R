# Script to visualize the available Landsat Scenes for certain
# WRS-2 Path and Row pairs;
# Developed by Ph. Gärtner (gaertner.p@gmail.com), in Berlin, Germany, 19th of July 2016.
# Extended by Lars Schulz (lars@larsschulz.info), 16-07-22
# Inputs:  Collection of Metadata provided as .csv dataset via
#  --- http://landsat.usgs.gov/metadatalist.php; 
# Output: A ggplot figure day of the year against sensor availability

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

# Pfad bitte aendern
setwd('data/')
getwd()

# Functions ---- 
get_metadata_csv <- function (csv, path.min, path.max, row.min, row.max) {
  
  x <- read_csv(csv)
  
  meta.sel <- subset(x, 
                     path >= path.min & path <= path.max & row >= row.min & row <= row.max, 
                     select=c(sensor, acquisitionDate, path, row, 
                              sceneCenterLatitude, sceneCenterLongitude, 
                              cloudCover, cloudCoverFull)) 
  
  #meta.sel$cloudfree <- as.numeric(meta.sel$cloudCoverFull)
  
  meta.sel$year <- format(meta.sel$acquisitionDate, "%Y")
  meta.sel$doy  <- yday(meta.sel$acquisitionDate)
  
  
  return(meta.sel)
}

get_metadata_xml <- function (xmlfile, path.min, path.max, row.min, row.max) {
  
  x <- xmlToDataFrame(xmlfile)
  #drop 1. line
  x <- x[-1,]

  
  meta.sel <- subset(x, 
                     path >= path.min & path <= path.max & row >= row.min & row <= row.max, 
                     select=c(sensor, acquisitionDate, path, row, 
                              sceneCenterLatitude, sceneCenterLongitude, 
                              cloudCover, cloudCoverFull)) 
  
  #meta.sel$cloudfree <- as.numeric(meta.sel$cloudCoverFull)
  
  meta.sel$year <- format(meta.sel$acquisitionDate, "%Y")
  meta.sel$doy  <- yday(meta.sel$acquisitionDate)
  
  return(meta.sel)
}


# Read data ----

# CHECK XML files in directory
list.files(getwd(), pattern = "xml")


# read xml
# metadata_landsat8.xml <- get_metadata_xml('metadata_landsat8.xml', 176, 176, 36, 36)

# read csv
# metadata_landsat8.sel <- get_metadata_csv('metadata_landsat8.csv', 176, 176, 36, 36)

# loop csv files in directory
files_csv <- list.files(getwd(), pattern = "csv")
count <- length(files_csv)
for (i in 1:count){
  file <- files_csv[i]
  assign(file, get_metadata_csv(file, 176, 176, 36, 36))
  print(file)
}

# esa data is in another format
esa_data <- read_csv('esa/esa_data.csv')

attach(esa_data)

esa_data_remapped <- rename(esa_data, c("Sensor"="sensor"
                   , "Start"="acquisitionDate"
                   , "Track"="path"
                   , "Frame"="row"
                   , "CloudPercentage"="cloudCoverFull"
))

# adding columns
# TOODO split SCENE_CENTER
esa_data_remapped$sceneCenterLatitude <- SCENE_CENTER
esa_data_remapped$sceneCenterLongitude <- SCENE_CENTER
esa_data_remapped$cloudCover <- as.integer(esa_data_remapped$cloudCoverFull / 10)

esa_data_remapped <- subset(esa_data_remapped, 
                   select=c(sensor, acquisitionDate, path, row, 
                            sceneCenterLatitude, sceneCenterLongitude, 
                            cloudCover, cloudCoverFull)) 

esa_data_remapped$year <- format(esa_data_remapped$acquisitionDate, "%Y")
esa_data_remapped$doy  <- yday(esa_data_remapped$acquisitionDate)


# save as csv
write.csv(esa_data_remapped, file = "esa/esa_data_remapped.csv")


# Combine sources ----
# TODO: Parameterisieren
m <- rbind(metadata_landsat1_3.csv,
           metadata_landsat4_5_TM.csv,
           metadata_landsat7_slcoff.csv,
           metadata_landsat7_slcon.csv,
           metadata_landsat8.csv,
           esa_data_remapped           
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


summary(m)
unique(m$sensor)
(size <- count(m$sensor))
size$bite <- c(0.450, 0.450, 2.000, 2.000) # 0.45 GB for ETM scene, 2.0 GB for L8 scene
(size$GB   <- size$freq * size$bite)
size$label <- paste(size[,1], size[,4], sep=" (")
size$label <- paste(size[,5], "GB)", sep=" ")

sum(size$GB)


p <- ggplot(m, aes(x = doy, y = year, size=cloudCoverFull)) # smaller dots for greater cloud Cover
p <- p + scale_shape_identity() + facet_grid(row ~ path)
p <- p + geom_point(alpha = 8/10, aes(shape = 16, colour=sensor)) + theme_bw()
p <- p + scale_y_discrete(name= "Year", breaks=seq(1970, 2020, 5))
p <- p + scale_x_discrete(name= "Day of Year", breaks= seq(0, 365, 100))
p <- p + scale_size_continuous(name="Cloud Cover [%]")
p <- p + scale_color_discrete(name="Sensor Type", labels=c(size$label)) # add GB to the legend
p <- p + theme(#legend.justification=c(1,0), 
               #legend.position=c(1,0),
               legend.position="bottom",
               legend.text = element_text(size= 11),
               legend.title = element_text(size= 12),       
               legend.key = element_blank())
p <- p + guides(colour = guide_legend(override.aes = list(size=5)))
p

# Output files ----

# save as csv
write.csv(m, file = "output/output_combined.csv")
write.csv(summary(m), file = "output/output_summary.csv")

# save an image
dev.copy(png,'output/output_combined.png')

