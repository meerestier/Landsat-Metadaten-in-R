# Script to visualize the available Landsat Scenes for certain
# WRS-2 Path and Row pairs;
# Developed by Ph. G?rtner (gaertner.p@gmail.com)
# In Berlin, Germany, 19th of July 2016.
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

# CHECK XML files in directory
list.files(getwd(), pattern = "xml")


# read xml
metadata_landsat8.xml.sel <- get_metadata_xml('metadata_landsat8.xml', 176, 176, 36, 36)
data <- xmlToDataFrame("metadata-landsat8.xml")

# read csv
#metadata_landsat8.sel <- get_metadata_csv('metadata_landsat8.csv', 176, 176, 36, 36)

# loop csv files in directory
files_csv <- list.files(getwd(), pattern = "csv")
count <- length(files_csv)
for (i in 1:count){
  file <- files_csv[i]
  assign(file, get_metadata_csv(file, 176, 176, 36, 36))
  print(file)
}






m <- rbind(L8.sel,
           ETM_off.sel,
           ETM_on.sel)

# reorder path from high to low
neworder <- c(177:175)

library("plyr")
m <- arrange(transform(m, path=factor(path,levels=neworder)),path)


# removing scenes
m <- subset(m, sensor !=  'TIRS') # remove scenes from 'TIRS' sensor
m <- subset(m, cloudCover <  8) # remove scenes with a higher cloud cover then 80%


summary(m)
unique(m$sensor)
(size <- count(m$sensor))
size$bite <- c(0.450, 0.450, 2.000, 2.000) # 0.45 GB for ETM scene, 2.0 GB for L8 scene
(size$GB   <- size$freq * size$bite)
size$label <- paste(size[,1], size[,4], sep=" (")
size$label <- paste(size[,5], "GB)", sep=" ")

sum(size$GB)


library(ggplot2)
p <- ggplot(m, aes(x = doy, y = year, size=cloudCover)) 
p <- p + scale_shape_identity() + facet_grid(row ~ path)
p <- p + geom_point(alpha = 8/10, aes(shape = 15, colour=sensor)) + theme_bw()
p <- p + scale_y_discrete(name= "Year", breaks=seq(1999, 2015, 4))
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


