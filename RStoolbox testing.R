# Script to visualize the available Landsat Scenes for certain
# WRS-2 Path and Row pairs;
# Developed by Ph. Gärtner (gaertner.p@gmail.com), in Berlin, Germany, 19th of July 2016.
# Extended by Lars Schulz (lars@larsschulz.info), 16-07-22
# Inputs:  Collection of Metadata provided as .csv dataset via
#  --- http://landsat.usgs.gov/metadatalist.php; 
# Output: A ggplot figure day of the year against sensor availability

# Cyprus covers path 176 til 176 on row 36

# Installations ----

# install.packages("RStoolbox")

# Activate ----
library(RStoolbox)

