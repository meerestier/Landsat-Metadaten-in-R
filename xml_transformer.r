# Script ------------------------------------------------------------------
# transform XML to other formats (eg. .csv)
# file: xml_transformer.r
# version: 0.1 
# date: 16-07-22
# contact: lars@larsschulz.info

#install
install.packages("XML")
library("XML")


#read XML
data <- xmlToDataFrame("data/metadata.xml")
