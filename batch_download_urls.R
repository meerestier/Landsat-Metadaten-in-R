# Script to batch download from a list of urls
# Developed by Lars Schulz (lars@larsschulz.info), 16-07-22, Berlin
# Input: List of urls
# Output: Downloaded files in folder
# Usecase: Cyprus Landsat scenes

# Installations ----

install.packages("RCurl")
install.packages("rgdal")

# Activate ----
library(RCurl)
library(rgdal)

# Configure ----

# Pfad bitte aendern
setwd('data/')
getwd()


# Script ----

indir = "downloads/" # Data will be downloaded to this directory
url.list = "results.csv" # Name of file generated from reverb website

file.list = read.csv(paste(indir,url.list,sep=""))
food = unlist(file.list)

for (j in 1:length(food)){
  wgeturl = as.character(file.list[j,])
  foo = strsplit(as.character(file.list[j,]),"/")[[1]][9]
  dest.fname = strsplit(foo,"\\?")[[1]][1]
  foo = download.file(wgeturl, destfile=paste(indir,dest.fname,sep=""), mode="wb", method='internal')
}
