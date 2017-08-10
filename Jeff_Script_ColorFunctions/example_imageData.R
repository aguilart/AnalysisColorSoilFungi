# example script for extracting colony radius and colour information from .jpg images - written by Jeff Powell (jeffpowell2@gmail.com)
# output for 'culture.data()' is data.frame containing colony radius and an estimate of bias (directional growth), average values for RGB channels, standard deviations for RGB channels, and hex code for average colour
# output for 'colour.data()' is data.frame average values for RGB channels, standard deviations for RGB channels, and hex code for average colour
# for large images (> 5mb), I ran R from a terminal window because lag was a problem in Rstudio

# clear workspace
rm(list=ls())

# load libraries
library(jpeg)
library(doBy)
library(colorspace)

# load functions
source('cultureData.R')
source('colourData.R')

# open example .jpg file and plot image
par(mar=rep(0, 4))

image <- readJPEG('test.jpg')
plot(c(0, dim(image)[1]), c(0, dim(image)[2]), type='n', xlab='', ylab='')
rasterImage(image, 0, 0, dim(image)[1], dim(image)[2], interpolate=F)

# select four corners of image
img <- locator(4)

# culture.data() - get culture data from image, arguments are:
# ncultures: number of cultures in this image to get data from
# npoints: number of points to select around colony, plate from which to obtain data
# plat.mm: plate diameter in mm
data1 <- culture.data(ncultures=2, npoints=8, plat.mm=90)
data1

# colour.data() - if only interested in getting colour data from image, arguments are:
# npoints: number of points in this image to get data from
data2 <- colour.data(npoints=4)
data2

# calculate 'black' channel
data2$K.mean <- 1 - apply(as.matrix(data2[, c('R.mean', 'G.mean', 'B.mean')])/255, 1, max)
data2
in