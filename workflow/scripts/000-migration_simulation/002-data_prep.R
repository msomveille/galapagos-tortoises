##  Data preparation  ##

library(tidyverse)
library(raster)
library(rgdal)
library(dggridR)
library(rgeos)
library(fields)

# Get environmental data
elevation <- raster(snakemake@input$elevation)
NDVI_highland_season <- raster(snakemake@input$NDVI_highland)
NDVI_lowland_season <- raster(snakemake@input$NDVI_lowland)
extent(NDVI_highland_season) <- extent(elevation)
extent(NDVI_lowland_season) <- extent(elevation)

# Get seasonal distributions
tortoises.highland <- readOGR(snakemake@input$highland, verbose = FALSE)
tortoises.lowland <- readOGR(snakemake@input$lowland, verbose = FALSE)
#tortoises.highland <- readOGR(
#  dsn = paste(strsplit(snakemake@input$highland, "/")[[1]][1], strsplit(snakemake@input$highland, "/")[[1]][2], strsplit(snakemake@input$highland, "/")[[1]][3], sep = "/"),
#  layer = strsplit(strsplit(snakemake@input$highland, "/")[[1]][4], ".")[[1]][1], 
#  verbose = FALSE)
#tortoises.lowland <- readOGR(
#  dsn = paste(strsplit(snakemake@input$lowland, "/")[[1]][1], strsplit(snakemake@input$lowland, "/")[[1]][2], sep = "/"),
#  layer = strsplit(strsplit(snakemake@input$lowland, "/")[[1]][3], ".")[[1]][1], 
#  verbose = FALSE)
#tortoises.highland <- readOGR("results/output/000", "chull_highland", verbose=F) 
#tortoises.lowland <- readOGR("results/output/000", "chull_lowland", verbose=F) 

# Construct a grid of hexagons over Santa Cruz Island
hexgrid <- dgconstruct(projection="ISEA", topology="HEXAGON", res=16, metric=T)
hexgrid_center <- dgSEQNUM_to_GEO(hexgrid, 200000000:250000000)
hex_sel <- which(hexgrid_center[[1]] > extent(elevation)[1] & hexgrid_center[[1]] < extent(elevation)[2] & hexgrid_center[[2]] > extent(elevation)[3] & hexgrid_center[[2]] < extent(elevation)[4])
hexgrid.galapagos <- dgcellstogrid(hexgrid, 200000000+hex_sel, frame=F,wrapcells=TRUE)
elevation.hex <- raster::extract(elevation, hexgrid.galapagos, mean, na.rm=T)
hexgrid.galapagos <- hexgrid.galapagos[-which(elevation.hex == "NaN" | is.na(elevation.hex)==T)] # Remove hexagons in water
hexgrid.galapagos.centroids <- matrix(unlist(lapply(hexgrid.galapagos@polygons, function(x) x@labpt)), byrow=T, ncol=2)
elevation.hex <- elevation.hex[-which(elevation.hex == "NaN" | is.na(elevation.hex)==T),]

# Extract environmental data and species distribution onto the hexagonal grid
NDVI_highlands.hex <- raster::extract(NDVI_highland_season, hexgrid.galapagos, mean, na.rm=T)
NDVI_lowlands.hex <- raster::extract(NDVI_lowland_season, hexgrid.galapagos, mean, na.rm=T)
tortoises.highland2 <- spTransform(tortoises.highland, proj4string(hexgrid.galapagos))
tortoises.lowland2 <- spTransform(tortoises.lowland, proj4string(hexgrid.galapagos))
highland.hex <- gIntersects(tortoises.highland2, hexgrid.galapagos, byid=T)
lowland.hex <- gIntersects(tortoises.lowland2, hexgrid.galapagos, byid=T)
galapagos_hexagons_data <- data.frame(Longitude = hexgrid.galapagos.centroids[,1],
                                      Latitude = hexgrid.galapagos.centroids[,2],
                                      Elevation = elevation.hex, 
                                      NDVIhighland = NDVI_highlands.hex, 
                                      NDVIlowland = NDVI_lowlands.hex,
                                      Highland = ifelse(highland.hex[,1] == TRUE, 1, 0),
                                      Lowland = ifelse(lowland.hex[,1] == TRUE, 1, 0))
write.table(galapagos_hexagons_data, snakemake@output$data, row.names=F, sep=";")
#write.table(galapagos_hexagons_data, "results/output/000/galapagos_hexagons_data.csv", row.names=F, sep=";")

# Compute a distance matrix based on the great circle distance (in km) separating hexagons occupied in highland and lowland – which is used as input in ORSIM
distance.matrix <- galapagos_hexagons_data %>%
  dplyr::select(Longitude, Latitude) %>%
  as.matrix() %>%
  rdist.earth(miles=F)
write.table(distance.matrix, snakemake@output$distMat, row.names=F, col.names=F, sep=";")
#write.table(distance.matrix, "results/output/000/distanceMatrix.csv", row.names=F, col.names=F, sep=";")

