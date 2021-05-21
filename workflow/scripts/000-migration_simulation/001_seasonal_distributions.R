#
# Linking animal migration and ecosystem processes: data-driven simulation of seed dispersal by migratory herbivores
# Marius Somveille 1, 2 Diego Ellis Soto 2, 3, 4
#
# 1 Department of Biology, Colorado State University, CO 80523 USA
# 2 Max Planck - Yale Center for Biodiversity Movement and Global Change
# 3 Department of Ecology and Evolutionary Biology, Yale University, New Haven, Connecticut, USA
# 4 Center for Biodiversity and Global Change, Yale University, New Haven, CT, USA
#
# The aim of this manuscript is to replicate all analysis performed in Somveille & Ellis-Soto 
#
# [2] Create highland and lowland polygons based on tortoise habitat use
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 

# To do:
# Add colour scale from Marius for the plotting of elevation ####
# Rerun simulation code with alpha hulls
# Run Marius code with seed dispersal with adjusted lowlands or just run with chulls
## To do: #Get a shapefile of the GNP and agricultural area
# Get the proportion of seeds per dung pile and across elevation


library(adehabitatLT)
require(adehabitatHR)
require(plyr)
library(move)
library(lubridate)
library(circular)
library(fields)
library(mapdata)
library(scales)
require(tidyverse)
require(sf)
require(mapview)
require(ggmap)
require(rgeos)
require(mapview)
require(rgdal)
require(raster)
require(raster);require(sf);require(sp);require(rgdal);require(mapview);require(scales)


# Get elevation data
SRTM <- snakemake@input$elevation #raster('resources/Elevation/SRTM_Santa_Cruz.tif') # Digital elevation model of the Galapagos

# Get migration tracks
load(snakemake@input$tracks) #load('resources/Tracks/all_tortuga_clean.RData') # 9 individual tortoises
paste0('We use a total of ', length(unique(all_tortuga_clean@trackId)), ' tortoise individuals ')

# Only keep the tortoises with appropriate migration patterns
tortugas = unique(all_tortuga_clean@trackId)
tortugas.tracks <- all_tortuga_clean[[tortugas[c(1,3,4,6,8)]]]
tortugas.tracks.df <- as.data.frame(tortugas.tracks)

dates <- as.Date(all_tortuga_clean$timestamp)
all_tortuga_clean$jday <- yday(as.Date(all_tortuga_clean$timestamp))
all_tortuga_clean$month <- month(all_tortuga_clean$timestamp)
all_tortuga_clean$individual.local.identifier <- all_tortuga_clean@trackId
all_tortuga_clean_df <- data.frame(all_tortuga_clean)

(e<-bbox(extent(all_tortuga_clean)*2))
m <- get_map(e, zoom=8, source="google", maptype="terrain") # Zoom could be 10
La_reserva <- ggmap(m)+
  geom_path(data=all_tortuga_clean_df,
            aes(x=coords.x1,
                y=coords.x2,
                colour=individual.local.identifier))

# Create the utilization distribution of tortoises in lowland and highland, respectively.
# Based on this, we draw a polygon in Google Earth Pro, store it as a KML and convert it as a shapefile for further analysis that use highland and lowland utilization distributions as starting and ending point of simulated tracks, respectively.
# tortugas_sp <- as(tortugas_somveille_ellis_soto, 'SpatialPointsDataFrame')
tortugas_sp <- as(all_tortuga_clean, 'SpatialPointsDataFrame')

tortugas_sp$elevation <- raster::extract(SRTM, tortugas_sp) # Extract elevation
tortugas_sp$month <- month(tortugas_sp$timestamp)

tortugas_sp_highland <- tortugas_sp[tortugas_sp$month > 8 | tortugas_sp$month < 3,]
tortugas_sp_highland <- tortugas_sp_highland[tortugas_sp_highland$elevation > 200,]

tortugas_sp_lowland <- tortugas_sp[tortugas_sp$month < 8 | tortugas_sp$month > 2,]
tortugas_sp_lowland <- tortugas_sp_lowland[tortugas_sp_lowland$elevation < 50,]

lowland_kernelUD <- kernelUD(tortugas_sp_lowland,h = "href") # , same4all=TRUE)
lowland_kernelArea <- kernel.area(lowland_kernelUD, percent = 90,
                                  unin = "km", unout = "ha")
lowland_homeranges <- getverticeshr(lowland_kernelUD)

highland_kernelUD <- kernelUD(tortugas_sp_highland,h = "href") # , same4all=TRUE)
highland_kernelArea <- kernel.area(highland_kernelUD, percent = 90,
                                   unin = "km", unout = "ha")
highland_homeranges <- getverticeshr(highland_kernelUD)

# Calculate a geometric convex hull around the kernel utilization distributions
lowland_hull=gConvexHull(lowland_homeranges)
highland_hull=gConvexHull(highland_homeranges)

lowland_hull_pts <- c(lowland_hull, lowland_hull[1])
highland_hull_pts <- c(highland_hull, highland_hull[1])


# To do: Draw a mcp around a kernel UD?
SRTM_LR <- crop(SRTM, extent(-90.5, -0.75, -90.35, -0.60)) # Hone into the study site
plot(SRTM_LR, main = 'Kernel UD for the Lowlands')
# plot(tortugas_sp_lowland, xlab = NA, ylab = NA, add=T)
points(tortugas_sp_lowland, pch = 16, cex = 0.1, col = "#A020F050")
plot(getverticeshr(lowland_kernelUD, percent = 90), lwd = 2, col = alpha("firebrick", 0.3), add=T)
plot(getverticeshr(lowland_kernelUD, percent = 70), lwd = 2, col = alpha("firebrick", 0.5), add=T)
plot(getverticeshr(lowland_kernelUD, percent = 50), lwd = 2, col = alpha("firebrick", 0.5), add=T)
plot(mcp(tortugas_sp_lowland), add=T)
plot(lowland_hull, add=T)

points(tortugas_sp_highland, pch = 16, cex = 0.1, col = "#A020F050")
plot(getverticeshr(highland_kernelUD, percent = 90), lwd = 2, col = alpha("steelblue", 0.3), add=T)
plot(getverticeshr(highland_kernelUD, percent = 70), lwd = 2, col = alpha("steelblue", 0.5), add=T)
plot(getverticeshr(highland_kernelUD, percent = 50), lwd = 2, col = alpha("steelblue", 0.5), add=T)
plot(highland_hull, add=T)
plot(mcp(tortugas_sp_highland), add=T)

# Save the mcp

#writeOGR(mcp(tortugas_sp_highland), dsn = "results/output/","mcp_highland_2021", "ESRI Shapefile")
#writeOGR(mcp(tortugas_sp_lowland), dsn = "results/output/","mcp_lowland_2021", "ESRI Shapefile")


# Save the convex hull 
      
shapefile(highland_hull, strsplit(snakemake@output$highland, ".")[[1]][1], "ESRI Shapefile")
shapefile(lowland_hull, strsplit(snakemake@output$lowland, ".")[[1]][1], "ESRI Shapefile")
#shapefile(highland_hull, dsn = "results/output/","chull_highland_2021", "ESRI Shapefile")
#shapefile(lowland_hull, dsn = "results/output/","chull_lowland_2021", "ESRI Shapefile")


