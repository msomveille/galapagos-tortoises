# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
#
# Linking animal migration and ecosystem processes: data-driven simulation of seed dispersal by migratory herbivores
# Marius Somveille 1, 2 Diego Ellis Soto 2, 3, 4
#
# 1 Department of Biology, Colorado State University, CO 80523 USA
# 2 Max Planck - Yale Center for Biodiversity Movement and Global Change
# 3 Department of Ecology and Evolutionary Biology, Yale University, New Haven, Connecticut, USA
#
# 4 Center for Biodiversity and Global Change, Yale University, New Haven, CT, USA
#
# The aim of this manuscript is to replicate all analysis performed in Somveille & Ellis-Soto 
#
# A total of 9 tortoises are used to parameterize the tracking dataset:
#
# [1] Segment tortoise downslope migration by individual, and merge them all together at the end for further steps ####

# Using the locator function, it is possible to accurately manually disentangle start and ending point of tortoise downslope migration
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

library(adehabitatLT)
library(move)
library(lubridate)
library(circular)
library(fields)
library(mapdata)
library(scales)
require(tidyverse)

# Get the gps tracking data
load("resources/Tracks/all_tortugas_clean.RData")

# Only keep the tortoises with appropriate migration patterns
tortugas = unique(all_tortuga_clean@trackId)
tortugas.tracks <- all_tortuga_clean[[tortugas[c(1,3,4,6,8)]]]

par(mfrow=c(1,1))

## Tortoise 1: Steve Devine

tor = tortugas.tracks[['Steve.Devine']]
torNSD <- (spDistsN1(coordinates(tor), coordinates(tor)[1,],longlat=T))^2

plot(tor$timestamp, torNSD, type="l",
     xlab="Time", ylab="Net square distance (Km²)", main="All data")
start_end_downslope <- locator(16) # Subset the data by clicking on the graph

vec = as.POSIXct(start_end_downslope$x, tz = 'UTC', origin= '1970-01-01' )
start = vec[seq(1, length(vec), by = 2)] 
end = vec[seq(2, length(vec), by = 2)] 
timing_steve = data.frame(start_date = start, end_date = end)

steve <- as.data.frame(tor)
lista <- rep( list(data.frame(matrix(ncol = ncol(steve), nrow = nrow(steve)))), nrow(timing_steve) ) 

for(i in 1:nrow(timing_steve)){
  int <- interval(timing_steve[i,1], timing_steve[i,2])
  lista[[i]] <- steve[steve$timestamp %within% int,]
}
steve_downslope_migration <- lista

steve_downslope_migration_df <-bind_rows(steve_downslope_migration, .id = "column_label")
plot(steve_downslope_migration_df$coords.x1, steve_downslope_migration_df$coords.x2)
steve_spdf <- SpatialPointsDataFrame(coords = steve_downslope_migration_df[,c('coords.x1','coords.x2')], data = steve_downslope_migration_df,proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Tortoise 2: Sepp
tor = tortugas_somveille_ellis_soto[['Sepp']]
torNSD <- (spDistsN1(coordinates(tor), coordinates(tor)[1,],longlat=T))^2
par(mfrow=c(1,1))
plot(tor$timestamp, torNSD, type="l",xlab="Time", ylab="Net square distance (Km²)", main="All data")
start_end_downslope <- locator(8)

vec = as.POSIXct(start_end_downslope$x, tz = 'UTC', origin= '1970-01-01' )
start = vec[seq(1, length(vec), by = 2)] 
end = vec[seq(2, length(vec), by = 2)] 
timing_sepp = data.frame(start_date = start, end_date = end)

sepp <- as.data.frame(tor)
lista <- rep( list(data.frame(matrix(ncol = ncol(sepp), nrow = nrow(sepp)))), nrow(timing_sepp) ) 

for(i in 1:nrow(timing_sepp)){
  int <- interval(timing_sepp[i,1], timing_sepp[i,2])
  lista[[i]] <- sepp[sepp$timestamp %within% int,]
}
sepp_downslope_migration <- lista

sepp_downslope_migration_df <-bind_rows(sepp_downslope_migration, .id = "column_label")
plot(sepp_downslope_migration_df$coords.x1, sepp_downslope_migration_df$coords.x2)
sepp_spdf <- SpatialPointsDataFrame(coords = sepp_downslope_migration_df[,c('coords.x1','coords.x2')], data = sepp_downslope_migration_df,
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Tortoise 3: Sebastian
tor = tortugas_somveille_ellis_soto[['Sebastian']]
torNSD <- (spDistsN1(coordinates(tor), coordinates(tor)[1,],longlat=T))^2
par(mfrow=c(1,1))
plot(tor$timestamp, torNSD, type="l",xlab="Time", ylab="Net square distance (Km²)", main="All data")

start_end_downslope <- locator(14)

vec = as.POSIXct(start_end_downslope$x, tz = 'UTC', origin= '1970-01-01' )
start = vec[seq(1, length(vec), by = 2)] 
end = vec[seq(2, length(vec), by = 2)] 
timing_sebastian = data.frame(start_date = start, end_date = end)

sebastian <- as.data.frame(tor)
lista <- rep( list(data.frame(matrix(ncol = ncol(sebastian), nrow = nrow(sebastian)))), nrow(timing_sebastian) ) 

for(i in 1:nrow(timing_sebastian)){
  int <- interval(timing_sebastian[i,1], timing_sebastian[i,2])
  lista[[i]] <- sebastian[sebastian$timestamp %within% int,]
}
sebas_downslope_migration <- lista

sebas_downslope_migration_df <-bind_rows(sebas_downslope_migration, .id = "column_label")
plot(sebas_downslope_migration_df$coords.x1, sebas_downslope_migration_df$coords.x2)
sebas_spdf <- SpatialPointsDataFrame(coords = sebas_downslope_migration_df[,c('coords.x1','coords.x2')], data = sebas_downslope_migration_df,
                                     proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# 6 Karlitos
tor = tortugas_somveille_ellis_soto[['Karlitos']]
torNSD <- (spDistsN1(coordinates(tor), coordinates(tor)[1,],longlat=T))^2
par(mfrow=c(1,1))
plot(tor$timestamp, torNSD, type="l",xlab="Time", ylab="Net square distance (Km²)", main="All data")
start_end_downslope <- locator(4)

vec = as.POSIXct(start_end_downslope$x, tz = 'UTC', origin= '1970-01-01' )
start = vec[seq(1, length(vec), by = 2)] 
end = vec[seq(2, length(vec), by = 2)] 
timing_karlitos = data.frame(start_date = start, end_date = end)

karlitos <- as.data.frame(tor)
lista <- rep( list(data.frame(matrix(ncol = ncol(karlitos), nrow = nrow(karlitos)))), nrow(timing_karlitos) ) 

for(i in 1:nrow(timing_karlitos)){
  int <- interval(timing_karlitos[i,1], timing_karlitos[i,2])
  lista[[i]] <- karlitos[karlitos$timestamp %within% int,]
}
karlitos_downslope_migration <- lista

karlitos_downslope_migration_df <-bind_rows(karlitos_downslope_migration, .id = "column_label")
plot(karlitos_downslope_migration_df$coords.x1, karlitos_downslope_migration_df$coords.x2)
karlitos_spdf <- SpatialPointsDataFrame(coords = karlitos_downslope_migration_df[,c('coords.x1','coords.x2')], data = karlitos_downslope_migration_df,
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# 8 George

tor = tortugas_somveille_ellis_soto[['George']]
torNSD <- (spDistsN1(coordinates(tor), coordinates(tor)[1,],longlat=T))^2
par(mfrow=c(1,1))
plot(tor$timestamp, torNSD, type="l",xlab="Time", ylab="Net square distance (Km²)", main="All data")
start_end_downslope <- locator(2)

vec = as.POSIXct(start_end_downslope$x, tz = 'UTC', origin= '1970-01-01' )
start = vec[seq(1, length(vec), by = 2)] 
end = vec[seq(2, length(vec), by = 2)] 
timing_george = data.frame(start_date = start, end_date = end)

george <- as.data.frame(tor)
lista <- rep( list(data.frame(matrix(ncol = ncol(george), nrow = nrow(george)))), nrow(timing_george) ) 

for(i in 1:nrow(timing_george)){
  int <- interval(timing_george[i,1], timing_george[i,2])
  lista[[i]] <- george[george$timestamp %within% int,]
}
george_downslope_migration <- lista
george_downslope_migration_df <-bind_rows(george_downslope_migration, .id = "column_label")
plot(george_downslope_migration_df$coords.x1, george_downslope_migration_df$coords.x2)
george_spdf <- SpatialPointsDataFrame(coords = george_downslope_migration_df[,c('coords.x1','coords.x2')], data = george_downslope_migration_df,
                                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Save all downslope tortoise migrations into an .Rdata frame
downslope_migration <- rbind(steve_spdf, sepp_spdf, sebas_spdf, karlitos_spdf, george_spdf)
save(downslope_migration, file = 'results/output/000/downslope_migration.Rdata')
