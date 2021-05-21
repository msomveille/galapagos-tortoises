##  Simulating the spread of seeds along simulated migration tracks  ##

library(rgdal)
library(truncnorm)
library(raster)
library(sp)
library(tidyverse)
library(rgeos)

# Get simulated migratory movements
load(snakemake@input$sim_migrations)
#load("results/output/000/simulated_movements.RData")

# Guava empirical distribution
elevation <- raster(snakemake@input$elevation)
#elevation <- raster("resources/Elevation/SRTM_Santa_Cruz.tif")
Guava.distrib <- readOGR(snakemake@input$guava_distrib, verbose = FALSE)
#Guava.distrib <- readOGR("resources/LandCover", "Galapagos_Agroecosystems_LandCover2018", verbose=F)
Guava.distrib <- Guava.distrib[Guava.distrib$Level_5 == 'Psidium-Guava',]
Guava.distrib <- spTransform(Guava.distrib, proj4string(elevation))
Guava.distrib <- Guava.distrib[4,]
Guava.distrib <- gBuffer(Guava.distrib, byid=TRUE, width=0)

tortoises.highland <- readOGR(snakemake@input$highland, verbose = FALSE)
#tortoises.highland <- readOGR("results/output/000", "chull_highland", verbose=F) 

fract.guava.highland <- round(area(gIntersection(tortoises.highland, Guava.distrib, byid = T)) / area(tortoises.highland), 2)

# Guava sdm
SDM.guava <- raster(snakemake@input$sdm_guava)
#SDM.guava <- raster("resources/SDM_guava/Current/psidium_current_cut.gri")
proj4string(SDM.guava) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
values(SDM.guava)[which(values(SDM.guava)<0.524)] <- 0
values(SDM.guava)[which(values(SDM.guava)>0)] <- 1

# Simulate seed dispersal
daily.feeding.frequency <- as.numeric(snakemake@params$freq)
excreting.guava <- list()
guava.seeds <- list()
guava.germination <- list()
for(k in 1:length(simulated.movements)){
  
  # Get individual simulated migratory movement
  simulated.move <- simulated.movements[[k]][,1:2]
  
  if(length(simulated.move) > 0){
    sim.moves <- SpatialPoints(simulated.move)
    proj4string(sim.moves) <- proj4string(Guava.distrib)
    sim.moves <- SpatialPointsDataFrame(sim.moves, data.frame(ID=1:length(sim.moves)))
    
    # Compute whether each daily location along the migratory movement is somewhere that is suitable for guava germination
    SDM.guava.values <- raster::extract(SDM.guava, sim.moves)
    
    # Compute whether there is guava or not at each daily location along the migratory movement
    guava.distrib.values <- over(sim.moves, Guava.distrib)
    guava.distrib.values <- guava.distrib.values[,7]
    guava.distrib.values[which(is.na(guava.distrib.values) == T)] <- 0
    guava.distrib.values[which(guava.distrib.values > 0)] <- 1

    # Days before the focal day during which the individual has eaten guava (up to 20 days)
    #days.with.guava <- rep(guava.distrib.values[1], 20*daily.feeding.frequency)
    #days.with.guava <- rep(1, 20*daily.feeding.frequency)
    days.with.guava <- sample(c(1,0), 20*daily.feeding.frequency, prob = c(fract.guava.highland, 1-fract.guava.highland), replace =T)
    excreting <- vector()
    seeds <- vector()
    germination <- vector()
    Proba.excreting.guava <- vector()
    for(i in 1:length(sim.moves)){
      # Excreting process based on a gut retention time function calibrated using empirical data
      if(sum(days.with.guava)>0){
        Proba.excreting.guava[i] <- sum(dtruncnorm(which(days.with.guava == 1)/daily.feeding.frequency, a=0,  mean = 7.5, sd = 2.16))/daily.feeding.frequency
        excreting[i] <- sample(c(1,0), 1, prob=c(Proba.excreting.guava[i], 1-Proba.excreting.guava[i]))
        #Proba.excreting.guava <- dtruncnorm(which(days.with.guava == 1)/daily.feeding.frequency, a=0,  mean = 7.5, sd = 2.16)
        #excreting.guava <- vector()
        #for(j in 1:length(Proba.excreting.guava)){
        #  excreting.guava[j] <- sample(c(1,0), 1, prob=c(Proba.excreting.guava[j], 1-Proba.excreting.guava[j]))
        #}
      }else{
        excreting[i] <- 0
      }
      
      # Number of guava seeds excreted based on a function calibrated using empirical data
      seeds[i] = excreting[i] * rtruncnorm(n=1, a=0,  mean = 1443, sd = 2057)
      
      # Guava germination
      germination[i] <- excreting[i] * SDM.guava.values[i]
      
      # Eating process
      days.with.guava <- c(guava.distrib.values[i], days.with.guava[1:(length(days.with.guava)-1)])	
    }
  }else{
    excreting <- NULL
    seeds <- NULL
    germination <- NULL
  }
  excreting.guava[[k]] <- excreting
  guava.seeds[[k]] <- seeds
  guava.germination[[k]] <- germination
}


# Combine all simulated movements and seeds dispersed together
moveLength <- unlist(lapply(guava.seeds, length))
moveID <- vector()
stepID <- vector()
for(i in 1:length(moveLength)){
  if(moveLength[i] > 0){
    moveID <- c(moveID, rep(i, moveLength[i]))
    stepID <- c(stepID, 1:moveLength[i])
  }
}
simulated.data <- data.frame(
  moveID = moveID,
  stepID = stepID,
  long = unlist(lapply(simulated.movements, function(x) x[,1])),
  lat = unlist(lapply(simulated.movements, function(x) x[,2])),
  seeds = unlist(guava.seeds),
  germinatedSeeds = unlist(guava.seeds) * unlist(guava.germination)
)
simulated.data <- simulated.data %>%
  dplyr::select(long, lat) %>%
  SpatialPointsDataFrame(
    data.frame(
      moveID = moveID,
      stepID = stepID,
      seeds = simulated.data$seeds, 
      seeds.germinated = simulated.data$germinatedSeeds
    ),
    proj4string = CRS(proj4string(Guava.distrib))
  )

assign(paste("simulated.data", daily.feeding.frequency, sep = "."), simulated.data) 

save(list = paste("simulated.data", daily.feeding.frequency, sep = "."), file = snakemake@output$sim_dispersal)
#save(simulated.data, file = "results/output/100/simulated_data_seeds_1.RData")



