##  Simulating stochastic individual movements between highland and lowland using the eRTG algorithm  ##

library(tidyverse)
library(move)
library(lubridate)

# Get environmental and distribution data
galapagos_hexagons_data <- read.csv(snakemake@input$data, sep=";")
#galapagos_hexagons_data <- read.csv("results/output/000/galapagos_hexagons_data.csv", sep=";")

highland_coords <- galapagos_hexagons_data %>%
  dplyr::filter(Highland == 1) %>%
  dplyr::select(Longitude, Latitude) %>%
  as.matrix()
lowland_coords <- galapagos_hexagons_data %>%
  dplyr::filter(Lowland == 1) %>%
  dplyr::select(Longitude, Latitude) %>%
  as.matrix()

# Import ORSIM output
ORSIM_flow <- read.csv(snakemake@input$connectivity, header=F)
#ORSIM_flow <- read.csv("results/output/000/ORSIMresults.csv", header=F)
ORSIM_flow <- ORSIM_flow[which(galapagos_hexagons_data$Highland == 1),][,which(galapagos_hexagons_data$Lowland == 1)]
ORSIM_flow <- round(ORSIM_flow * 1000)

links <- which(ORSIM_flow > 1, arr.ind=T)
links <- links %>%
  as.data.frame() %>%
  add_column(inds = ORSIM_flow[links])
migra_links <- data.frame(
  x1 = rep(highland_coords[links$row,1], links$inds), 
  x2 = rep(lowland_coords[links$col,1], links$inds), 
  y1 = rep(highland_coords[links$row,2], links$inds), 
  y2 = rep(lowland_coords[links$row,2], links$inds)
)

# Get eRTG source code
source(snakemake@input$ertg)
#source("resources/eRTG/eRTG_V1.r")

# Get tortoise downslope migrations obtained from tracking data
load(snakemake@input$obs_migrations)
#load('results/output/000/downslope_migration.RData')
down_migr <- downslope_migration %>%
  as_tibble() %>%
  separate(timestamp,c('year','month','day','time'),'-') %>%
  unite("ID_Year", individual.local.identifier, year, sep="_")

# Estimate the functions required for simulating trajectories from tracking data (running time: ~15min)
movement.length <- vector()
qSimu <- list()
movement.densities <- list()
movements <- list()
for(i in 1:length(unique(down_migr$ID_Year))){
  movement <- down_migr[which(down_migr$ID_Year == unique(down_migr$ID_Year)[i]),]
  movements[[i]] <- move(x = movement$coords.x1,
                         y = movement$coords.x2, 
                         time = as.POSIXct(movement$timestamps, format="%Y-%m-%d %H:%M:%S", tz="UTC"), 
                         data = as.data.frame(movement), 
                         proj=CRS("+proj=aeqd")
  )
  movements[[i]] <- movements[[i]][!duplicated(round_date(movements[[i]]@timestamps,"24 hour"))] # Sample only one location per day
  movement.length[i] <- length(movements[[i]])
  # Compute step lengths and turning angles from data
  stepLength <- distance(movements[[i]])[-1]
  turningAngle <- wrap(diff(atan2(diff(coordinates(movements[[i]])[,2]), diff(coordinates(movements[[i]])[,1]))))
  deltaStep <- diff(stepLength)
  deltaTurn <- wrap(diff(turningAngle))
  # Create the density object that contains the information to preserve
  movement.densities[[i]] <- get.densities(stepLength, turningAngle, deltaStep, deltaTurn)
  # create an unconditional walk that has a given start but undefined end point
  # we choose this one to be much longer, because we need this to inform us later
  # about the relationship between dislocation as a function of steps taken.
  # This relationship will be required to model the conditional random walk
  RT <- simm.uncond(10000, 
                    start = head(coordinates(movements[[i]]),1), 
                    a0 = atan2((tail(coordinates(movements[[i]]),1)[2] - head(coordinates(movements[[i]]),1)[2]), (tail(coordinates(movements[[i]]),1)[1] - head(coordinates(movements[[i]]),1)[1])), 
                    densities = movement.densities[[i]]
  )
  # calculate the process specific dislocation as a function of number of steps.
  qSimu[[i]] <- qProb(RT, length(movements[[i]]))
}


# Simulate the migratory movements of tortoises (running time: 1h)
simu_starts <- migra_links %>%
  dplyr::select(x1, y1) %>%
  as.matrix()
simu_destinations <- migra_links %>%
  dplyr::select(x2, y2) %>%
  as.matrix()
simulated.movements <- list()
run.length <- vector()
for(i in 1:dim(simu_starts)[1]){
  
  simu.start = simu_starts[i,]
  simu.destination = simu_destinations[i,]
  
  simulated.mov <- NULL
  run.attempts = 0
  while(length(simulated.mov) == 0 & run.attempts < 190){
    
    selected.movement <- order(movement.length)[1]
    if(run.attempts>10 & run.attempts<=20){
      selected.movement <- order(movement.length)[2]
    }
    if(run.attempts>20 & run.attempts<=30){
      selected.movement <- order(movement.length)[3]
    }
    if(run.attempts>30 & run.attempts<=40){
      selected.movement <- order(movement.length)[4]
    }
    if(run.attempts>40 & run.attempts<=50){
      selected.movement <- order(movement.length)[5]
    }
    if(run.attempts>50 & run.attempts<=60){
      selected.movement <- order(movement.length)[6]
    }
    if(run.attempts>60 & run.attempts<=70){
      selected.movement <- order(movement.length)[7]
    }
    if(run.attempts>70 & run.attempts<=80){
      selected.movement <- order(movement.length)[8]
    }
    if(run.attempts>80 & run.attempts<=90){
      selected.movement <- order(movement.length)[9]
    }
    if(run.attempts>90 & run.attempts<=100){
      selected.movement <- order(movement.length)[10]
    }
    if(run.attempts>100 & run.attempts<=110){
      selected.movement <- order(movement.length)[11]
    }
    if(run.attempts>110 & run.attempts<=120){
      selected.movement <- order(movement.length)[12]
    }
    if(run.attempts>120 & run.attempts<=130){
      selected.movement <- order(movement.length)[13]
    }
    if(run.attempts>130 & run.attempts<=140){
      selected.movement <- order(movement.length)[14]
    }
    if(run.attempts>140 & run.attempts<=150){
      selected.movement <- order(movement.length)[15]
    }	
    if(run.attempts>150 & run.attempts<=160){
      selected.movement <- order(movement.length)[16]
    }	
    if(run.attempts>160 & run.attempts<=170){
      selected.movement <- order(movement.length)[17]
    }	
    if(run.attempts>170 & run.attempts<=180){
      selected.movement <- order(movement.length)[18]
    }	
    if(run.attempts>180 & run.attempts<=190){
      selected.movement <- order(movement.length)[19]
    }	
    
    t0 <- Sys.time()
    simulated.mov <- simm.condCor(movement.length[selected.movement], 
                                  start = simu.start, 
                                  end = simu.destination, 
                                  a0 = atan2((simu.destination[2] - simu.start[2]),(simu.destination[1] - simu.start[1])), 
                                  densities = movement.densities[[selected.movement]], 
                                  qProbs = qSimu[[selected.movement]], 
                                  BG = NULL, 
                                  extractMethod = "simple", 
                                  plot = F) 
    difftime(Sys.time(), t0, units="secs")
    run.attempts = run.attempts + 1
  }
  simulated.movements[[i]] <- simulated.mov
  run.length[i] <- run.attempts
  print(i)
}

save(simulated.movements, file=snakemake@output$sim_migrations)
#save(simulated.movements, file="results/output/000/simulated_movements.RData")

