###  Plot figures  ###

library(raster)
library(RColorBrewer)
library(rgdal)
library(rworldmap)
library(rgeos)
library(sf)
library(gridExtra)
library(grid)
library(data.table)
library(plyr)
library(tidyverse)
library(ggpubr)

## Prepare data for plotting

# Colors based on elevation
elevation <- raster(snakemake@input$elevation)
#elevation <- raster("resources/Elevation/SRTM_Santa_Cruz.tif")
tib <- as.data.frame(rasterToPoints(elevation)) %>%
  as_tibble() %>%
  dplyr::rename(val = "SRTM_Santa_Cruz") %>%
  dplyr::mutate(
    forColor = case_when(
      val > 700 ~ "col8",
      val > 600 ~ "col7",
      val > 500 ~ "col6",
      val > 400 ~ "col5",
      val > 300 ~ "col4",
      val > 200 ~ "col3",
      val > 100 ~ "col2",
      val > 0     ~ "col1",
      TRUE        ~ "col0"
    )
  )

# Get seasonal distributions
tortoises.highland <- readOGR(snakemake@input$highland, verbose = FALSE)
tortoises.lowland <- readOGR(snakemake@input$lowland, verbose = FALSE)
#tortoises.highland <- readOGR("results/output/000", "chull_highland", verbose=F) 
#tortoises.lowland <- readOGR("results/output/000", "chull_lowland", verbose=F) 

# Get environmental and distribution data
galapagos_hexagons_data <- read.csv(snakemake@input$data, sep=";")
#galapagos_hexagons_data <- read.csv("results/output/000/galapagos_hexagons_data.csv", sep=";")

# Import ORSIM output
ORSIM_flow <- read.csv(snakemake@input$connectivity, header=F)
#ORSIM_flow <- read.csv("results/output/000/ORSIMresults.csv", header=F)
ORSIM_flow <- ORSIM_flow[which(galapagos_hexagons_data$Highland == 1),][,which(galapagos_hexagons_data$Lowland == 1)]
ORSIM_flow <- round(ORSIM_flow * 1000)

# Coordinates of migratory network
highland_coords <- galapagos_hexagons_data %>%
  dplyr::filter(Highland == 1) %>%
  dplyr::select(Longitude, Latitude) %>%
  as.matrix()
lowland_coords <- galapagos_hexagons_data %>%
  dplyr::filter(Lowland == 1) %>%
  dplyr::select(Longitude, Latitude) %>%
  as.matrix()
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

# Get simulated migratory movements
load(snakemake@input$sim_migrations)
#load("results/output/000/simulated_movements.RData")

# Get simulated dispersed seeds
sim_dispersal_paths <- snakemake@input$sim_dispersal
for(i in 1:length(sim_dispersal_paths)){
  load(sim_dispersal_paths[[i]])
}
#load("results/output/100/simulated_data_seeds_1.RData")

## Plot migratory network simulated by ORSIM

# Plot the background elevation raster
#theme_set(theme_classic())
#p1 <- ggplot() +
#  geom_raster(data = tib, aes(x = x, y = y, fill = forColor)) +
#  scale_x_continuous(limits = c(-90.55, -90.25)) +
#  scale_y_continuous(position = "left", limits = c(-0.80,-0.60)) +
#  scale_fill_manual(
#    values = brewer.pal(9, "Oranges"),
#    name = "Elevation (m)",
#    labels = c("< 0", "0–100", "100–200", "200–300", "300–400", "400–500", "500–600", "600–700", "> 700"),
#    guide = guide_legend(reverse=TRUE)
#  ) +
#  labs(x = "Longitude",
#       y = "Latitude")
# Plot the seasonal distributions
#p1 <- p1 +
#  geom_polygon(data = tortoises.highland, aes(x = long, y = lat, group = group), colour="grey47", fill="grey47") +
#  geom_polygon(data = tortoises.lowland, aes(x = long, y = lat, group = group), colour="grey47", fill="grey47")
# Plot the migratory connections
#p1 <- p1 +
#  geom_segment(data = migra_links, aes(x = x1, y = y1, xend = x2, yend = y2))
#p1


## Figure 2: Simulated tortoise migrations and resulting guava seed rain
jpeg(snakemake@output$fig_2, width=1200, height=600, quality = 100)
#jpeg("results/figures/Figure_2c.jpg", width=1200, height=600, quality = 100)

## (a) Simulated migration trajectories of tortoises

migra.trajectories <- data.frame()
for(i in 1:length(simulated.movements)){
  migra.trajectories <- rbind(migra.trajectories, cbind(rep(i, length(simulated.movements[[i]][,1])), simulated.movements[[i]][,1:2]))
}
migra.trajectories <- migra.trajectories %>%
  rename(Individual = "V1",
         Longitude = "x",
         Latitude = "y") %>%
  as_tibble()

# Plot the background elevation raster
theme_set(theme_void())
p1 <- ggplot() +
  geom_raster(data = tib, aes(x = x, y = y, fill = forColor)) +
  scale_x_continuous(limits = c(-90.55, -90.25)) +
  scale_y_continuous(position = "left", limits = c(-0.80,-0.60)) +
  scale_fill_manual(
    values = brewer.pal(9, "Oranges"),
    name = "Elevation (m)",
    labels = c("< 0", "0–100", "100–200", "200–300", "300–400", "400–500", "500–600", "600–700", "> 700"),
    guide = guide_legend(reverse=TRUE)
  ) +
  labs(x = "Longitude",
       y = "Latitude")
# Plot the seasonal distributions
p1 <- p1 +
  geom_polygon(data = tortoises.highland, aes(x = long, y = lat, group = group), colour="grey47", fill="grey47") #+
  #geom_polygon(data = tortoises.lowland, aes(x = long, y = lat, group = group), colour="grey47", fill="grey47")
# Plot the migratory trajectories 
p1 <- p1 + geom_path(data = migra.trajectories, aes(x = Longitude, y = Latitude, group = Individual), size = 0.1) +
  theme(legend.key.size = unit(0.7, "cm"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size=22)
  ) +
  labs(title = "(a) Simulated migratory movements")

## (b) Density of guava seeds dispersed by simulated migrating tortoises

# Define spatial grid
bb <- bbox(simulated.data.1)
cs <- c(0.001, 0.001) # cell size of 0.001°x0.001°
cc <- bb[, 1] + (cs/2)  # cell offset
cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
sp_grd <- SpatialGridDataFrame(grd, 
                               data = data.frame(id=1:prod(cd)), 
                               proj4string = CRS(proj4string(simulated.data.1))
)
sp_grd <- as(sp_grd, "SpatialPolygonsDataFrame")
sp_grd_raster <- rasterize(sp_grd, raster(grd))

# Compute the seed rain (number of seeds excreted by tortoises per grid cell during their downslope migration)
seed.rain <- rasterize(simulated.data.1, sp_grd_raster, field = simulated.data.1@data$seeds, fun = sum, na.rm=T)

# Compute the germinated seed rain (number of seeds excreted by tortoises per grid cell during their downslope migration that succesfully germinate)
seed.rain.germ <- rasterize(simulated.data.1, sp_grd_raster, field = simulated.data.1@data$seeds.germinated, fun = sum, na.rm=T)

# Color for plotting the seed rain
seedRain <- as.data.frame(rasterToPoints(seed.rain)) %>%
  as_tibble() %>%
  dplyr::mutate(
    forColor = case_when(
      layer > 12000 ~ "col7",
      layer > 10000 ~ "col6",
      layer > 8000 ~ "col5",
      layer > 6000 ~ "col4",
      layer > 4000 ~ "col3",
      layer > 2000 ~ "col2",
      layer > 0  ~ "col1",
      layer == 0 ~ "col0"
    )
  )

# Plot the background
theme_set(theme_void())
p2 <- ggplot() +
  geom_raster(data = tib, aes(x = x, y = y), fill = "grey75") +
  scale_x_continuous(limits = c(-90.55, -90.25)) +
  scale_y_continuous(position = "left", limits = c(-0.80,-0.60))
# Plot the seed rain 
p2 <- p2 +
  geom_raster(data = seedRain, aes(x = x, y = y, fill = forColor)) +
  scale_fill_manual(
    values = c("grey75", brewer.pal(7, "Reds")),
    name = "Number of seeds",
    labels = c("0", "1–2000", "2001–4000", "4001–6000", "6001–8000", "8001–10000", "10001–12000", "> 12000"),
    guide = guide_legend(reverse=TRUE)
  )
# Plot the seasonal distributions
p2 <- p2 +
  geom_polygon(data = tortoises.highland, aes(x = long, y = lat, group = group), colour="grey47", fill="grey47")
  #geom_polygon(data = tortoises.lowland, aes(x = long, y = lat, group = group), colour="black", fill=NA)
p2 <- p2 +
  theme(legend.key.size = unit(0.7, "cm"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size=22)
  ) +
  labs(title = "(b) Predicted seed rain")

gridExtra::grid.arrange(p1, p2, nrow = 1)

dev.off()  





## Figure 3: Model simulation captures empirical dispersal of guava seeds

# Get observed dung data with seeds
obs_dungs <- read.csv(snakemake@input$dungpiles) %>%
#obs_dungs <- read.csv("resources/Dung/collected_dungpiles.csv") %>%
  filter(Species == 'Psydium Guajava' | Species == 'Psidium guajava') %>%
  mutate(alt = as.integer(as.character(alt)),
         guava_presence = 1
  ) 
obs_dungs_sp <- SpatialPointsDataFrame(coords= obs_dungs[,c('Long', 'Lat')],
                                       data = obs_dungs,
                                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Add elevation value to simulated data
simulated.data.1$elevation <- raster::extract(elevation, simulated.data.1)
simulated.data.2$elevation <- raster::extract(elevation, simulated.data.2)
simulated.data.4$elevation <- raster::extract(elevation, simulated.data.4)

# Get landcover data
landcover <- readOGR(snakemake@input$landcover)
#landcover <- readOGR('resources/LandCover/benitez_landcover_Santa_Cruz.shp')

# Get shapefile of agricultural area and National Park in La Reserva
zona_agricola <- readOGR(snakemake@input$zona_agricola)
#zona_agricola <- readOGR("resources/LandCover/Zona_Agricola.shp")
zona_agricola <- spTransform(zona_agricola, crs(obs_dungs_sp))


# Observed distribution of dung piles with guava across La Reserva
obs_dungs_sp$zona_agricola <- over(obs_dungs_sp, zona_agricola)$tipo
obs_dungs_sp_df <- data.frame(obs_dungs_sp)
obs_dungs_sp_df$zona_agricola <- gsub('NA', 'PNG', obs_dungs_sp_df$zona_agricola)
obs_dungs_sp_df$zona_agricola[is.na(obs_dungs_sp_df$zona_agricola)] <- 'PNG'
obs_dungs_sp_df$zona_agricola <- as.factor(obs_dungs_sp_df$zona_agricola)
obs_dungs_sp_df <- obs_dungs_sp_df[obs_dungs_sp_df$Long < -90.333,] # La Reserva
obs_dungs_sp_df$zona_agricola <- revalue(obs_dungs_sp_df$zona_agricola, c("PNG"="National Park", "zona agricola"="Agricultural area"))

# Simulated distribution of dung piles with guava across La Reserva
simulated.data.1$zona_agricola <- over(simulated.data.1, zona_agricola)$tipo
sim_dungs_sp_df <- data.frame(simulated.data.1)
sim_dungs_sp_df$zona_agricola <- gsub('NA', 'PNG', sim_dungs_sp_df$zona_agricola)
sim_dungs_sp_df$zona_agricola[is.na(sim_dungs_sp_df$zona_agricola)] <- 'PNG'
sim_dungs_sp_df$zona_agricola <- as.factor(sim_dungs_sp_df$zona_agricola)
sim_dungs_sp_df <- sim_dungs_sp_df[sim_dungs_sp_df$long < -90.333,] # La Reserva
sim_dungs_sp_df$zona_agricola <- revalue(sim_dungs_sp_df$zona_agricola, c("PNG"="National Park", "zona agricola"="Agricultural area"))

jpeg(snakemake@output$fig_3, width=800, height=600, quality = 100)
#jpeg("results/figures/Figure_3.jpg", width=800, height=600, quality = 100)

# Observed density plot of dung piles with guava across elevation
obs_density <- ggdensity(obs_dungs$alt, 
                         main = "(a) Observed dung with guava across elevation",
                         xlab = "Elevation (in m)") + 
  xlim(0, 500) + 
  geom_vline(xintercept = 148, linetype="dashed") + 
  geom_vline(xintercept = 398, linetype="dashed") +
  theme(plot.title = element_text(hjust=0.5)) +
  ylab('Density')

# Simulated density plot of dung piles with guava across elevation
sim_density <- ggdensity(simulated.data.1[which(simulated.data.1$seeds > 0),]$elevation, 
                         main = "(c) Simulated dung with guava across elevation",
                         xlab = "Elevation (in m)") + 
  xlim(0, 500) + 
  geom_vline(xintercept = 148, linetype="dashed") + 
  geom_vline(xintercept = 398, linetype="dashed") +
  theme(plot.title = element_text(hjust=0.5)) +
  ylab('Density')

# Observed plot of dung piles with guava across La Reserva
obs_alt_zones <- ggplot(obs_dungs_sp_df, aes(x =zona_agricola, y =alt)) + 
  geom_boxplot() + 
  theme_classic() + 
  theme(text = element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        plot.title = element_text(hjust=0.5)) +
  ylim(0, 500) + 
  ggtitle('(b) Observed dung with guava at La Reserva') + 
  ylab('Elevation (in m)') +
  xlab(element_blank()) + 
  coord_flip()

# Simulated plot of dung piles with guava across La Reserva
sim_alt_zones <- ggplot(sim_dungs_sp_df, aes(x = zona_agricola, y = elevation)) + 
  geom_boxplot() + 
  theme_classic() + 
  theme(text = element_text(size=12),
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        plot.title = element_text(hjust=0.5)) +
  ylim(0, 500) + 
  ggtitle('(d) Simulated dung with guava at La Reserva') + 
  ylab('Elevation (in m)') + 
  xlab(element_blank()) +
  coord_flip()

grid.arrange(obs_density, obs_alt_zones, sim_density, sim_alt_zones)

dev.off()



## Figure S1: Empirical tracks of migrating tortoises used to calibrate the model

# Get tortoise downslope migrations obtained from tracking data
load(snakemake@input$obs_migrations)
#load('results/output/000/downslope_migration.RData')
down_migr <- downslope_migration %>%
  as_tibble() %>%
  separate(timestamp,c('year','month','day','time'),'-') %>%
  unite("ID_Year", individual.local.identifier, year, sep="_")

jpeg(snakemake@output$fig_S1, width=600, height=600, quality = 100)
#jpeg("results/figures/Figure_S1.jpg", width=600, height=600, quality = 100)

# Plot the background elevation raster
theme_set(theme_void())
p1 <- ggplot() +
  geom_raster(data = tib, aes(x = x, y = y, fill = forColor)) +
  scale_x_continuous(limits = c(-90.55, -90.25)) +
  scale_y_continuous(position = "left", limits = c(-0.80,-0.60)) +
  scale_fill_manual(
    values = brewer.pal(9, "Oranges"),
    name = "Elevation (m)",
    labels = c("< 0", "0–100", "100–200", "200–300", "300–400", "400–500", "500–600", "600–700", "> 700"),
    guide = guide_legend(reverse=TRUE)
  ) +
  labs(x = "Longitude",
       y = "Latitude")
# Plot the seasonal distributions
#p1 <- p1 +
#  geom_polygon(data = tortoises.highland, aes(x = long, y = lat, group = group), colour="grey47", fill="grey47") +
#  geom_polygon(data = tortoises.lowland, aes(x = long, y = lat, group = group), colour="grey47", fill="grey47")
# Plot the migratory connections
p1 <- p1 +
  geom_path(data = down_migr, aes(x = coords.x1, y = coords.x2, group = ID_Year)) +
  theme(legend.key.size = unit(0.7, "cm"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12)
  )
p1

dev.off() 



##  Figure S2: quantile-quantile plot of observed and simulated dung piles with guava across elevation

jpeg(snakemake@output$fig_S2, width=600, height=400, quality = 100)
#jpeg("results/figures/Figure_S2.jpg", width=600, height=400, quality = 100)

grid.arrange(ggqqplot(obs_dungs$alt, 
                      title = '(a) Observed'), 
             ggqqplot(simulated.data.1[which(simulated.data.1$seeds > 0),]$elevation, 
                      title = '(b) Simulated'), 
             ncol = 2)
dev.off()




## Figure S3: Simulated seed dispersal for different migratory movement frequency

jpeg(snakemake@output$fig_S3, width=1050, height=1500, quality = 100)
#jpeg("results/figures/Figure_S3.jpg", width=1500, height=2000, quality = 100)

theme_set(theme_void())

# Empirical data
obs_density <- ggdensity(obs_dungs$alt, 
                         main = "(a) Observed dung with guava",
                         xlab = "Elevation (in m)") + 
  xlim(0, 500) + 
  geom_vline(xintercept = 148, linetype="dashed") + 
  geom_vline(xintercept = 398, linetype="dashed") + 
  theme(text = element_text(size=22),
        plot.title = element_text(hjust = 0.5, size=22)) +
  ylab('Density')

# Migrating once a day
sim_density_1 <- ggdensity(simulated.data.1[which(simulated.data.1$seeds > 0),]$elevation, 
                           main = "(b) Simulated dung with guava (1 move/day)",
                           xlab = "Elevation (in m)") + 
  xlim(0, 500) + 
  geom_vline(xintercept = 148, linetype="dashed") + 
  geom_vline(xintercept = 398, linetype="dashed") + 
  theme(text = element_text(size=22),
        plot.title = element_text(hjust = 0.5, size=22)) +
  ylab('Density')

seed.rain.1 <- rasterize(simulated.data.1, sp_grd_raster, field = simulated.data.1@data$seeds, fun = sum, na.rm=T)
seedRain.1 <- as.data.frame(rasterToPoints(seed.rain.1)) %>%
  as_tibble() %>%
  dplyr::mutate(
    forColor = case_when(
      layer > 12000 ~ "col7",
      layer > 10000 ~ "col6",
      layer > 8000 ~ "col5",
      layer > 6000 ~ "col4",
      layer > 4000 ~ "col3",
      layer > 2000 ~ "col2",
      layer > 0  ~ "col1",
      layer == 0 ~ "col0"
    )
  )
seed_rain_plot_1 <- ggplot() +
  geom_raster(data = tib, aes(x = x, y = y), fill = "grey75") +
  scale_x_continuous(limits = c(-90.55, -90.25)) +
  scale_y_continuous(position = "left", limits = c(-0.80,-0.60)) +
  geom_raster(data = seedRain.1, aes(x = x, y = y, fill = forColor)) +
  scale_fill_manual(
    values = c("grey75", brewer.pal(7, "Reds")),
    name = "Number of seeds",
    labels = c("0", "1–2000", "2001–4000", "4001–6000", "6001–8000", "8001–10000", "10001–12000", "> 12000"),
    guide = guide_legend(reverse=TRUE)
  ) +
  geom_polygon(data = tortoises.highland, aes(x = long, y = lat, group = group), colour="grey47", fill="grey47") +
  theme(legend.key.size = unit(0.7, "cm"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size=22)
  ) +
  labs(title = "(c) Seed rain (1 move/day)")

# Migrating twice a day
sim_density_2 <- ggdensity(simulated.data.2[which(simulated.data.2$seeds > 0),]$elevation, 
                           main = "(d) Simulated dung with guava (2 moves/day)",
                           xlab = "Elevation (in m)") + 
  xlim(0, 500) + 
  geom_vline(xintercept = 148, linetype="dashed") + 
  geom_vline(xintercept = 398, linetype="dashed") + 
  theme(text = element_text(size=22),
        plot.title = element_text(hjust = 0.5, size=22)) +
  ylab('Density')

seed.rain.2 <- rasterize(simulated.data.2, sp_grd_raster, field = simulated.data.2@data$seeds, fun = sum, na.rm=T)
seedRain.2 <- as.data.frame(rasterToPoints(seed.rain.2)) %>%
  as_tibble() %>%
  dplyr::mutate(
    forColor = case_when(
      layer > 12000 ~ "col7",
      layer > 10000 ~ "col6",
      layer > 8000 ~ "col5",
      layer > 6000 ~ "col4",
      layer > 4000 ~ "col3",
      layer > 2000 ~ "col2",
      layer > 0  ~ "col1",
      layer == 0 ~ "col0"
    )
  )
seed_rain_plot_2 <- ggplot() +
  geom_raster(data = tib, aes(x = x, y = y), fill = "grey75") +
  scale_x_continuous(limits = c(-90.55, -90.25)) +
  scale_y_continuous(position = "left", limits = c(-0.80,-0.60)) +
  geom_raster(data = seedRain.2, aes(x = x, y = y, fill = forColor)) +
  scale_fill_manual(
    values = c("grey75", brewer.pal(7, "Reds")),
    name = "Number of seeds",
    labels = c("0", "1–2000", "2001–4000", "4001–6000", "6001–8000", "8001–10000", "10001–12000", "> 12000"),
    guide = guide_legend(reverse=TRUE)
  ) +
  geom_polygon(data = tortoises.highland, aes(x = long, y = lat, group = group), colour="grey47", fill="grey47") +
  theme(legend.key.size = unit(0.7, "cm"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size=22)
  ) +
  labs(title = "(e) Seed rain (2 moves/day)")

# Migrating four times a day
sim_density_4 <- ggdensity(simulated.data.4[which(simulated.data.4$seeds > 0),]$elevation, 
                           main = "(f) Simulated dung with guava (4 moves/day)",
                           xlab = "Elevation (in m)") + 
  xlim(0, 500) + 
  geom_vline(xintercept = 148, linetype="dashed") + 
  geom_vline(xintercept = 398, linetype="dashed") +
  theme(text = element_text(size=22),
        plot.title = element_text(hjust = 0.5, size=22)) +
  ylab('Density')

seed.rain.4 <- rasterize(simulated.data.4, sp_grd_raster, field = simulated.data.4@data$seeds, fun = sum, na.rm=T)
seedRain.4 <- as.data.frame(rasterToPoints(seed.rain.4)) %>%
  as_tibble() %>%
  dplyr::mutate(
    forColor = case_when(
      layer > 12000 ~ "col7",
      layer > 10000 ~ "col6",
      layer > 8000 ~ "col5",
      layer > 6000 ~ "col4",
      layer > 4000 ~ "col3",
      layer > 2000 ~ "col2",
      layer > 0  ~ "col1",
      layer == 0 ~ "col0"
    )
  )
seed_rain_plot_4 <- ggplot() +
  geom_raster(data = tib, aes(x = x, y = y), fill = "grey75") +
  scale_x_continuous(limits = c(-90.55, -90.25)) +
  scale_y_continuous(position = "left", limits = c(-0.80,-0.60)) +
  geom_raster(data = seedRain.4, aes(x = x, y = y, fill = forColor)) +
  scale_fill_manual(
    values = c("grey75", brewer.pal(7, "Reds")),
    name = "Number of seeds",
    labels = c("0", "1–2000", "2001–4000", "4001–6000", "6001–8000", "8001–10000", "10001–12000", "> 12000"),
    guide = guide_legend(reverse=TRUE)
  ) +
  geom_polygon(data = tortoises.highland, aes(x = long, y = lat, group = group), colour="grey47", fill="grey47") +
  theme(legend.key.size = unit(0.7, "cm"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size=22)
  ) +
  labs(title = "(g) Seed rain (4 moves/day)")

blank <- grid.rect(gp=gpar(col="white"))

grid.arrange(obs_density, blank, sim_density_1, seed_rain_plot_1, sim_density_2, seed_rain_plot_2, sim_density_4, seed_rain_plot_4, nrow=4)

dev.off()  


##  Figure S4

# Calculate observed landcover percentages
obs_dungs_sp$landcover <- over(obs_dungs_sp, landcover)$Ecosis_Nat
percents_landcover_observed <- table(obs_dungs_sp$landcover) / sum(table(obs_dungs_sp$landcover))
percents_landcover_observed <- percents_landcover_observed[which(percents_landcover_observed > 0)]
percents_landcover_observed <- as.data.frame(setDT(data.frame(percents_landcover_observed), keep.rownames = FALSE)[])
names(percents_landcover_observed) <- c('Landcover_class', 'Percent')
percents_landcover_observed$sim_obs <- 'Observed'
percents_landcover_observed$Landcover_class <- c('Agricultural area', 'Deciduous forest', 'Evergreen stationary\nforest', 'Evergreen forest\nand shrubs', 'Invasive species')
# Calculate simulated landcover percentages
simulated.data.1$landcover <- over(simulated.data.1, landcover)$Ecosis_Nat
percents_landcover_simulated <- table(simulated.data.1$landcover) / sum(table(simulated.data.1$landcover))
percents_landcover_simulated <- percents_landcover_simulated[which(percents_landcover_simulated > 0)]
percents_landcover_simulated <- as.data.frame(setDT(data.frame(percents_landcover_simulated), keep.rownames = FALSE)[])
names(percents_landcover_simulated) <- c('Landcover_class', 'Percent')
percents_landcover_simulated$sim_obs <- 'Simulated'
percents_landcover_simulated$Landcover_class <- c('Agricultural area', 'Deciduous forest', 'Evergreen stationary\nforest', 'Evergreen forest\nand shrubs', 'Invasive species')

percents_landcover <- rbind(percents_landcover_observed, percents_landcover_simulated)

jpeg(snakemake@output$fig_S4, width=600, height=400, quality = 100)
#jpeg("results/figures/Figure_S4.jpg", width=700, height=400, quality = 100)
ggplot(percents_landcover, aes(x=Landcover_class, y=Percent, fill=sim_obs)) +
  geom_bar(position=position_dodge(), stat="identity", color="grey30") + 
  theme_classic() + 
  scale_fill_manual(values=c("dark orange", "grey"), name=element_blank()) +
  ggtitle(NULL) + 
  ylim(0.0, 0.4) +
  xlab('Landcover class') +
  ylab('Percentage of dungpile deposition')
dev.off() 




## Figure S5: Estimated seed dispersal efficiency
seed.rain.germ.2 <- rasterize(simulated.data.2, sp_grd_raster, field = simulated.data.2@data$seeds.germinated, fun = sum, na.rm=T)
seedRainGerm.2 <- as.data.frame(rasterToPoints(seed.rain.germ.2)) %>%
  as_tibble() %>%
  dplyr::mutate(
    forColor = case_when(
      layer > 12000 ~ "col7",
      layer > 10000 ~ "col6",
      layer > 8000 ~ "col5",
      layer > 6000 ~ "col4",
      layer > 4000 ~ "col3",
      layer > 2000 ~ "col2",
      layer > 0  ~ "col1",
      layer == 0 ~ "col0"
    )
  )

jpeg(snakemake@output$fig_S5, width=600, height=600, quality = 100)

theme_set(theme_void())

# Plot the background
p1 <- ggplot() +
  geom_raster(data = tib, aes(x = x, y = y), fill = "grey75") +
  scale_x_continuous(limits = c(-90.55, -90.25)) +
  scale_y_continuous(position = "left", limits = c(-0.80,-0.60))
# Plot the seed rain 
p1 <- p1 +
  geom_raster(data = seedRainGerm.2, aes(x = x, y = y, fill = forColor)) +
  scale_fill_manual(
    values = c("grey75", brewer.pal(7, "Reds")),
    name = "Number of seeds",
    labels = c("0", "1–2000", "2001–4000", "4001–6000", "6001–8000", "8001–10000", "10001–12000", "> 12000"),
    guide = guide_legend(reverse=TRUE)
  )
# Plot the seasonal distributions
p1 <- p1 +
  geom_polygon(data = tortoises.highland, aes(x = long, y = lat, group = group), colour="grey47", fill="grey47")
  #geom_polygon(data = tortoises.lowland, aes(x = long, y = lat, group = group), colour="grey47", fill="grey47")
p1 <- p1 +
  theme(legend.key.size = unit(0.7, "cm"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12)
  )
p1
dev.off()












