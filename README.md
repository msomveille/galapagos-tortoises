This repository contains the code to simulate the spread of propagules by migratory animals. In particular, it is applied to the spread of invasive guava by giant tortoises on Santa Cruz Island in the Galápagos Archipelago.

The code supports the analysis in Somveille M & Ellis-Soto (2021) Linking animal migration and ecosystem processes: data-driven simulation of propagule dispersal by migratory herbivores.

The entire analysis can be reproduced using snakemake (see https://snakemake.readthedocs.io/en/stable/index.html for how to install and use snakemake). To run the analyis, the user can clone this repository onto their personal computer, open galapagos-tortoises.Rproj in RStudio and in the terminal tab in RStudio run: snakemake --core 1. This will run the anlysis and generate the figures shown in Somveille & Ellis-Soto (2021), which are outputed in results/figures.

Data must be obtained separately and stored in resources/.
