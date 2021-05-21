## Plot figures for the paper
rule plot_figures:
    input:
        elevation = "resources/Elevation/SRTM_Santa_Cruz.tif",
        highland = "results/output/000/chull_highland.shp",
        lowland = "results/output/000/chull_lowland.shp",
        data = "results/output/000/galapagos_hexagons_data.csv",
        connectivity = "results/output/000/ORSIMresults.csv",
        sim_migrations = "results/output/000/simulated_movements.RData",
        sim_dispersal = expand("results/output/100/simulated_data_seeds_{mfreq}.RData", mfreq=freq),
        dungpiles = "resources/Dung/collected_dungpiles.csv",
        guava_distrib = "resources/LandCover/Galapagos_Agroecosystems_LandCover2018.shp",
        landcover = "resources/LandCover/benitez_landcover_Santa_Cruz.shp",
        zona_agricola = "resources/LandCover/Zona_agricola.shp",
        obs_migrations = "results/output/000/downslope_migration.RData"
    output:
        fig_2 = "results/figures/Figure_2.jpg",
        fig_3 = "results/figures/Figure_3.jpg",
        fig_S1 = "results/figures/Figure_S1.jpg",
        fig_S2 = "results/figures/Figure_S2.jpg",
        fig_S3 = "results/figures/Figure_S3.jpg",
        fig_S4 = "results/figures/Figure_S4.jpg",
        fig_S5 = "results/figures/Figure_S5.jpg"
    conda:
        "../envs/env_R.yaml"
    script:
        "../scripts/200-figures/201-plot_figures.R"
