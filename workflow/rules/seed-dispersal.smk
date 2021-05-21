## Simulate seed dispersal by migratory tortoises
rule seed_dispersal:
    input:
        elevation = "resources/Elevation/SRTM_Santa_Cruz.tif",
        sim_migrations = "results/output/000/simulated_movements.RData",
        guava_distrib = "resources/LandCover/Galapagos_Agroecosystems_LandCover2018.shp",
        highland = "results/output/000/chull_highland.shp",
        sdm_guava = "resources/SDM_guava/Current/psidium_current_cut.gri"
    params:
        freq = "{mfreq}"
    output:
        sim_dispersal = "results/output/100/simulated_data_seeds_{mfreq}.RData"
    conda:
        "../envs/env_R.yaml"
    script:
        "../scripts/100-seed_dispersal/101-seed_dispersal.R"
