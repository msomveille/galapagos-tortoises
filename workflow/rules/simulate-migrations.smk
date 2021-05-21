## Define seasonal distribution based on tracking data
rule seasonal_distributions:
    input:
        elevation = "resources/Elevation/SRTM_Santa_Cruz.tif",
        tracks = "resources/Tracks/all_tortuga_clean.RData"
    output:
        highland = "results/output/000/chull_highland.shp",
        lowland = "results/output/000/chull_lowland.shp"
    conda:
        "../envs/env_R.yaml"
    script:
        "../scripts/000-migration_simulation/001-seasonal_distributions.R"

## Prepare data for running ORSIM
rule data_prep:
    input:
        elevation = "resources/Elevation/SRTM_Santa_Cruz.tif",
        NDVI_highland = "resources/NDVI/NDVI_highlands.tif",
        NDVI_lowland = "resources/NDVI/NDVI_lowlands.tif",
        highland = "results/output/000/chull_highland.shp",
        lowland = "results/output/000/chull_lowland.shp"
    output:
        data = "results/output/000/galapagos_hexagons_data.csv",
        distMat = "results/output/000/distanceMatrix.csv"
    conda:
        "../envs/env_R.yaml"
    script:
        "../scripts/000-migration_simulation/002-data_prep.R"

## Run ORSIM to simulate migratory connectivity
#input_for_ORSIM = {"data": "results/output/000/galapagos_hexagons_data.csv", "distMat" = "results/output/000/distanceMatrix.csv"}

rule run_ORSIM:
    input:
        {
        "data": "results/output/000/galapagos_hexagons_data.csv",
        "distMat": "results/output/000/distanceMatrix.csv"
        }
        #input_for_ORSIM
        #data = "results/output/000/galapagos_hexagons_data.csv",
        #distMat = "results/output/000/distanceMatrix.csv"
    output:
        connectivity = "results/output/000/ORSIMresults.csv"
    conda:
        "../envs/env_python.yaml"
    script:
        "../scripts/000-migration_simulation/003-ORSIM.py"

## Simulate stochastic individual movements
rule simulate_movements:
    input:
        data = "results/output/000/galapagos_hexagons_data.csv",
        connectivity = "results/output/000/ORSIMresults.csv",
        ertg = "resources/eRTG/eRTG_V1.r",
        obs_migrations = "results/output/000/downslope_migration.RData"
    output:
        sim_migrations = "results/output/000/simulated_movements.RData"
    conda:
        "../envs/env_R.yaml"
    script:
        "../scripts/000-migration_simulation/005-simulate_movements.R"
