sensors_raw_file <- paste0(here::here(), "/data/sensors_raw.Rds")
ops_file_raw <- paste0(here::here(), "/data/ops.rds")
ops_file_transformed <- paste0(here::here(), "/data/ops_transformed.rds")
nanotracer_file <- paste0(here::here(), "/data/nanotracer.rds")
sensors_file <- paste0(here::here(), "/data/sensors.rds")

# List of experimental conditions and of sensors
cdt <- readRDS(paste0(here::here(), "/data/experimental_conditions.Rds"))

