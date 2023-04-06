require(lubridate)
require(tidyverse)
source("R/utilities.r")
source("R/variables.r")

df_ops <-  readRDS(ops_file_raw)
# Transform the bin sizes of the OPS into equivalent sensor bins
df_ops <- transform_ops_bin_sizes(df_ops)
df_ops$date <- round_date(df_ops$date, unit = "10 s")
# Add back the experimental conditions to the dataframe
df_ops <- flag_cdt_data(df_ops, cdt)

df_ops %>%
  saveRDS(ops_file_transformed)


df_sensors <- readRDS(sensors_raw_file)

# Make sure we only use the right day of experiments
df_sensors <- df_sensors %>%
  filter(date >= as.POSIXct("2019-09-05 07:00:00"),
         date <= as.POSIXct("2019-09-05 20:00:00"))

# Average the readings of the sensors every 10 seconds
df_sensors$cut_date <- cut(df_sensors$date, breaks = "10 s")
df_sensors <- df_sensors %>%
  group_by(sensor, cut_date, site) %>%
  summarise(across(where(is.numeric),  ~ mean(.x, na.rm = TRUE))) %>%
  mutate(date = as.POSIXct(cut_date, tz = "UTC"))


# Add back the experimental conditions to the dataframe
df_sensors <- flag_cdt_data(df_sensors, cdt)

df_sensors %>%
  saveRDS(sensor_file)


