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


df_ops <- df_ops %>%
  mutate(
    gr03_05um_ref = gr03um_ref - gr05um_ref,
    gr05_10um_ref = gr05um_ref - gr10um_ref,
    gr10_25um_ref = gr10um_ref - gr25um_ref,
    gr25_50um_ref = gr25um_ref - gr50um_ref,
    gr50_100um_ref = gr50um_ref - gr100um_ref
  )


df_ops <- df_ops %>%
  mutate(
    n03_05_ref = n05_ref,
    n05_1_ref = n1_ref - n05_ref,
    n1_25_ref = n25_ref - n1_ref,
    n25_4_ref = n4_ref - n25_ref,
    n4_10_ref = n10_ref - n4_ref
  )

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



df_sensors <- df_sensors %>%
  mutate(
    n03_05 = n05,
    n05_1 = n1 - n05,
    n1_25 = n25 - n1,
    n25_4 = n4 - n25,
    n4_10 = n10 - n4
  )



df_sensors <- df_sensors %>%
  mutate(
    gr03_05um = gr03um - gr05um,
    gr05_10um = gr05um - gr10um,
    gr10_25um = gr10um - gr25um,
    gr25_50um = gr25um - gr50um,
    gr50_100um = gr50um - gr100um
  )

# Add back the experimental conditions to the dataframe
df_sensors <- flag_cdt_data(df_sensors, cdt)

df_sensors %>%
  saveRDS(sensors_file)


