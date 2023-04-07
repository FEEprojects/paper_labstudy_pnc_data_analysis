

source("R/variables.r")
df_sensors <- readRDS(sensors_file)
df_ops <- readRDS(ops_file_transformed)

df_pms <- df_sensors %>%
  filter(grepl("PMS", sensor))
df_sps <- df_sensors %>%
  filter(grepl("SPS", sensor))
df_opcr1 <- df_sensors %>%
  filter(grepl("OPC", sensor))


summary(lm(data = df_pms, gr03um ~ gr05um))
summary(lm(data = df_pms, gr03um ~ gr10um))
summary(lm(data = df_pms, gr03um ~ gr25um))
summary(lm(data = df_pms, gr03um ~ gr50um))
summary(lm(data = df_pms, gr03um ~ gr100um))


summary(lm(data = df_pms, gr05um ~ gr10um))
summary(lm(data = df_pms, gr05um ~ gr25um))
summary(lm(data = df_pms, gr05um ~ gr50um))
summary(lm(data = df_pms, gr05um ~ gr100um))


summary(lm(data = df_pms, gr10um ~ gr25um))
summary(lm(data = df_pms, gr10um ~ gr50um))
summary(lm(data = df_pms, gr10um ~ gr100um))

summary(lm(data = df_pms, gr25um ~ gr50um))
summary(lm(data = df_pms, gr25um ~ gr100um))

summary(lm(data = df_pms, gr50um ~ gr100um))





#' For the SPS
#'
#'


summary(lm(data = df_sps, n05 ~ n1))
summary(lm(data = df_sps, n05 ~ n25))
summary(lm(data = df_sps, n05 ~ n4))
summary(lm(data = df_sps, n05 ~ n10))

summary(lm(data = df_sps, n1 ~ n25))
summary(lm(data = df_sps, n1 ~ n4))
summary(lm(data = df_sps, n1 ~ n10))

summary(lm(data = df_sps, n25 ~ n4))
summary(lm(data = df_sps, n25 ~ n10))

summary(lm(data = df_sps, n4 ~ n10))


#' Maybe there are two or three actual sizes with that sensor.
#'



#' For the OPCr1
#'
#'

summary(lm(data = df_opcr1, Bin0 ~ Bin1))
summary(lm(data = df_opcr1, Bin0 ~ Bin2))
summary(lm(data = df_opcr1, Bin0 ~ Bin3))
summary(lm(data = df_opcr1, Bin0 ~ Bin4))
summary(lm(data = df_opcr1, Bin0 ~ Bin5))


summary(lm(data = df_opcr1, Bin1 ~ Bin2))
summary(lm(data = df_opcr1, Bin1 ~ Bin3))
summary(lm(data = df_opcr1, Bin1 ~ Bin4))
summary(lm(data = df_opcr1, Bin1 ~ Bin5))

summary(lm(data = df_opcr1, Bin2 ~ Bin3))
summary(lm(data = df_opcr1, Bin2 ~ Bin4))
summary(lm(data = df_opcr1, Bin2 ~ Bin5))

summary(lm(data = df_opcr1, Bin3 ~ Bin4))
summary(lm(data = df_opcr1, Bin3 ~ Bin5))

summary(lm(data = df_opcr1, Bin4 ~ Bin5))



#' For the OPS
#'


summary(lm(data = df_ops, Bin0_ref ~ Bin1_ref))
summary(lm(data = df_ops, Bin0_ref ~ Bin2_ref))
summary(lm(data = df_ops, Bin0_ref ~ Bin3_ref))
summary(lm(data = df_ops, Bin0_ref ~ Bin4_ref))
summary(lm(data = df_ops, Bin0_ref ~ Bin5_ref))


summary(lm(data = df_ops, Bin1_ref ~ Bin2_ref))
summary(lm(data = df_ops, Bin1_ref ~ Bin3_ref))
summary(lm(data = df_ops, Bin1_ref ~ Bin4_ref))
summary(lm(data = df_ops, Bin1_ref ~ Bin5_ref))

summary(lm(data = df_ops, Bin2_ref ~ Bin3_ref))
summary(lm(data = df_ops, Bin2_ref ~ Bin4_ref))
summary(lm(data = df_ops, Bin2_ref ~ Bin5_ref))

summary(lm(data = df_ops, Bin3_ref ~ Bin4_ref))
summary(lm(data = df_ops, Bin3_ref ~ Bin5_ref))

summary(lm(data = df_ops, Bin4_ref ~ Bin5_ref))


#' SPS Like
#'


summary(lm(data = df_ops, n05_ref ~ n1_ref))
summary(lm(data = df_ops, n05_ref ~ n25_ref))
summary(lm(data = df_ops, n05_ref ~ n4_ref))
summary(lm(data = df_ops, n05_ref ~ n10_ref))

summary(lm(data = df_ops, n1_ref ~ n25_ref))
summary(lm(data = df_ops, n1_ref ~ n4_ref))
summary(lm(data = df_ops, n1_ref ~ n10_ref))

summary(lm(data = df_ops, n25_ref ~ n4_ref))
summary(lm(data = df_ops, n25_ref ~ n10_ref))

summary(lm(data = df_ops, n4_ref ~ n10_ref))


#' PMS Like
#'


summary(lm(data = df_ops, gr03um_ref ~ gr05um_ref))
summary(lm(data = df_ops, gr03um_ref ~ gr10um_ref))
summary(lm(data = df_ops, gr03um_ref ~ gr25um_ref))
summary(lm(data = df_ops, gr03um_ref ~ gr50um_ref))
summary(lm(data = df_ops, gr03um_ref ~ gr100um_ref))


summary(lm(data = df_ops, gr05um_ref ~ gr10um_ref))
summary(lm(data = df_ops, gr05um_ref ~ gr25um_ref))
summary(lm(data = df_ops, gr05um_ref ~ gr50um_ref))
summary(lm(data = df_ops, gr05um_ref ~ gr100um_ref))

summary(lm(data = df_ops, gr10um_ref ~ gr25um_ref))
summary(lm(data = df_ops, gr10um_ref ~ gr50um_ref))
summary(lm(data = df_ops, gr10um_ref ~ gr100um_ref))

summary(lm(data = df_ops, gr25um_ref ~ gr50um_ref))
summary(lm(data = df_ops, gr25um_ref ~ gr100um_ref))

summary(lm(data = df_ops, gr50um_ref ~ gr100um_ref))
