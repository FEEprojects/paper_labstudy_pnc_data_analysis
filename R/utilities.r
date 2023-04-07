require(dplyr)
library(purrr)
require(magrittr)
require(ggplot2)
require(tidyr)
require(splitstackshape)

# Convert bins from the OPS 3330 into bins for the different sensors Based on https://www.mdpi.com/1424-8220/18/9/2790/htm 
# For the Plantower PMS5003


transform_ops_bin_sizes<- function(df_ops){
  
  df_ops <- df_ops %>%
    mutate(gr03um_ref = Bin.1 + Bin.2 + Bin.3 + Bin.4 + Bin.5 + Bin.6 + Bin.7 + Bin.8 + Bin.9 + Bin.10 + Bin.11 + Bin.12 + Bin.13 + Bin.14 + Bin.15 + Bin.16 + Bin.17,
           gr05um_ref = Bin.3*0.6929+ Bin.4 + Bin.5 + Bin.6 + Bin.7 + Bin.8 + Bin.9 + Bin.10 + Bin.11 + Bin.12 + Bin.13 + Bin.14 + Bin.15 + Bin.16 + Bin.17,
           gr10um_ref = Bin.6*0.5318 +Bin.7 + Bin.8 + Bin.9 + Bin.10 + Bin.11 + Bin.12 + Bin.13 + Bin.14 + Bin.15 + Bin.16 + Bin.17,
           gr25um_ref = Bin.10*0.3497 +Bin.11 + Bin.12 + Bin.13 + Bin.14 + Bin.15 + Bin.16 + Bin.17,
           gr50um_ref = Bin.13*0.8217+Bin.14 + Bin.15 + Bin.16 + Bin.17,
           gr100um_ref = Bin.17
    )
  
  
  
  df_ops <- df_ops %>% mutate(n05_ref = Bin.3*0.3070 + Bin.1 + Bin.2,
                    n1_ref = Bin.6*0.4682 + Bin.1 + Bin.2 + Bin.3 + Bin.4 + Bin.5,
                    n25_ref = Bin.10*0.6503 + Bin.1 + Bin.2 + Bin.3 + Bin.4 + Bin.5 + Bin.6 + Bin.7 + Bin.8 + Bin.9,
                    n4_ref = Bin.12*0.8022 + Bin.1 + Bin.2 + Bin.3 + Bin.4 + Bin.5 + Bin.6 + Bin.7 + Bin.8 + Bin.9 + Bin.10 + Bin.11,
                    n10_ref = Bin.1 + Bin.2 + Bin.3 + Bin.4 + Bin.5 + Bin.6 + Bin.7 + Bin.8 + Bin.9 + Bin.10 + Bin.11 + Bin.12 + Bin.13 + Bin.14 + Bin.15 + Bin.16 + Bin.17
  )
  
  
  
  df_ops <- df_ops %>% mutate(Bin0_ref=`Bin.1`*0.6757+`Bin.2`+`Bin.3`*0.4825,
                    "Bin1_ref" = `Bin.4`*0.1479+`Bin.5`+`Bin.6`*0.0136,
                    "Bin2_ref" = `Bin.6`*0.0773+`Bin.7`+`Bin.8`*0.9062,
                    "Bin3_ref" = `Bin.8`*0.6804+`Bin.9`*0.906158,
                    "Bin4_ref" = `Bin.9`*0.6038+`Bin.10`*0.9858,
                    "Bin5_ref"= `Bin.10`*0.538752+`Bin.11`*0.0228,
                    "Bin6_ref" = `Bin.11`*0.5213+`Bin.12`*0.1917,
                    "Bin7_ref" = `Bin.12`*0.1979 +`Bin.13`*0.3314,
                    "Bin8_ref" = `Bin.13`*0.1784+`Bin.14`*0.2506,
                    "Bin9_ref" = `Bin.14`*0.3554+`Bin.15`*0.0310,
                    "Bin10_ref" = `Bin.15`*0.5/(0.8031-6.451),
                    "Bin11_ref" = `Bin.15`*0.0196+`Bin.16`*0.2382,
                    "Bin12_ref" = `Bin.16`*(0.5)/(10-8.031)
  )->df_ops
  
  return(df_ops)
}



custom_theme<- theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

#' Flag condition data
#'
#'@description This function flag the dataframe 
#'with the correct condition (experiment, source, variaton) depending 
#'on time. 
#'Useful after doing a timeAveraging on the data 
#'as the factors are dropped
#'
#'@param tmp data.frame. The dataframe to flag
#'@param cdt data.frame. "data/conditions.Rds"
#'
#'@return tmp with the additional columns "exp", "source", "variation"
#'
flag_cdt_data<-function(tmp, cdt){
  tmp$exp = ""
  tmp$source = ""
  tmp$variation = ""
  
  for(i in 1:nrow(cdt)){
    tmp[tmp$date>=as.POSIXct(cdt[i,]$start.date,tz="UTC") &
          tmp$date<=as.POSIXct(cdt[i,]$end.date,tz="UTC"),] %<>% mutate(exp = as.character(cdt[i,]$exp),
                                                                        source = as.character(cdt[i,]$source),
                                                                        variation = as.character(cdt[i,]$variation))
  }
  return(tmp)
}



#' get_sensor_type
#' Extract the sensor type from the sensor id
#' sensor id must respect the format sensorType_idNumber
#'
#' @param df - dataframe containing the time series measurements of a network of PM sensors
#'            with columns:
#'             - sensor: sensor identification number - sensorType_idNumber
#' 
#' @param ... 
#'
#' @return dataframe with column sensor_type added
#'
#' @export
#'
#' @examples get_sensor_type(df)
get_sensor_type <- function(df, ...){
  
  cSplit(indt=df,splitCols = c("sensor"),sep="-",direction="wide",drop=FALSE) %>%
    dplyr::select(-sensor_2) %>%
    rename(sensor_type = sensor_1)
  
}