# script to pull ARTIS data and create necessary filtered dataframes

# Load libraries
library(dplyr)
library(arrow)

# Set working directory
setwd("/Users/temi/Desktop/Consumption_Rpackage")


# Load full consumption dataset
# custom time series filter - result "artis_ts_result" is NOT in-memory
# this is a "Predicate Pushdown" or "On-disk filtering" method. 
# Only able to filter or subset data - no manipulation until data is in-memory
artis_ds = arrow::open_dataset(file.path("ARTIS_v1.2.0_consumption_FAO_mid_all_HS_yrs_2025-11-01.parquet"))

artis_consumption_df = artis_ds %>%
  filter(
    (hs_version == "HS96" & year <= 2003) |
      (hs_version == "HS02" & year >= 2004 & year <= 2009) |
      (hs_version == "HS07" & year >= 2010 & year <= 2012) |
      (hs_version == "HS12" & year >= 2013 & year <= 2020)
  ) # %>% Can add more data filtering

# Bring the results into R memory by adding "collect()" at the end of the pipe
artis_consumption_df = artis_consumption_df %>% 
  collect()

# focal countries
countries = c('BEN', 'CIV', 'GMB', 'GHA', 'GIN', 'GNB', 'LBR', 'NGA', 'SEN', 'SLE', 'TGO',
'EGY', 'TUN', 'UGA', 'TZA', 'ZMB', 'ZWE', 'MRT', 'ZAF', 'LBY', 'MOZ', 'NAM',
'GAB', 'CMR', 'CPV', 'MAR', 'COD', 'COG', 'AGO', 'DJI', 'DZA', 'KEN', 'MDG',
'MUS', 'SYC', 'SDN', 'SOM', 'ERI')

# create exporter and consumer dataframes
artis_exporter_df = artis_consumption_df[artis_consumption_df$exporter_iso3c %in% countries,]
artis_consumer_df = artis_consumption_df[artis_consumption_df$consumer_iso3c %in% countries,]

rm(artis_consumption_df) # remove full ARTIS consumption dataset from R environment

# create domestic/capture dataframe
artis_capture = artis_consumer_df %>%
  filter(consumption_source == "domestic") %>%
  filter(method == "capture")

# create domestic/aquaculture dataframe
artis_aquaculture = artis_consumer_df %>%
  filter(consumption_source == "domestic") %>%
  filter(method == "aquaculture")

# create importer dataframe by filtering by foreign 1&2
artis_import = artis_consumer_df %>%
  filter(consumption_source %in% c("foreign step 2", "foreign step 1"))

# create exporter dataframe by filtering by foreign 1
artis_export = artis_exporter_df %>%
  filter(consumption_source %in% c("foreign step 1"))

# export .csv files
write.csv(artis_export, "artis_export.csv", row.names = FALSE)
write.csv(artis_consumer_df, "artis_consumer_df.csv", row.names = FALSE)
write.csv(artis_capture, "artis_capture.csv", row.names = FALSE)
write.csv(artis_aquaculture, "artis_aquaculture.csv", row.names = FALSE)
write.csv(artis_import, "artis_import.csv", row.names = FALSE)



 