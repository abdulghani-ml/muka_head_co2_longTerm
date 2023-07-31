#-----------------------------------------------------------------------#
#
# Title:  Merging Filtered Data with Eddy Covariance Data
#
# Authors: Yusri Yusup, Ph.D.
#
# Affiliation:  Environmental Technology, School of Industrial Technology,
#               Universiti Sains Malaysia
#
# Datasets used:
#   1. df_filtered
#   2. df
#
# Dataset period:
# No. of rows: 
# No. of columns:
# Objective: To import and process data
#
#
#-----------------------------------------------------------------------#

#### MERGING DATA ####
df_merged_filtered <- merge(df, df_filtered, by = "date")

#### CALCULATE DELTA T ####

delT <- df_merged_filtered$TS - df_merged_filtered$TA
df_merged_filtered <- cbind(df_merged_filtered,delT)
rm(delT)

#### CALCULATE DELTA T (EMA DATA) ####

delT_ema <- df_merged_filtered$TS_ema - df_merged_filtered$TA_ema
df_merged_filtered <- cbind(df_merged_filtered,delT_ema)
rm(delT_ema)

#### END ####