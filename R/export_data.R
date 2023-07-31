#-----------------------------------------------------------------------#
#
# Title:  The Muka Head Eddy Covariance Data Import and Process Script
#
# Authors: Yusri Yusup, Ph.D.
#
# Affiliation:  Environmental Technology, School of Industrial Technology,
#               Universiti Sains Malaysia
#
# Datasets used:
#   1. Muka Head Eddy Covariance dataset
#   
#
# Dataset period:
# No. of rows: 
# No. of columns:
# Objective: To import and process the Muka Head EC data
#
#
#-----------------------------------------------------------------------#

#### SAVE DATA ####
# Save the data in the RDS format
save(df_merged_filtered, file = "data/RDS/Muka_Head.Rdata")


#### END ####



