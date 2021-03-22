### SCRIPT USED IN TFG OF LUCIA DE HOYOS GONZALEZ
# BACHELOR IN BIOMEDICAL ENGINEERING AT UC3M
# Madrid, June 2019
# Contact: luciadeh@gmail.com
# ===================================================== 
#
# Description:
#       this script inputs the cortical thickness data
#         and calculates the annual percentage change
#         for each subjects and ROI.
#
#       APC is calculated for the four brain parcellation 
#         scales.
#
# ===================================================== 

# 1. Get the thickness data per scale
setwd('D:/lucia_TFG/01-PreparedFiles/')

# Get the scans of each subject
IDs <- read.csv('fenotypes_final.csv', header=TRUE, stringsAsFactors = FALSE)
# Get the file names of the data to use (for the 4 scales)
f_thickness <- list.files(pattern= 'study_thickness') 
# Empty list to save the file information
data_tp1 <- data_tp2 <- data_APC <- list()  

for (i in 1:length(f_thickness)){
  # read the data and save it on the list
  A <- read.csv(f_thickness[i], header=TRUE, stringsAsFactors = FALSE)
  data_tp1[[i]]  <- A[which(!is.na(match(A$ID, IDs$ID_scan1))), ]
  data_tp2[[i]] <- A[which(!is.na(match(A$ID, IDs$ID_scan2))), ] 
  
  data_APC[[i]] <- data_tp1[[i]][, 2:length(data_tp1[[i]])] 
  data_APC[[i]][,] <- 0 # then, set as zero do start replacing
  # Create the APCS
  for (j in 1:nrow(data_tp1[[i]])){
    # ***************************************************************
    t2 <- data_tp2[[i]][j, ] # thickness data of timepoint 2
    t2_1 <- t2[2:length(t2)] # take out the columns scan and subject
    # ***************************************************************
    t1 <- data_tp1[[i]][j, ] #thickness data of timepoint 2
    t1_1 <- t1[2:length(t1)] # take out the columns scan and subject
    # ***************************************************************
    # Calculate the APC
    data_APC[[i]][j, ] <- (((t2_1 - t1_1)/((t2_1 + t1_1)/2))/(IDs$mri_interval[j]))*100
  }
  data_APC[[i]] <- data.frame(Study.ID= IDs$subID, data_APC[[i]])
  write.csv(x= data_APC[[i]], file = paste0('thickness_APC_scale', i, '.csv'), row.names = FALSE)
}
rm(A, data_APC, data_tp1, data_tp2, IDs, t1, t1_1, t2, t2_1, f_thickness, i, j)
