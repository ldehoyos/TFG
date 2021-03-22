### SCRIPT USED IN TFG OF LUCIA DE HOYOS GONZALEZ
# BACHELOR IN BIOMEDICAL ENGINEERING AT UC3M
# Madrid, June 2019
# Contact: luciadeh@gmail.com
# ===================================================== 
#
# Description:
#       this script prepares the fenotypic files for the analyis
#
# ===================================================== 

setwd('D:/lucia_TFG/01-PreparedFiles/')

# 1. Read the initial fenotypic file and write it to the PreparedFiles folder
phenotypes <- read.csv('D:/lucia_TFG/00-InitialFiles/my_phenotypes/fenotypes_controls.csv', header=TRUE,stringsAsFactors = FALSE)
write.csv(x= phenotypes, file= 'fenotypes_initial.csv', row.names = FALSE) 

# 2. Load the initial fenotypes file 
  phenotypes <- read.csv('fenotypes_initial.csv', header=TRUE,stringsAsFactors = FALSE)
  # This file already is filtered by controls and included subjects after post processing

# 3. Select only two scans per subject, third scan is not going to be used for the APCs
#   3.1. Take out the third scan of the subjects
    times <- rle(phenotypes$subID); n_scans <- times[[1]]; subjects <- times[[2]]; t_3 <- subjects[which(n_scans == 3)]
    
    phenotypes_3tp <- phenotypes[which(!is.na(match(phenotypes$subID, t_3))),]
    phenotypes_3tp <- phenotypes_3tp[which(phenotypes_3tp$timepoint == 3),]
    phenotypes_2tp <- phenotypes[-match(phenotypes_3tp$ID, phenotypes$ID), ]
  
#   3.2. Order the file of the phenotypes of the two timepoints by subject ID.
    phenotypes_2tp <- phenotypes_2tp[order(phenotypes_2tp$subID),] 
    rm(times, t_3, n_scans, subjects)
    
# 4. Save the information of timepoint 1 
  phenotypes_2tp1 <- phenotypes_2tp[which(phenotypes_2tp$timepoint == 1), ]

# 5. Save the information of timepoint 2 
  phenotypes_2tp2 <- phenotypes_2tp[which(phenotypes_2tp$timepoint != 1), ]
  
# 6. Calculate mean age and scan interval

    mAge <- (phenotypes_2tp2$age + phenotypes_2tp1$age)/2 # get the mean age
    
    scan2 <-  as.character(phenotypes_2tp2$mri_datum) # date of second timepoint
    scan1 <-  as.character(phenotypes_2tp1$mri_datum) # date of first timepoint
    
    # Calculate difference between dates of first and second scans.
    diff_in_years <- as.numeric(difftime(strptime(scan2,  format = "%Y-%m-%d"),
                                         strptime(scan1, format = "%Y-%m-%d"),units="days"))/365
    
# 7. Save the relation between subjects and scan IDs 
    
IDs <- data.frame(subID= phenotypes_2tp1$subID, 
                  ID_scan1= phenotypes_2tp1$ID, ID_scan2= phenotypes_2tp2$ID, 
                  age_scan1= phenotypes_2tp1$age, age_scan2=phenotypes_2tp2$age, age_mean= mAge, 
                  mri_date_1= phenotypes_2tp1$mri_datum, mri_date_2=phenotypes_2tp2$mri_datum, mri_interval = diff_in_years,
                  handedness= phenotypes_2tp1$handedness, scode= phenotypes_2tp1$scode, acode= phenotypes_2tp1$acode, IQ= phenotypes_2tp1$IQ_total, date_of_birth = phenotypes_2tp1$date_of_birth,
                  SES_EdLvl = phenotypes_2tp1$SES_Education_level, SES_OcStat = phenotypes_2tp1$SES_occupational_status, SES_EdYrs= phenotypes_2tp1$SES_Education_totalyears)

write.csv(IDs, 'fenotypes_final.csv', row.names = FALSE) 
rm(IDs, phenotypes, phenotypes_2tp, phenotypes_2tp1, phenotypes_2tp2, phenotypes_3tp, diff_in_years, mAge, scan1, scan2)

# EXPLANATION
# A) phenotypes: initial file (included + controls)
# B) phenotypes_3tp: third scan of subjects with 3 timepoints
# C) phenotypes_2tp: with 2 scans (if 3, taken out the third one)
# D) phenotypes_2tp1: scan1 information of only included + controls with 2 scans (if 3, taken out the third one)
# E) phenotypes_2tp2: scan2 information of only included + controls with 2 scans (if 3, taken out the third one)
# F) IDs: relation between scans and subjects included + controls with 2 scans (if 3, taken out the third one)
