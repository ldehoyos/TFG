### SCRIPT USED IN TFG OF LUCIA DE HOYOS GONZALEZ
# BACHELOR IN BIOMEDICAL ENGINEERING AT UC3M
# Madrid, June 2019
# Contact: luciadeh@gmail.com
# ===================================================== 
#
# Description:
#       this script saves and orders the cortical thickness data for APC
#         from the previous script, takes the first and second timepoint 
#         scans of the included subjects
#
# ===================================================== 

# 1. Get the thickness data per scale 
setwd('D:/lucia_TFG/00-InitialFiles/prepared_data/')
f_thickness <- list.files(pattern= 'thickness.csv') # List the files for thickness
data_thickness <- list() # Empty list to save the file information 

for (i in 1:length(f_thickness)){
  data_thickness[[i]] <-  read.csv(f_thickness[i], header=TRUE)
}


## 2. Get the information of the subjects chosen by me 
setwd('D:/lucia_TFG/01-PreparedFiles/')
phenotypes <- read.csv('fenotypes_final.csv', header=TRUE, stringsAsFactors = FALSE) # READ THE FENOTYPES
IDs <- c(phenotypes$ID_scan1, phenotypes$ID_scan2)

## 3. From the initial file, select only the scans in the phenotypes file
data_thickness_selected <- list() # Empty list to save the scan data (selection)
# Create new names for the csv files in the new directory
for (i in 1:length(f_thickness)){
  positions <- match(IDs, data_thickness[[i]]$ID)
  myData <-  data_thickness[[i]][positions,]
  data_thickness_selected[[i]] <- myData
  
  # Save the file
  write.csv(data_thickness_selected[[i]], file = paste0('planU_thickness_2tp_scale', i, '.csv'), row.names = FALSE)
}

rm(data_thickness, data_thickness_selected, myData, phenotypes, f_thickness, i, IDs, positions)