### SCRIPT USED IN TFG OF LUCIA DE HOYOS GONZALEZ
# BACHELOR IN BIOMEDICAL ENGINEERING AT UC3M
# Madrid, June 2019
# Contact: luciadeh@gmail.com
# ===================================================== 
#
# Description:
#       this script inputs the raw processed data and prepares it for further analysis
#
# ===================================================== 

setwd("D:/lucia_TFG/00-InitialFiles/raw_data/")
f_th = list.files(pattern = 'thickness.csv')
for (i in 1:length(f_th)){
  fin <- read.csv(f_th[i], header=TRUE, stringsAsFactors = FALSE)
  # get the data for left hemisphere
  lh <- fin[,grep('lh_', colnames(fin))];   
  # get the data for right hemisphere
  rh <- fin[,grep('rh_', colnames(fin))]
  # save the scan ID
  ID <- fin$ID
  fout <- data.frame(ID= ID, lh, rh)
  ## SAVE THE EDITED FILE
  write.csv(x= fout, file= paste0('D:/lucia_TFG/00-InitialFiles/prepared_data/', f_th[i]),row.names = FALSE)
}