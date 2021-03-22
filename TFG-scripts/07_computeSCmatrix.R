### SCRIPT USED IN TFG OF LUCIA DE HOYOS GONZALEZ
# BACHELOR IN BIOMEDICAL ENGINEERING AT UC3M
# Madrid, June 2019
# Contact: luciadeh@gmail.com
# ===================================================== 
#
# Description:
#       this script computes the structural covariance
#         matrices for males and females in all age
#         groups and parcellation scales
#
# ===================================================== 

# 1. INITIALIZE FILES AND PACKAGES ----
library(DescTools)
library(corrplot)
library(viridis)
library(data.table)
library(ggplot2)

# 2. GET FILES, GET MEAN MATRIX, GET CORRELATION MATRIX ----

setwd('D:/lucia_TFG/02-FinishedFiles')
fnames= list.files(pattern = 'CT_')
atlfiles = list.files(path= 'D:/lucia_TFG/00-InitialFiles/atlas', full.names = TRUE)

for (s in 1:4){
  scale <- paste0('_S', s)
  myAtl= atlfiles[grep(paste0('scale', s), atlfiles)]
  myFiles= fnames[grep(scale, fnames)] # 6 files in each scale
  atl = read.csv(myAtl, header = TRUE, stringsAsFactors = FALSE)
  atl= atl$name.full
  
  for (j in 1:length(myFiles)){
    myData= read.csv(myFiles[[j]], header=TRUE, stringsAsFactors = FALSE)
    myData <- myData[,-1]
    # get the data 
    newData <- data.frame(myData[,match(colnames(myData), atl)])
    write.csv(x= newData, paste0('D:/lucia_TFG/05-matrixcompare/00_initial/', myFiles[[j]]), row.names = FALSE)
    # divide by left and righ hemisphere and get the mean matrix
    lhData  <- newData[,grep('lh_', colnames(newData))]; colnames(lhData) <- gsub('lh_', '', colnames(lhData))
    rhData  <- newData[,grep('rh_', colnames(newData))]; colnames(rhData) <- gsub('rh_', '', colnames(rhData))
    meanv = (rhData[,match(colnames(lhData), colnames(rhData))] + lhData)/2
    write.csv(x= meanv, paste0('D:/lucia_TFG/05-matrixcompare/01_mean/', myFiles[[j]]), row.names = FALSE)
    # correlate matrix
    corrm = data.frame(cor(meanv)); colnames(corrm) <- c()
    write.csv(x= corrm, paste0('D:/lucia_TFG/05-matrixcompare/02_correlated/', myFiles[[j]]), row.names = FALSE)
    
  } 
}
rm(myData, newData, atl, atlfiles, fnames, j, myAtl, myFiles, s, scale, corrm, lhData, meanv, rhData)
