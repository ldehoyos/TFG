### SCRIPT USED IN TFG OF LUCIA DE HOYOS GONZALEZ
# BACHELOR IN BIOMEDICAL ENGINEERING AT UC3M
# Madrid, June 2019
# Contact: luciadeh@gmail.com
# ===================================================== 
#
## Description:
#       Following the same procedure as with fenotypes,
#         the APC data is divided into 3 age groups
#         and by sex (males and females)
#
# ===================================================== 

setwd('D:/lucia_TFG/01-PreparedFiles/')

f_phenotypes <- read.csv('fenotypes_APC.csv', header=TRUE, stringsAsFactors = FALSE)
f_APC <- list.files(pattern='thickness_APC') # 5 files 
groupsID <- levels(factor(f_phenotypes$gcode))

counter <- 0
for (i in 1:length(f_APC)){
  myData <- read.csv(f_APC[i], header=TRUE, stringsAsFactors = FALSE)
  for (j in 1:length(groupsID)){
    subjects <- f_phenotypes$subID[which(f_phenotypes$gcode == groupsID[j])]
    groupsubj <- match(subjects, myData$Study.ID)
    counter <- counter +1
    groupData <- myData[groupsubj, ]
    phData <- f_phenotypes[groupsubj,]
    # DIVIDE BY SEX
    groupDataF <- groupData[which(phData$scode == 0), ] 
    groupDataM <- groupData[which(phData$scode == 1), ]
    
    write.csv(x= groupData, file = paste0('thickness_APC_group', j, '_scale', i, '.csv'), row.names = FALSE)
    write.csv(x= groupDataF, file = paste0('thickness_APC_group', j, '_scale', i, '_S0.csv'), row.names = FALSE)
    write.csv(x= groupDataM, file = paste0('thickness_APC_group', j, '_scale', i, '_S1.csv'), row.names = FALSE)
  }
}
rm(f_phenotypes, groupData, myData, counter, f_APC, groupsID, groupsubj, i, j, subjects, groupDataF, groupDataM, phData)