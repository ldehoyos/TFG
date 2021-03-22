### SCRIPT USED IN TFG OF LUCIA DE HOYOS GONZALEZ
# BACHELOR IN BIOMEDICAL ENGINEERING AT UC3M
# Madrid, June 2019
# Contact: luciadeh@gmail.com
# ===================================================== 
#
# Description:
#       this script calculates the APC t-test of the 
#         ROIs.
#
#       this process is each group matrix:
#         females and males in 3 age groups and
#           for the four parcellation scales
#
# ===================================================== 

setwd('D:/00-GM/lucia_TFG/01-thickness/05-matrixcompare1/01_mean')
females = list.files(pattern = 'CT_F'); males = list.files(pattern = 'CT_M')

for (s in 1:4){
  scale = paste0('_S', s)
  for (g in 1:3){
    group = paste0('G', g, scale)
    # For males
    Mm <- read.csv(males[grep(group, males)], header = TRUE, stringsAsFactors = FALSE)
    # For females
    Mf <- read.csv(females[grep(group, females)], header = TRUE, stringsAsFactors = FALSE)
    ROI = ncol(Mf)
    # Calculate mean
    # meanf = unlist(lapply(seq(1,ROI), function(x) mean(Mf[,x])))
    # meanm = unlist(lapply(seq(1,ROI), function(x) mean(Mm[,x])))
    # df = data.frame(ROI= colnames(Mf), Females= meanf, Males= meanm)
    
    # Calculate one sample t-test
    testf = lapply(seq(1,ROI), function(x) t.test(Mf[,x]))
    testm = lapply(seq(1,ROI), function(x) t.test(Mm[,x]))
    
    valuesf= lapply(seq_along(testf), function(x) c(testf[[x]]$statistic, testf[[x]]$p.value))
    valuesm= lapply(seq_along(testm), function(x) c(testm[[x]]$statistic, testm[[x]]$p.value))
    
    statsf = matrix(unlist(valuesf), ncol=2, byrow = TRUE)
    statsm = matrix(unlist(valuesm), ncol=2, byrow = TRUE)
    
    df= data.frame(t.F= statsf[,1], p.F= statsf[,2], t.M= statsm[,1], p.M= statsm[,2], row.names = colnames(Mf))
    
    # Save the matrix
    write.csv(df, file = paste0('D:/lucia_TFG/05-matrixcompare/05-stats/', group, '.csv'), row.names = TRUE)
  }
}
rm(df, Mf, Mm, statsf, statsm, testf, testm, valuesf, valuesm, females, g, group, males, ROI, s, scale)
