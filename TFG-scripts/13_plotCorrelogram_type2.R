### SCRIPT USED IN TFG OF LUCIA DE HOYOS GONZALEZ
# BACHELOR IN BIOMEDICAL ENGINEERING AT UC3M
# Madrid, June 2019
# Contact: luciadeh@gmail.com
# ===================================================== 
#
# Description:
#       this script plots the correlogram of the
#         significant correlations
#
#       this process is done for each difference matrix:
#         3 age groups and for the four parcellation scales
#
#       package used -> chorddiag 
#
# ===================================================== 
library(igraph); library(tidygraph); library(tidyverse); library(chorddiag)

group <- 1
scale <- 1

file_p <- read.csv(paste0('D:/lucia_TFG/05-matrixcompare/03_pvalues/group', group, '_scale', scale, '.csv'), header=FALSE)
file_F <- read.csv(paste0('D:/lucia_TFG/05-matrixcompare/02_correlated/CT_F_group', group, '_scale', scale, '.csv'), header=FALSE)
file_M <- read.csv(paste0('D:/lucia_TFG/05-matrixcompare/02_correlated/CT_M_group', group, '_scale', scale, '.csv'), header=FALSE)
file_D <- (file_M^2) - (file_F^2)
file_D[which(file_p > 0.05)] <- 0


regions <- colnames(read.csv(paste0('D:/lucia_TFG/05-matrixcompare/01_mean/CT_F_group', group, '_scale', scale, '.csv'), header=TRUE))

colnames(file_D) <- rownames(file_D) <- regions

vect <- c()

for (i in 1:ncol(file_D)){
  if (mean(file_D[i,]) == 0){
    vect <- c(vect, 'grey')
  } else if (mean(file_D[i, ]) > 0){
    vect <- c(vect, 'blue')
  } else if (mean(file_D[i,]) < 0){
    vect <- c(vect, 'red')
  }
}
chord<-chorddiag(data = abs(file_D),
                 groupnamePadding = 15,
                 groupPadding = 10,
                 groupColors = vect,
                 groupnameFontsize = 10 ,
                 showTicks = FALSE,
                 margin=150,
                 chordedgeColor = "white")
print(chord)