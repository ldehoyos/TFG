### SCRIPT USED IN TFG OF LUCIA DE HOYOS GONZALEZ
# BACHELOR IN BIOMEDICAL ENGINEERING AT UC3M
# Madrid, June 2019
# Contact: luciadeh@gmail.com
# ===================================================== 
#
# Description:
#       this script plots the SC matrices and difference
#         matrix only of the significant correlations
#
#       this process is done for each group matrix:
#         females and males in 3 age groups and
#           for the four parcellation scales
#
# ===================================================== 

# 1. INITIALIZE FILES AND PACKAGES ----
# devtools::install_github("LCBC-UiO/ggseg", build_vignette=T)
library(ggseg)
library(DescTools)
library(corrplot)
library(viridis)
library(data.table)
library(ggplot2)
library(rapportools)

# 2. PLOT RESULTS ----
setwd('D:/lucia_TFG/07-newmatrixcompare')
fnamespmat= list.files(path = 'D:/lucia_TFG/05-matrixcompare/03_pvalues', pattern = '.csv', full.names = TRUE)
fnames= list.files(path= 'D:/lucia_TFG/05-matrixcompare/02_correlated', pattern = '.csv', full.names = TRUE)

p_thr= 0.05 # only doing it for 0.05
s= 1

for (g in 1:3){
  group = paste0('G', g); scale = paste0(group, '_S', s)
  dat <-read.csv(paste0("D:/lucia_TFG/05-matrixcompare/01_mean/CT_F_G1_S", s,".csv"))
  colnames(dat)
  f= paste0('F_', scale); m = paste0('M_', scale)
  MM <- as.matrix(read.csv(fnames[grep(m, fnames)], header = FALSE, stringsAsFactors = FALSE))
  MF <- as.matrix(read.csv(fnames[grep(f, fnames)], header = FALSE, stringsAsFactors = FALSE))
  Mdiff <- MM^2 - MF^2      
  # MB is the matrix with the p-values.
  MB <- as.matrix(read.csv(fnamespmat[grep(scale, fnamespmat)], header = FALSE, stringsAsFactors = FALSE))
  delet <- which(MB <= p_thr, arr.ind = TRUE); delet <- delet[,1]; 
  if (length(which(duplicated(delet))) != 0){delet <-   delet[-which(duplicated(delet))]}
  
  MB2 <- MB[delet, delet]; MDiff2 <- Mdiff[delet, delet]; MM2 <- MM[delet, delet];  MF2 <- MF[delet, delet]
  
  MM2 <- MM2[order(colnames(dat)[delet]), order(colnames(dat)[delet])]
  MB2 <- MB2[order(colnames(dat)[delet]), order(colnames(dat)[delet])]
  MF2 <- MF2[order(colnames(dat)[delet]), order(colnames(dat)[delet])]
  MDiff2 <- MDiff2[order(colnames(dat)[delet]), order(colnames(dat)[delet])]
  
  rownames(MM2) <- rownames(MF2) <- rownames(MDiff2) <- colnames(MM2) <- colnames(MF2) <- colnames(MDiff2) <- colnames(dat)[delet][order(colnames(dat)[delet])];
  
  if  (is.null(dev.list()) == FALSE){dev.off()}
  name <- paste0(scale, ".png")
  # name <- paste0(scale, "_p_", p_thr, ".pdf")
  png(file = name,  width = 4000, height = 2000, units = "px", pointsize = 20)
  # pdf(file = name, width = 20, height = 10)
  par(mfrow=c(1,3))
  corrplot(MDiff2, method = "color", type = 'lower',  addCoef.col = "white", tl.cex = 2.5, tl.col = 'black', number.cex = 1.5,
           p.mat = MB2, sig.level = p_thr, insig = "blank", addgrid.col = NA, outline = FALSE,  cex.main = 1)
  corrplot(MF2, method = "color",  type = 'lower',   tl.cex = 2.5, tl.col = 'black',
           p.mat = MB2, sig.level = p_thr, insig = "blank",addgrid.col = NA, outline = FALSE,  cex.main = 1)
  corrplot(MM2, method = "color", type= "lower",   tl.cex = 2.5, tl.col = 'black',
           p.mat = MB2, sig.level = p_thr, insig = "blank", addgrid.col = NA, outline = FALSE,  cex.main = 1)
  if  (is.null(dev.list()) == FALSE){dev.off()}
}