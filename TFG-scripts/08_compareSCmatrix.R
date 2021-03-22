### SCRIPT USED IN TFG OF LUCIA DE HOYOS GONZALEZ
# BACHELOR IN BIOMEDICAL ENGINEERING AT UC3M
# Madrid, June 2019
# Contact: luciadeh@gmail.com
# ===================================================== 
#
# Description:
#       this script compares the obtained structural 
#         covariance matrices of males and females
#         in each age group independently.
#
#       this process is done for the four parcellation
#         scales
#
# ===================================================== 

# 1. INITIALIZE FILES AND PACKAGES ----
library(DescTools)
library(corrplot)
library(viridis)
library(data.table)
library(ggplot2)

# 2. GET P-VALUES OF THE DIFFERENCE OF SC MATRICES OF FEMALES AND MALES ---- 
# Import the info of the groups
df = read.table('D:/lucia_TFG/01-PreparedFiles/InfoGroups.txt'); df= df[,1:2]

setwd('D:/lucia_TFG/05-matrixcompare/02_correlated')
fnames= list.files(pattern = '.csv')

for (g in 1:3){
  group = paste0('G', g)
  nM= df[g,2]
  nF= df[g,1]
  for (s in 1:4){
    scale = paste0(group, '_S', s)
    # For males
    m = paste0('M_', scale); Mm <- read.csv(fnames[grep(m, fnames)], header = FALSE, stringsAsFactors = FALSE)
    # For females
    f = paste0('F_', scale); Mf <- read.csv(fnames[grep(f, fnames)], header = FALSE, stringsAsFactors = FALSE)
    # Convert to z-scores (DescTools)
    Ma<-FisherZ(Mm); Fe<-FisherZ(Mf)
    # Calculate the difference of Z-scores
    diffmat <- Ma^2- Fe^2; diffmat <- as.matrix(diffmat)
    # Fisher test on diff matrix
    test <- diffmat/(sqrt(1/(nM-3)+1/(nF-3)))
    # Set nan to 0
    test[is.nan(test)] = 0
    # Get p-values
    pmat = 2*pnorm(-abs(test))
    # Replace nan with 0's.
    pmat[!is.finite(pmat)] <- 0
    # fdr correction
    # Lower triangle
    ltri <- lower.tri(pmat)
    pmat[ltri] <- p.adjust(pmat[ltri], method = "fdr")
    # Upper triangle
    utri <- upper.tri(pmat)
    pmat[utri] <- t(pmat)[utri]
    
    # Save the matrix
    pmat <- data.frame(pmat); colnames(pmat) <- c()
    write.csv(pmat, file = paste0('D:/00-GM/lucia_TFG/05-matrixcompare/03_pvalues/', scale, '.csv'), row.names = FALSE)
  }
}

rm(df, diffmat, Fe, ltri, utri, Ma, Mf, Mm, pmat, test, f, fnames, g, group, m, nF, nM, s, scale)


# 3. PLOT THE DIFFERENCE MATRIX, FEMALES AND MALES SC MATRICES WITH THE PVALUE THRESHOLD ----
setwd('D:/lucia_TFG/05-matrixcompare/04_plot')
fnamespmat= list.files(path = 'D:/lucia_TFG/05-matrixcompare/03_pvalues', pattern = '.csv', full.names = TRUE)
fnames= list.files(path= 'D:/lucia_TFG/05-matrixcompare/02_correlated', pattern = '.csv', full.names = TRUE)
thresholds = 0.05
for (i in 1:length(thresholds)){
  p_thr= thresholds[i]
  for (g in 1:3){
    group = paste0('G', g)
    for (s in 1:4){
      scale = paste0(group, '_S', s)
      f= paste0('F_', scale); m = paste0('M_', scale)
      
      MM <- as.matrix(read.csv(fnames[grep(m, fnames)], header = FALSE, stringsAsFactors = FALSE))
      MF <- as.matrix(read.csv(fnames[grep(f, fnames)], header = FALSE, stringsAsFactors = FALSE))
      
      Mdiff <- MM^2 - MF^2
      MB <- as.matrix(read.csv(fnamespmat[grep(scale, fnamespmat)], header = FALSE, stringsAsFactors = FALSE))
      
      if  (is.null(dev.list()) == FALSE){dev.off()}
      name <- paste0(scale, "_p_", p_thr, ".pdf")
      pdf(file = name, width = 20, height = 10)
      par(mfrow=c(1,3))
      
      corrplot(Mdiff, method = "color", type= "lower", tl.pos='n',  title = 'Difference (M - F)', 
               col = viridis(200),  p.mat = MB, sig.level = p_thr, insig = "blank", mar=c(0,0,3,0), addgrid.col = NA, outline = FALSE,  cex.main = 3)
      corrplot(MF, method = "color", type= "lower", tl.pos='n',  title = 'Females', col = viridis(200),  
               p.mat = MB, sig.level = p_thr, insig = "blank", mar=c(0,0,3,0), addgrid.col = NA, outline = FALSE,  cex.main = 3)
      corrplot(MM, method = "color", type= "lower", tl.pos='n',  title = 'Males', col = viridis(200),  
               p.mat = MB, sig.level = p_thr, insig = "blank", mar=c(0,0,3,0), addgrid.col = NA, outline = FALSE,  cex.main = 3)
      
      if  (is.null(dev.list()) == FALSE){dev.off()}
      
    }
  }
}
rm(MB, Mdiff, MF, MM, f, fnames, fnamespmat, g, group, m, name, p_thr, s, scale, i, thresholds)
