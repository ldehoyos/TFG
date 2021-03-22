### SCRIPT USED IN TFG OF LUCIA DE HOYOS GONZALEZ
# BACHELOR IN BIOMEDICAL ENGINEERING AT UC3M
# Madrid, June 2019
# Contact: luciadeh@gmail.com
# ===================================================== 
#
# Description:
#       this script selects the ROIs involve in a 
#         significant correlation and saves the stats
#         of those ROIs.
#
#       this process is done for each group matrix:
#         females and males in 3 age groups and
#           for the four parcellation scales
#
# ===================================================== 

# 1. GET THE SIGNITICATIVE ROI ----
setwd('D:/lucia_TFG/05-matrixcompare/')
fnamespmat= list.files(path = 'D:/lucia_TFG/05-matrixcompare/03_pvalues', pattern = '.csv', full.names = TRUE)
fnames= list.files(path= 'D:/lucia_TFG/05-matrixcompare/02_correlated', pattern = '.csv', full.names = TRUE)
froi=  list.files(path= 'D:/lucia_TFG/05-matrixcompare/05-stats', pattern = '.csv', full.names = TRUE)

thresholds = 0.05

for (i in 1:length(thresholds)){
  c <- 0; pmat <- diff <- mf <- mm<- list()
  p_thr = thresholds[i]
  for (g in 1:3){
    group = paste0('G', g)
    for (s in 1:4){
      scale = paste0(group, '_S', s)
      f= paste0('F_', scale); m = paste0('M_', scale)
      # Read the males and females information
      MM <- as.matrix(read.csv(fnames[grep(m, fnames)], header = FALSE, stringsAsFactors = FALSE))
      MF <- as.matrix(read.csv(fnames[grep(f, fnames)], header = FALSE, stringsAsFactors = FALSE))
      # Get the difference matrix
      MD <- MM^2 - MF^2
      # Get the pmat information
      MB <- as.matrix(read.csv(fnamespmat[grep(scale, fnamespmat)], header = FALSE, stringsAsFactors = FALSE))
      MR <- read.csv(froi[grep(scale, froi)], header = TRUE, stringsAsFactors = TRUE)
      MR <- as.character(MR$X)
      # Take out the upper triangle information
      MB[upper.tri(MB)]= 1; MM[upper.tri(MM)]= 1; MF[upper.tri(MF)]= 1; MD[upper.tri(MD)]= 1
      # Save in a list
      c <- c+1
      matP=  which(MB < p_thr, arr.ind = TRUE)
      if (nrow(matP) == 0){
        pmat[[c]] = diff[[c]] = mf[[c]] =  mm[[c]] = NA
      } else{
        pmat[[c]] = paste0(MR[matP[,1]], '&&', MR[matP[,2]])
        diff[[c]] = MD[which(MB < p_thr)]
        mf[[c]]   = MF[which(MB < p_thr)]
        mm[[c]]   = MM[which(MB < p_thr)]
      }
      # Get the names of the lists
      names(pmat)[[c]] <- names(diff)[[c]] <- names(mf)[[c]] <- names(mm)[[c]] <- paste0(scale, '_')
    }
  }
  
  # Put everything in a data table
  D = matrix(unlist(diff)); rownames(D) <- names(unlist(diff))
  F = matrix(unlist(mf));   rownames(F) <- names(unlist(mf))
  M = matrix(unlist(mm));   rownames(M) <- names(unlist(mm))
  
  ## Still have to omit the NAs
  gr1 <- pmat[grep('G1', names(pmat))]; gr2 <- pmat[grep('G2', names(pmat))]; gr3 <- pmat[grep('G3', names(pmat))];
  names(gr1) <- sub('_', '&&', sub('_S', 'S', names(gr1)));
  names(gr2) <- sub('G1', 'G2', names(gr1));  names(gr3) <- sub('G1', 'G3', names(gr1))
  
  gr1 = (paste0(names(unlist(gr1)), unlist(gr1))); gr2 = (paste0(names(unlist(gr2)), unlist(gr2))); gr3 = (paste0(names(unlist(gr3)), unlist(gr3)))
  
  rwm= c(gr1, gr2, gr3)
  
  M= M[-grep('NA', rwm, ignore.case = FALSE)];  F= F[-grep('NA', rwm, ignore.case = FALSE)]
  D= D[-grep('NA', rwm, ignore.case = FALSE)];  rwm= rwm[-grep('NA', rwm, ignore.case = FALSE)]
  
  if (length(which(duplicated(rwm))) != 0){
    M= M[-which(duplicated(rwm))]; F= F[-which(duplicated(rwm))]; D= D[-which(duplicated(rwm))]; rwm= rwm[-which(duplicated(rwm))]
  }
  df <- data.frame(Diff= D, Females= F, Males= M, row.names = rwm)
  
  write.csv(x = df, file = paste0('D:/lucia_TFG/05-matrixcompare/06/final_', p_thr, '.csv'), row.names= TRUE)
  rm(df, D, F, M, gr1, gr2, gr3, rwm)
}

rm(diff, matP, MB, MD, mf, MF, mm, MM, pmat, c, f, fnames, fnamespmat, froi, g, group, i, m, MR, p_thr, s, scale, thresholds)
# 2. GET ROI STATS ----
setwd('D:/lucia_TFG/05-matrixcompare/06')
fnames= list.files(pattern = '.csv'); final <- list()

for (i in 1:length(fnames)){  final[[i]] <- read.csv(fnames[i], header=TRUE, stringsAsFactors = FALSE)}
names(final) <- c('0.05')

setwd('D:/lucia_TFG/05-matrixcompare/05-stats/')
fnames2 = list.files(); data <- c()
for (g in 1:3){
  for (s in 1:5){
    n= paste0('G', g, '_S', s, '.csv')
    myD = read.csv(n, header = TRUE, stringsAsFactors = FALSE)
    myD[,1] <-paste0('G', g, 'S', s, '&&', myD[,1])
    data <- rbind(data, myD)
  }
}
rm(myD, fnames, fnames2, g, i, n, s)

sp <- list()
for (i in 1:3){
  f <- final[[i]]
  n= matrix(unlist(strsplit(f$X, '&&')), ncol = 3, byrow = TRUE); 
  n[,2] <- gsub('^[0-9]+', '', n[,2]) # take out the numbers at the start
  df= data.frame(X= n[,1], ROI1= n[,2], ROI2=  n[,3], Diff= f$Diff, Females= f$Females, Males= f$Males, stringsAsFactors = FALSE)
  sp[[i]] <- df
}
rm(n, df, i, f, final)
names(sp) <- c('0.05')

for (i in 1:3){
  A = sp[[i]]; dA = data$X
  r1 <- paste0(A$X, '&&', A$ROI1); r1A <- match(r1, dA) # first ROI
  r2 <- paste0(A$X, '&&', A$ROI2); r2A <- match(r2, dA) # second ROI
  
  ROI1 = data[r1A,]; ROI1<- ROI1[,-1]; colnames(ROI1) <- c('ROI1.F.t', 'ROI1.F.p', 'ROI1.M.t', 'ROI1.M.p')
  ROI2 = data[r2A,]; ROI2<- ROI2[,-1]; colnames(ROI2) <- c('ROI2.F.t', 'ROI2.F.p', 'ROI2.M.t', 'ROI2.M.p')
  
  fin <- data.table(A, ROI1, ROI2); rownames(fin) <- c()
  fin[,4:ncol(fin)] <- round(fin[,4:ncol(fin)], 3) # round the numbers to 3 decimals
  fin <- data.frame(fin) # create data frame and save it
  write.csv(fin, file = paste0("D:/lucia_TFG/05-matrixcompare/06/info_", names(sp)[[i]],".csv"), row.names = FALSE)
}