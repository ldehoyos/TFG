### SCRIPT USED IN TFG OF LUCIA DE HOYOS GONZALEZ
# BACHELOR IN BIOMEDICAL ENGINEERING AT UC3M
# Madrid, June 2019
# Contact: luciadeh@gmail.com
# ===================================================== 
#
# Description:
#       this script computes the sample description
#            t test -> descriptive variables
#        chi square -> qualitative variables
#
# ===================================================== 

library(data.table)
library(MASS)

setwd('D:/lucia_TFG/04-statistics')

# Part 1: Descriptive variables (t test) -----
## VARIABLE: age_scan1, age_scan2, IQ, scan_interval_days
file.copy("D:/lucia_TFG/02-FinishedFiles/covars.csv", "D:/lucia_TFG/04-statistics/phenotypical_scans.csv")

# 1.1 Import file 
data_scans <- read.csv('phenotypical_scans.csv', stringsAsFactors = FALSE, header = TRUE)

# 1.2 Separate for GROUPS 
G1 <- data_scans[which(data_scans$gcode == 1), ]
G2 <- data_scans[which(data_scans$gcode == 2), ]
G3 <- data_scans[which(data_scans$gcode == 3), ]

# 1.3 Separate for scode 
GROUPS <- tTest <- list()
GROUPS[[1]] <- G1[which(G1$scode == 0), ]
GROUPS[[2]] <- G1[which(G1$scode == 1), ]

GROUPS[[3]] <- G2[which(G2$scode == 0), ]
GROUPS[[4]] <- G2[which(G2$scode == 1), ]

GROUPS[[5]] <- G3[which(G3$scode == 0), ]
GROUPS[[6]] <- G3[which(G3$scode == 1), ]

co <- 0; means <- c()

for (i in seq(1, 6, 2)){ # through the three groups, females and males
  S0 <- GROUPS[[i]]
  S1 <- GROUPS[[i+1]]
  t_age1 <- unlist(t.test(S0$age_scan1, S1$age_scan1, conf.level = 0.95))
  t_age2 <- unlist(t.test(S0$age_scan2, S1$age_scan2, conf.level = 0.95))
  t_IQ <- unlist(t.test(S0$IQ, S1$IQ, conf.level = 0.95))
  t_scan_interval <- unlist(t.test(S0$mri_interval, S1$mri_interval,conf.level = 0.95))
  t_SES_totalyear <- unlist(t.test(S0$SES_EdYrs, S1$SES_EdYrs,conf.level = 0.95))
  co <- co+1
  t <- c(age1= t_age1[[1]], age2= t_age2[[1]], IQ= t_IQ[[1]], scan_int= t_scan_interval[[1]], SES_totyr= t_SES_totalyear[[1]])
  p <- c(age1= t_age1[[3]], age2= t_age2[[3]],  IQ= t_IQ[[3]], scan_int= t_scan_interval[[3]],SES_totyr=t_SES_totalyear[[3]])
  
  tTest[[co]] <- data.table(t, p)
  
  mF <- c(age1 = mean(S0$age_scan1), age2= mean(S0$age_scan2), IQ = mean(na.omit(S0$IQ)), 
          scanInterval = mean(na.omit(S0$mri_interval)), SES_EdYrs = mean(na.omit(S0$SES_EdYrs)))
  mM <-  c(age1 = mean(S1$age_scan1), age2= mean(S1$age_scan2), IQ = mean(na.omit(S1$IQ)), 
           scanInterval = mean(na.omit(S1$mri_interval)), SES_EdYrs = mean(na.omit(S1$SES_EdYrs)))
  
  means[[co]] <- data.table(mF, mM, t, p)
  names(means)[[co]] <- names(tTest)[[co]] <- paste0('group', co)
}

df0 <- cbind(G1= means[[1]], G2= means[[2]], G3= means[[3]])
df1 <- round(as.numeric(as.matrix(df0)), 3)
testt= data.frame(matrix(df1, nrow = 5, byrow = FALSE), row.names = names(mF)); colnames(testt) <- colnames(df0)

# 1.4 Save the file 
write.csv(x= testt, file = 'results_test_t.csv', row.names = TRUE)
rm(G1, G2, G3, GROUPS, means, S0, S1, tTest, co, i, mF, mM, p, t, t_age1, t_age2, t_IQ, t_scan_interval, t_SES_totalyear, df0, testt, df1)

# Part 2: Qualitative variables (chi squared) ----
# VARIABLES: gcode, scode, acode, handedness
# 2.1 Import file
data_scans <- data.frame(data_scans$gcode, data_scans$scode, data_scans$acode, data_scans$handedness, data_scans$SES_EdLvl,
                         data_scans$SES_OcStat)

# 2.2 Get all the other
stat_list <- p_val <- observed <-tl <- list()
c <- 0
lev <- levels(factor(data_scans$data_scans.gcode))

for (i in 1:length(lev)){
  df <- data_scans[which(data_scans$data_scans.gcode == lev[i]),]
  colnames(df) <- c("gcode", "scode", "acode", "handedness", "SES_EdLvl", "SES_OcStat")
  for (j in 3:6){
    tbl <-  table(df$scode, df[,j])
    ams <-  chisq.test(tbl)
    ams$data.name <- paste0('G', i, '_', colnames(df)[j])
    c <- c+1
    myData <-  unlist(ams)
    a <- names(myData)
    stat_list[[c]] <- myData[grep('statistic', a)]
    p_val[[c]] <- myData[grep('p.value', a)]
    observed[[c]] <- myData[grep('observed', a)]
    tl [[c]] <- t(tbl)
    names(stat_list)[[c]] <- names(observed)[[c]] <- names(tl)[[c]] <- names(p_val)[[c]] <- myData[grep('data', a)]
    rm(tbl)
  }
}

# 2.3 Create data frame of stats and P-val rounding to three digits 
st <- round(as.numeric(unlist(stat_list)), 3)
stats <- data.frame(matrix(st, ncol=3, byrow = FALSE), stringsAsFactors = FALSE); colnames(stats) <- c("G1", "G2", "G3"); 

pv <- round(as.numeric(unlist(p_val)), 3)
P <- data.frame(matrix(pv, ncol=3, byrow = FALSE), stringsAsFactors = FALSE); colnames(P) <- c("G1", "G2", "G3"); 

# 2.4 Save the stats and the times repeated and join them 
df1 <- tl[grep('G1', names(tl))]; df2 <- tl[grep('G2', names(tl))]; df3 <- tl[grep('G3', names(tl))]
G1 <- G2 <- G3 <- c()

for (i in 1:4){
  G1 <- rbind(G1, df1[[i]]);   G2 <- rbind(G2, df2[[i]]);  G3 <- rbind(G3, df3[[i]])
}
G1 <- rbind(G1[1:12,], c(0, 0), G1[13,]); G2 <- rbind(G2[1:12,], c(0, 0), G2[13,])
rownames(G1)[13] <- rownames(G2)[13] <- 3; rownames(G1)[14] <- rownames(G2)[14] <- 4

G3 <- rbind(G3[1:7,], c(0, 0), G3[8:10,], c(0, 0), G3[11:12,]); rownames(G3) <-rownames(G1)

colnames(G1) <- colnames(G2) <- colnames(G3) <- c("F", "M")
dfs <- rbind(stats[1,], c('', '', ''), stats[2,], c('', '', ''), c('', '', ''), 
             stats[3,], c('', '', ''), c('', '', ''), c('', '', ''), c('', '', ''), stats[4,], c('', '', ''), c('', '', ''),c('', '', ''))

dfp <- rbind(P[1,], c('', '', ''), P[2,], c('', '', ''), c('', '', ''), 
             P[3,], c('', '', ''), c('', '', ''), c('', '', ''), c('', '', ''), P[4,], c('', '', ''), c('', '', ''),c('', '', ''))

rwm <- c("acode0", "acode1", 
         "handedness1", "handedness2", "handedness3", 
         "EdLvl1", "EdLvl2", "EdLvl3", "EdLvl4", "EdLvl5",
         "OccStat1", "OccStat2","OccStat3","OccStat4")

testchi <- data.frame(G1= G1, G1.x= dfs$G1, G1.p= dfp$G1, 
                      G2= G2, G2.x= dfs$G2, G2.p= dfp$G2,
                      G3= G3, G3.x= dfs$G3, G3.p= dfp$G3, row.names = rwm);  
rm(df1, df2, df3, G1, G2, G3, dfs, dfp, tl, i, P, p_val, stat_list,stats, df, observed, ams, data_scans, a, c, j, lev, myData, pv, rwm, st)
# 2.5 Save the file  
write.csv(x=testchi, file= 'results_test_chi.csv', row.names = TRUE)

# Part 3. Join both tests into a single file----
testchi <- read.csv('results_test_chi.csv', header=FALSE, stringsAsFactors = FALSE)
testt <- read.csv('results_test_t.csv', header=FALSE, stringsAsFactors = FALSE)
testall <- rbind(testt, testchi); colnames(testall) <- c()
write.csv(x=testall, file= 'results_test_statistics.csv', row.names=FALSE)
rm(testall, testchi, testt)