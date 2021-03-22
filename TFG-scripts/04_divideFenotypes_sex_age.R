### SCRIPT USED IN TFG OF LUCIA DE HOYOS GONZALEZ
# BACHELOR IN BIOMEDICAL ENGINEERING AT UC3M
# Madrid, June 2019
# Contact: luciadeh@gmail.com
# ===================================================== 
#
# Description:
#       In this script the fenotypes are divided into 
#         the three age groups (optimal solution)
#
#       To divide the sample into groups the package 
#         'cut2' is used.
#
# ===================================================== 

library(Hmisc) # cut2
setwd('D:/lucia_TFG/01-PreparedFiles/')
df <- DF <- read.csv('fenotypes_final.csv', header=TRUE)
df <- data.frame(Study.ID= df$subID, MeanAge= df$age_mean, scode= df$scode); ran <- range(df$MeanAge) 

# ***********************************************
thr <- 45 # threshold to separate G1G2 from G3
# ***********************************************

# Divide groups by threshold
dfG1G2 <- df[which(df$MeanAge < thr),]; dfG3 <- df[which(df$MeanAge >= thr),]

# Divide G1 and G2 with cut2
A <- split(dfG1G2, cut2(dfG1G2$MeanAge, g=2)); dfG1 <- A[[1]]; dfG2 <- A[[2]]

# Get the info of each group and save it
sdp1 <- data.frame(table(dfG1$scode)); sdp2 <- data.frame(table(dfG2$scode)); sdp3 <- data.frame(table(dfG3$scode))
B <- data.frame(G1= c(sdp1$Freq[1], sdp1$Freq[2]), G2= c(sdp2$Freq[1], sdp2$Freq[2]), G3= c(sdp3$Freq[1],sdp3$Freq[2]))
min <-  c(range(dfG1$MeanAge)[1], range(dfG2$MeanAge)[1], range(dfG3$MeanAge)[1])
max <- c(range(dfG1$MeanAge)[2], range(dfG2$MeanAge)[2], range(dfG3$MeanAge)[2])
B <- rbind(B, c(nrow(dfG1), nrow(dfG2), nrow(dfG3)), min, max, max-min)
rownames(B) <- c("F", "M", "nSub", "min", "max", "difference")

write.table(x= t(B), file= 'InfoGroups.txt', quote = FALSE)
rm(A, B, df, dfG1, dfG1G2, dfG2, dfG3, sdp1, sdp2, sdp3, max, min, ran)

dfG1G2 <- DF[which(DF$age_mean < thr),]; dfG3 <- DF[which(DF$age_mean >= thr),]
A <- split(dfG1G2, cut2(dfG1G2$age_mean, g=2)); dfG1 <- A[[1]]; dfG2 <- A[[2]]

df2 <- data.frame(DF, gcode = 0)
df2$gcode[match(dfG1$subID, df2$subID)] <- 1
df2$gcode[match(dfG2$subID, df2$subID)] <- 2
df2$gcode[match(dfG3$subID, df2$subID)] <- 3
write.csv(df2, 'fenotypes_APC.csv', row.names = FALSE)

rm(A, DF, df2, dfG1, dfG1G2, dfG2, dfG3, thr)