### SCRIPT USED IN TFG OF LUCIA DE HOYOS GONZALEZ
# BACHELOR IN BIOMEDICAL ENGINEERING AT UC3M
# Madrid, June 2019
# Contact: luciadeh@gmail.com
# ===================================================== 
#
# Description:
#       this script plots the APC t test results
#        only of the significant ROIs
#
#       this process is done for each group matrix:
#         females and males in 3 age groups and
#           for the four parcellation scales
#           (in this script is done for scale 1)
#
# ===================================================== 
# Script to plot the brain with package 'ggseg'.
# For this script data from parcellation of laussane will be used.
# Scale 1

# Load packages
library(tidyverse)
library(ggseg)

# The csv must have 6 columns:
# - ROI: where the name of the region according to ggseg is written
# - Ft = Female t value
# - Fp = Female p value
# - Mt = Male t value
# - Mp = Male p value

# - Group = where the group is written (1,2,3)
laus1 <- read.csv("D:/ROIdata/DataScale1R.csv", stringsAsFactors = FALSE)
colnames(laus1) <- c("ROI", "Ft", "Fp", "Mt", "Mp", "Group")

# ggseg(mapping=aes(fill=area), position="stacked")

G1 <- subset(laus1, Group== "1")
G2 <- subset(laus1, Group== "2")
G3 <- subset(laus1, Group== "3")

G1res <- data.frame(area= rep(G1$ROI, 2), Sex = c(rep("1 Young Females",nrow(G1)),rep("1 Young Males",nrow(G1))) ,
                    t= c(G1$Ft, G1$Mt), p= c(G1$Fp, G1$Mp), stringsAsFactors = FALSE)
G2res <- data.frame(area= rep(G2$ROI, 2), Sex = c(rep("2 Middle Females",nrow(G2)),rep("2 Middle Males",nrow(G2))) ,
                    t= c(G2$Ft, G2$Mt), p= c(G2$Fp, G2$Mp), stringsAsFactors = FALSE)
G3res <- data.frame(area= rep(G3$ROI, 2), Sex = c(rep("3 Elder Females",nrow(G3)),rep("3 Elder Males",nrow(G3))) ,
                    t= c(G3$Ft, G3$Mt), p= c(G3$Fp, G3$Mp), stringsAsFactors = FALSE)

Gall <- rbind(G1res, G2res, G3res)

# T VALUES
Gall %>% 
  group_by(Sex) %>% 
  ggseg(atlas=dkt, colour="white", mapping=aes(fill=t), hemisphere = "left", size = 0.1) +
  facet_wrap(~Sex, ncol=3, dir = "v") +
  theme(legend.position = "bottom") +
  scale_fill_gradientn(colours = c("royalblue","firebrick","goldenrod"),na.value="grey")
# P VALUES
Gall %>% 
  group_by(Sex) %>% 
  ggseg(atlas=dkt, colour="white", mapping=aes(fill=p), hemisphere = "left") +
  facet_wrap(~Sex, ncol=3, dir = "v") +
  theme(legend.position = "bottom") +
  scale_fill_gradientn(colours = c("royalblue","firebrick","goldenrod"),na.value="grey")