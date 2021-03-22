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
#       package used -> circlize 
#
# ===================================================== 

library(tidyverse); library(igraph); library(circlize)
library(igraph)
# Create a circular plot for "young", "middle" and "old.
rm(list = ls()) # clear the workspace again

for (group in 1:3){
  
  nodes <- read.csv2(paste0("D:/diagram/group", group, "nodes.csv"), header=T, as.is=T)
  links <- read.csv2(paste0("D:/diagram/group", group, "links.csv"), header=T, as.is=T)
  colnames(nodes)[1]<- "id";  colnames(links)[1]<- "from"
  links$to <- nodes$id[match(links$to, nodes$media)]
  links$from <- nodes$id[match(links$from, nodes$media)]
  
  links <- links[order(links$from, links$to),]
  colnames(links)[3] <- "weight"
  rownames(links) <- NULL
  nodes2 <- nodes[order(nodes$media.type),]
  net <- graph_from_data_frame(d=links, vertices=nodes2, directed=T) 
  
  # Generate colors based on the love:
  colrs <- c("darkseagreen1", "lightblue1", "bisque", "lightpink")
  V(net)$color <- colrs[V(net)$media.type]
  
  # Create the net for each age group
  net.s1 <- net - E(net)[!E(net)$type=="scale1"] 
  net.s2 <- net - E(net)[!E(net)$type=="scale2"] 
  net.s3 <- net - E(net)[!E(net)$type=="scale3"]
  net.s4 <- net - E(net)[!E(net)$type=="scale4"]
  # 
  # # Plot them
  # l <- layout_in_circle(net)
  # if  (is.null(dev.list()) == FALSE){dev.off()}
  # png(file = paste0("D:/diagram/correlogram/Group ",group, '_S1.png'),  width = 2000, height = 2000, units = "px", pointsize = 30)
  # plot(net.s1, layout=l, main=paste0("Group ", group, " S1"), edge.arrow.size=0, edge.curved=0)
  # if  (is.null(dev.list()) == FALSE){dev.off()}
  # png(file = paste0("D:/diagram/correlogram/Group ",group, '_S2.png'),  width = 2000, height = 2000, units = "px", pointsize = 30)
  # plot(net.s2, layout=l, main=paste0("Group ", group, " S2"), edge.arrow.size=0, edge.curved=0)
  # if  (is.null(dev.list()) == FALSE){dev.off()}
  # png(file = paste0("D:/diagram/correlogram/Group ",group, '_S3.png'),  width = 2000, height = 2000, units = "px", pointsize = 30)
  # plot(net.s3, layout=l, main=paste0("Group ", group, " S3"), edge.arrow.size=0, edge.curved=0)
  # if  (is.null(dev.list()) == FALSE){dev.off()}
  # png(file = paste0("D:/diagram/correlogram/Group ",group, '_S4.png'),  width = 2000, height = 2000, units = "px", pointsize = 30)
  # plot(net.s4, layout=l, main=paste0("Group ", group, " S4"), edge.arrow.size=0, edge.curved=0)
  # if  (is.null(dev.list()) == FALSE){dev.off()}
  # # legend(x=-1.1, y=-1.1, c("Frontal","Parietal", "Temporal", "Occipital"), pch=21,
  # #        col="#777777", pt.bg=colrs, pt.cex=2.5, bty="n", ncol=1)
  
  
  # Plot them
  l <- layout_in_circle(net)
  if  (is.null(dev.list()) == FALSE){dev.off()}
  jpeg(file = paste0("D:/diagram/correlogram/Group ",group, '_S1.png'),  width = 1000, height = 1000, units = "px", pointsize = 20)
  plot(net.s1, layout=l, edge.arrow.size=0, edge.curved=0, 
       vertex.label.cex= 1.5,vertex.size=25 ,vertex.label.color="black",  vertex.label.font=14, edge.color="gray8")
  if  (is.null(dev.list()) == FALSE){dev.off()}
  jpeg(file = paste0("D:/diagram/correlogram/Group ",group, '_S2.png'),  width = 1000, height = 1000, units = "px", pointsize = 20)
  plot(net.s2, layout=l, edge.arrow.size=0, edge.curved=0,
       vertex.label.cex= 1.5,vertex.size=25 ,vertex.label.color="black",  vertex.label.font=14, edge.color="gray8")
  if  (is.null(dev.list()) == FALSE){dev.off()}
  jpeg(file = paste0("D:/diagram/correlogram/Group ",group, '_S3.png'),  width = 1000, height = 1000, units = "px", pointsize = 20)
  plot(net.s3, layout=l,  edge.arrow.size=0, edge.curved=0, 
       vertex.label.cex= 1.5,vertex.size=25 ,vertex.label.color="black",  vertex.label.font=14, edge.color="gray8")
  if  (is.null(dev.list()) == FALSE){dev.off()}
  jpeg(file = paste0("D:/diagram/correlogram/Group ",group, '_S4.png'),  width = 1000, height = 1000, units = "px", pointsize = 20)
  plot(net.s4, layout=l,  edge.arrow.size=0, edge.curved=0, 
       vertex.label.cex= 1.5,vertex.size=25 ,vertex.label.color="black",  vertex.label.font=14, edge.color="gray8")
  if  (is.null(dev.list()) == FALSE){dev.off()}
  # legend(x=-1.1, y=-1.1, c("Frontal","Parietal", "Temporal", "Occipital"), pch=21,
  #        col="#777777", pt.bg=colrs, pt.cex=2.5, bty="n", ncol=1)
  
}
