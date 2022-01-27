setwd('D:/Spring_2021/AIT 580_Prof.Harry Foxwell/12. ML')

getwd()
# Install and load needed graphic and cluster packages

install.packages("ggplot2", repos='http://cloud.r-project.org')
install.packages("mclust", repos='http://cloud.r-project.org')
install.packages("cluster", repos='http://cloud.r-project.org')
library(ggplot2)
library(mclust)
library(cluster)

stateSAT <- read.csv('clean.csv', header=FALSE)
stateSAT
is.na(stateSAT)
#satmath vs satverbal scores
ggplot(data=stateSAT, aes(x = V4, y = V3)) + geom_point()+
  xlab("sat_math_scores") +
  ylab("sat_verbal_scores") +
  ggtitle("sat_math vs satverbal scores")+
  theme(plot.title = element_text(hjust = 0.5))
#  plot.title = element_text (hjust = 0.5)


plot(stateSAT$V4,stateSAT$V3)
#V1 region

plot(stateSAT$V4,stateSAT$V3,col=stateSAT$V1)
#V5  percent taking
plot(stateSAT$V4,stateSAT$V3,col=stateSAT$V5)
#V6 percentnohs
plot(stateSAT$V4,stateSAT$V3,col=stateSAT$V6)
#V7 teacher pay
plot(stateSAT$V4,stateSAT$V3,col=stateSAT$V7)
# V2 population
plot(stateSAT$V4,stateSAT$V3,col=stateSAT$V2)


set.seed(1010103)

km <- kmeans(stateSAT, centers=2, nstart=1)

stateSAT$cluster <- factor(km$cluster)
centers <- data.frame(cluster=factor(1:2), km$centers)

centers

png("stateSATCluster.png", width = 4, height=3, units='in', res=300)


ggplot(data=stateSAT, aes(x=V4, y=V3, color=cluster, shape=cluster)) +
  geom_point(alpha=.3) +
  scale_shape_manual(values = 1:2, guide = guide_legend(override.aes=aes(size=1))) + 
  geom_point(data=centers,  aes(x=V4, y=V3), size=2, stroke=2)


dev.off()  



#-------------
km <- kmeans(stateSAT, centers=3, nstart=1)

stateSAT$cluster <- factor(km$cluster)
centers <- data.frame(cluster=factor(1:3), km$centers)
# Display the coordinates of the centroids
centers

png("stateSATCluster.png", width = 6, height=5, units='in', res=300)

# Plot the data and the centroids; set the output parameters
ggplot(data=stateSAT, aes(x=V4, y=V3, color=cluster, shape=cluster)) +
  geom_point(alpha=.3) +
  scale_shape_manual(values = 1:3, guide = guide_legend(override.aes=aes(size=1))) + 
  geom_point(data=centers,  aes(x=V4, y=V3), size=2, stroke=2)  
dev.off()  # finish the graph (close the png image output device)




#-------------------
km <- kmeans(stateSAT, centers=5, nstart=1)

stateSAT$cluster <- factor(km$cluster)
centers <- data.frame(cluster=factor(1:5), km$centers)
# Display the coordinates of the centroids
centers

png("stateSATCluster.png", width = 4, height=3, units='in', res=300)

# Plot the data and the centroids; set the output parameters
ggplot(data=stateSAT, aes(x=V4, y=V3, color=cluster, shape=cluster)) +
  geom_point(alpha=.3) +
  scale_shape_manual(values = 1:5, guide = guide_legend(override.aes=aes(size=1))) + 
  geom_point(data=centers,  aes(x=V4, y=V3), size=2, stroke=2)  
dev.off()  # finish the graph (close the png image output device)



#-------------------
km <- kmeans(stateSAT, centers=6, nstart=1)

stateSAT$cluster <- factor(km$cluster)
centers <- data.frame(cluster=factor(1:6), km$centers)
# Display the coordinates of the centroids
centers

png("stateSATCluster.png", width = 4, height=3, units='in', res=300)

# Plot the data and the centroids; set the output parameters
ggplot(data=stateSAT, aes(x=V4, y=V3, color=cluster, shape=cluster)) +
  geom_point(alpha=.3) +
  scale_shape_manual(values = 1:6, guide = guide_legend(override.aes=aes(size=1))) + 
  geom_point(data=centers,  aes(x=V4, y=V3), size=2, stroke=2)  
dev.off()  # finish the graph (close the png image output device)

#---------------------------
km <- kmeans(stateSAT, centers=50, nstart=1)

stateSAT$cluster <- factor(km$cluster)
centers <- data.frame(cluster=factor(1:50), km$centers)
# Display the coordinates of the centroids
centers

png("stateSATCluster51_50.png", width = 6, height=5, units='in', res=300)

# Plot the data and the centroids; set the output parameters
ggplot(data=stateSAT, aes(x=V4, y=V3, color=cluster, shape=cluster)) +
  geom_point(alpha=.3) +
  scale_shape_manual(values = 1:50, guide = guide_legend(override.aes=aes(size=1))) + 
  geom_point(data=centers,  aes(x=V4, y=V3), size=2, stroke=2)  
dev.off()  # finish the graph (close the png image output device)





