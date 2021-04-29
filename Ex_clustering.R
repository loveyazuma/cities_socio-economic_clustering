# --------------------------------------------------------------------------------------------
# Setup and data preparation for the 2D dataset
# --------------------------------------------------------------------------------------------

# clear previous variables
rm(list=ls())

# import packages or load distmat.R
#library(pracma) # Practical Math operations, like distmat to compute a distance matrix
source('distmat.R')

# Load and visualize the data
Data2D <- read.csv(file="data2d.csv", header=FALSE, sep=",")
plot(Data2D[,1], Data2D[,2] ,xlab="Variable 1",ylab="Variable 2")


# 1.1 Initialization of K-means

# Choose K points randomly from the dataset to initialize the cluster centroids.
# Store them as a matrix of size [K, dim] and call it cluCentroids.
K <- 4 #Number of clusters
################################
# R1
Data2D <- scale(Data2D)
set.seed(9)
init_indeces <- sample(x= nrow(Data2D), size = K)
cluCentroids <- Data2D[init_indeces,]
################################

# 1.2 Assign each data point to the nearest cluster centroid

# Assign each data point to a cluster centroid.
################################
# R2
D <- distmat(as.matrix(Data2D), as.matrix(cluCentroids))
assigned_clusterIDs <- apply(D, 1, which.min)
  #? for each data point, find the index of nearest cluster
################################

# this plot allows to see the cluster assignment
plot(Data2D[,1], Data2D[,2], col = palette()[assigned_clusterIDs])
points(cluCentroids[,1], cluCentroids[,2], col = palette(),pch=19,cex=2)

# 1.3: Update the centroids
################################
# R3
for (i in 1:K) {
  cluCentroids[i,] <- apply(Data2D[assigned_clusterIDs == i,], 2, mean)
    }
################################

# 1.4: Write kmeans and compare it to R's native kmeans
################################
# R3
source("my_kmeans.R")
# Using R's native kmeans
clustering <- kmeans(Data2D, K, nstart = 20)
assigned_clusterIDs <- clustering$cluster
cluCentroids <- clustering$centers
# Using my kmeans implementation
my_clustering <- my_kmeans(Data2D)
my_assigned_clusterIDs <- my_clustering$cluster
my_cluCentroids <- my_clustering$centers
################################

# Plot the clustering results of both algorithms
par(mfrow=c(1, 1))
plot(Data2D[,1], Data2D[,2], col = palette()[assigned_clusterIDs], main = "Built-in kmeans()")
points(cluCentroids[,1], cluCentroids[,2], col = palette(),pch=19,cex=2)

plot(Data2D[,1], Data2D[,2], col = palette()[my_assigned_clusterIDs], main = "Customised kmeans()")
points(my_cluCentroids[,1], my_cluCentroids[,2], col = palette(),pch=19,cex=2)

# --------------------------------------------------------------------------------------------
# Setup and data preparation for the world cities dataset
# --------------------------------------------------------------------------------------------

 

# import packages
if(!require(xlsx)){
  install.packages("xlsx")
}
require(xlsx)

# load the 8D data into a data frame
AllDataCities <- read.xlsx("oecd_cities_stats.xlsx", sheetName = "OECD.Stat export")

# extract a subset of the data frame to remove unwanted rows and columns
DataCities<-subset(AllDataCities,NA..9!='..' & NA..15!='..',select=c(NA..1,NA..3,NA..5,NA..7,NA..9,NA..11,NA..13,NA..15))

# extract the country codes correspoinding to each city
Cities<-subset(AllDataCities,NA..9!='..' & NA..15!='..',select=FALSE.)
Cities <- apply(Cities,2,as.character)
CountryCodes<-apply(Cities,1,function(x) substr(strsplit(x,": ")[[1]][1],3, 4))

# load a Look-up-table containing the contry codes, names and world regions
CountryLUT <- read.xlsx("oecd_cities_stats.xlsx", sheetName = "Countries")

# extract the coutry names and change the variable names
CountryNames<-matrix(sapply(CountryCodes,function(x) CountryLUT[1,which(x==names(CountryLUT))]))
RegionNames<-matrix(sapply(CountryCodes,function(x) CountryLUT[2,which(x==names(CountryLUT))]))
CityNames<-apply(Cities,1,function(x) strsplit(x,": ")[[1]][2])
VarNames<-c('Population','Youth dep. ratio', 'Old-age dep. ratio','Density','Green area per cap.','Core concentration', 'P2.5 pollution','Unemplyment')
names(DataCities)<-VarNames
rownames(DataCities)<-CityNames

# --------------------------------------------------------------------------------------------
# 2. Visually explore the world cities dataset
# --------------------------------------------------------------------------------------------


pairs(DataCities,col=sapply(RegionNames,FUN=function(x) which.max(unique(RegionNames) == x)),oma=c(3,3,5,15),pch=19)
par(xpd=TRUE)
legend(0.87, 0.8, as.vector(unique(RegionNames)),  fill=palette() )

# 2.1: Run kmeans on the data
################################
# R5
DataCities<-apply(DataCities,2,as.numeric)
DataCities.scale <- scale(DataCities)
K<- c(3:5)
## clustered without scaled values
for (k in K) {
  clustering <-kmeans(DataCities, k, nstart=20) #? get the clustering of the cities
  ################################
  # Plot the cluster centroids as barplots
  par(mfrow=c(2, ceiling(k/2)))
  for (i in 1:k) {
    barplot(clustering$centers[i,]-colMeans(clustering$centers),names.arg=VarNames,las=2)
    }
}

## clustered with scaled values
for (k in K) {
  clustering <-kmeans(DataCities.scale, k, nstart=20) #? get the clustering of the cities
  ################################
  # Plot the cluster centroids as barplots
  par(mfrow=c(2, ceiling(k/2)))
  for (i in 1:k) {
    barplot(clustering$centers[i,]-colMeans(clustering$centers),names.arg=VarNames,las=2)
  }
}

# 2.2: Check the regional proportions in each cluster
################################
# R6
par(mfrow=c(2, ceiling(5/2)))

for (i in 1:5) {
  pie(table(RegionNames, clustering$cluster)[,i])
}
################################

# 2.3: Use Principal Component Analysis to see how varaibles as grouped together
################################
# R7
pca<- prcomp(DataCities.scale)#? PCA of DataCities
source('ggbiplot.R')
#? use ggbiplot to visualize.
ggbiplot(pca)
################################




