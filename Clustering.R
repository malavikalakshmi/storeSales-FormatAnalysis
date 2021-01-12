library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(clusterCrit)
Sales <- read_xlsx("Sales.xlsx")
newmodsales <- data.frame(Sales)
newmodsales <- newmodsales %>% filter(Year %in% (2015))
newmodfinalsales = newmodsales %>%
dplyr::select(Percent_Dry,Percent_Dairy,Percent_Frozen,Percent_Meat,Percent_Produce,Percent_Floral,Percent_Deli,Percent_Bakery,Percent_GenMerch)
newmodfinalsales <- data.matrix(newmodfinalsales)

k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kmeans(na.omit(newmodfinalsales), k, nstart=50, iter.max = 20)$tot.withinss})
 

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


seg.k2 <- kmeans(newmodfinalsales, centers=2, nstart=50, iter.max=20)
seg.k3 <- kmeans(newmodfinalsales, centers=3, nstart=50, iter.max=20)
seg.k4 <- kmeans(newmodfinalsales, centers=4, nstart=50, iter.max=20)
seg.k5 <- kmeans(newmodfinalsales, centers=5, nstart=50, iter.max=20)

k2 = intCriteria(newmodfinalsales, seg.k2$cluster, "Calinski_Harabasz")
k3 = intCriteria(newmodfinalsales, seg.k3$cluster, "Calinski_Harabasz")
k4 = intCriteria(newmodfinalsales, seg.k4$cluster, "Calinski_Harabasz")
k5 = intCriteria(newmodfinalsales, seg.k5$cluster, "Calinski_Harabasz")

print(paste("F-stat for 2 clusters is", k2))
print(paste("F-stat for 3 clusters is", k3))
print(paste("F-stat for 4 clusters is", k4))
print(paste("F-stat for 5 clusters is", k5))

x<-seg.k3$cluster
store = newmodsales %>%
  dplyr::select(Store)
final <- cbind(store,x)
write.csv(x, file="ClustersFinal.csv")


print(seg.k3$betweenss/seg.k3$totss*100)


# Visualising the cluster Center
plot(newmodfinalsales, col =seg.k3$cluster, main = "K-Means with 3 clusters")
seg.k3$centers
points(seg.k3$centers, col = 4:5,pch = 8, cex=3)#cex = font size, pch = symbol


  