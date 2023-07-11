# Codes are written in R studio
```r
# reading the data from web
wine <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",sep=",",header = F)
colnames(wine) <- c("Type","Alcohol","Malic acid",
                     "Ash","Alcalinity of ash",
                     "Magnesiun","Total phenols",
                     "Flavanoids","Nonflavanoid phenols",
                     "Proanthocyanins","Color intensity","Hue",
                     "OD280/OD315 of diluted wines","Proline"
                     )
```
```r
# writing the data to the system and then uploading for execution
write.csv(wine,file="wine.csv",row.names = F)

wine <- read.csv(file.choose())
View(wine)
```
```r
wssplot <- function(data,nc=15,seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers=i)$withinss)
  }
  windows()
  plot(1:nc,wss,type="b",xlab="Number of clusters",ylab="Within groups sum of squares")
}


df <- scale(wine[,-1])
wssplot(df)

install.packages("NbClust")
library(NbClust)
set.seed(1234)
devAskNewPage(ask=T)
nc <- NbClust(df,min.nc = 2,max.nc = 15,method="kmeans")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),xlab="Number of Clusters",ylab="Number of Criteria",
        main="Number of Clusters chosen by 26 Criteria")

set.seed(1234)
fit.km <- kmeans(df,3,nstart = 25)
fit.km$size
fit.km$centers
aggregate(wine[,-1],by=list(cluster=fit.km$cluster),mean)

ct.km <- table(wine$Type,fit.km$cluster)
ct.km
install.packages("flexclust")
library(flexclust)
randIndex(ct.km)

# Partitioning arround mediods
library(cluster)
set.seed(1234)
fit.pam <- pam(wine[,-1],k=3,stand=TRUE)
fit.pam$medoids
clusplot(fit.pam,main="Bivariate Cluster Plot")
ct.pam <- table(wine$Type,fit.pam$clustering)
ct.pam
randIndex(ct.pam)

# Avoiding Non-Existent clusters
install.packages("fMultivar")
library(fMultivar)
set.seed(1234)
df <- rnorm2d(1000,rho = .5)
df <- as.data.frame(df)
plot(df,main="Bivariate Normal Distribution with rho=0.5")
wssplot(df)
library(NbClust)
nc <- NbClust(df,min.nc = 2,max.nc = 15,method="kmeans")
dev.new()
barplot(table(nc$Best.n[1,]),xlab="Number of clusters",ylab="Number of Criteria",
        main="Number of Clusters chosen by 26 Criteria")

install.packages("ggplot2")
library(ggplot2)
library(cluster)
fit <- pam(df,k=2)
df$clustering <- factor(fit$clustering)

ggplot(data=df,aes(x=V1,y=V2,color=clustering,shape=clustering)) + 
  geom_point() + ggtitle("Clustering of Bivariate Normal Data")

plot(nc$All.index[,4],type="o",ylab="CCC",xlab="Number of Clusters",col="blue")
```
