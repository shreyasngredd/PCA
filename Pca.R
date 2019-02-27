#####PRINCIPAL COMPONENT ANALYSIS#####

# Perform Principal component analysis and perform clustering using first 
# 3 principal component scores(both heirarchial and k mean clustering
# (scree plot or elbow curve) and obtain optimum number of clusters and 
# check whether we have obtained same number of clusters with the original data 
# frame

#Loading the data
wine<-read.csv(file.choose()) 
View(wine)

library(dplyr)
wine<- select(wine, -Type)
attach(wine)
View(wine)

#Summerize the data
summary(wine)
#Alcohol: Mean= 13.00, Median= 13.05; As Mean<Median,it is skewed to the right.
#Malic: Mean= 2.336, Median= 1.865;  As Mean>Median,it is skewed to the left.
#Ash: Mean= 2.367, Median= 2.360;  As Mean>Median,it is skewed to the left.
#Alcalinity: Mean= 19.49, Median= 19.50;  As Mean<Median,it is skewed to the right.
#Magnesium: Mean= 99.74, Median= 98.00;  As Mean>Median,it is skewed to the left.
#Phenols: Mean= 2.295, Median= 2.355;  As Mean>Median,it is skewed to the left.
#Flavanoids: Mean= 2.029, Median= 2.135;  As Mean<Median,it is skewed to the right.
#Nonflavanoids: Mean= 0.3619, Median= 0.3400;  As Mean>Median,it is skewed to the left.
#Proanthocyanins: Mean= 1.591, Median= 1.555;  As Mean>Median,it is skewed to the left.
#Color: Mean= 5.058, Median= 4.690;  As Mean>Median,it is skewed to the left.
#Hue: Mean= 0.9574 , Median= 0.9650;  As Mean>Median,it is skewed to the left.
#Dilution: Mean= 2.612, Median= 2.780;  As Mean<Median,it is skewed to the right.
#Proline: Mean= 746.9, Median= 673.5;  As Mean>Median,it is skewed to the left.

library(DataExplorer)
plot_str(wine)
str(wine)
plot_missing(wine)

#Visualizations
dev.new(width=5, height=4)
plot_histogram(wine)
plot_density(wine)

cor(wine)

#PCA summary
pca_wine<- princomp(wine,cor=TRUE,scores = TRUE, covmat = NULL)
summary(pca_wine)
str(pca_wine)
loadings(pca_wine)

plot(pca_wine)
biplot(pca_wine)

#Showing the increase of variance with considering principal components
plot(cumsum(pca_wine$sdev*pca_wine$sdev)*100/(sum(pca_wine$sdev*pca_wine$sdev)),type="b")
pca_wine$score[,1:3]

#Binding top three scores
wine<-cbind(wine,pca_wine$scores[,1:3])
View(wine)

#####Cluster Analysis- All Variables#####
#Hierarchical Clustering
clus_data<-wine[,8:10]

# Normalizing the data 
norm_clus<-scale(clus_data)
dist1<-dist(norm_clus,method = "euclidean")

fit1<-hclust(dist1,method="complete")

#Displaying Dendrogram
plot(fit1) 
rect.hclust(fit1, k=7, border="red")

# Cutting the dendrogram for 7 clusters
groups<-cutree(fit1,7) 

#cluster numbering
membership_1<-as.matrix(groups)  
View(membership_1)

final1<-cbind(membership_1,wine) 
View(final1)
View(aggregate(final1[,-c(2,16:18)],by=list(membership_1),FUN=mean)) 
write.csv(final1,file="wine_cluster.csv",row.names = F,col.names = F)

#K-means Clustering
library(plyr)
str(final1)

normalized_data<-scale(final1[,15:17])

# Determine number of clusters by scree-plot
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))      
for (i in 1:7) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
title(sub = "K-Means Clustering Scree-Plot")
#The number of clusters=7

# 7 cluster solution
library(factoextra)
fit <- eclust(normalized_data, "kmeans", k = 7, nstart = 25, graph = FALSE) 
fviz_cluster(fit, geom = "point", frame.type = "norm")

#Append cluster membership
final2<- data.frame(fit$cluster,final1) 
View(final2)
write.csv(final2,file="wine_kmcluster.csv",row.names = F,col.names = F)

aggregate(final1[,2:17], by=list(fit$cluster), FUN=mean)
table(fit$cluster)

