# ------------------------------------------------------------
#Part 1 - Import the dataset.
iris
newiris <- iris
newiris$Species <- NULL
newiris

# ------------------------------------------------------------
# Part 2 - Elbow Method.
# make elbow plot to see how many clusters we should set
# Initialize total within sum of squares error: wss
twss = 0

# For 1 to 10 cluster centers
for (c in 1:10) {
  km.out <- kmeans(newiris, centers = c, nstart = 20)
  # Save total within sum of squares to wss variable
  twss[c] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
par(mar = c(5, 3.95, 2, 2))
plot(1:10, twss, type = "o",  
     xlab = "Number of Clusters", 
     ylab = "Total Within Sum of Squares",
     main= "Optimal Number of Clusters Using the Elbow Method", cex = 0.2)
# ------------------------------------------------------------
# Part 3 - Apply KMeans
# from the using the elbow method, we conclude that there are 3 clusters in the dataset. 
# apply the K means clustering algorithm to the data set and explore.
km = kmeans(newiris, 3)
km 
table(iris$Species, km$cluster)
# ------------------------------------------------------------
# Part 4 - Visualizing the Clusters
# create a function, that plots clusters using colour accessible for individuals with colour vision deficiency .
makeplot = function(title = ""){
  par(mar = c(4, 3.9, 2, 2))
  plot(newiris[c("Sepal.Length", "Sepal.Width")], col=c('#F0E442', '#D55E00', '#0072B2')[as.numeric(iris$Species)], pch=c(20,20,20), main=title, cex = 1.2) 
  points(km$centers[,c("Sepal.Length", "Sepal.Width")], col="#000000", pch=10, cex=2)
  legend("topleft",c("Cluster 1","Cluster 2", "Cluster 3"),cex=0.5,col=c('#F0E442', '#D55E00', '#0072B2'), pch=c(20,20,20), pt.cex = 2, bg="transparent", text.width=0.15, bty="n")
}
# plot 
makeplot("Iteration 1")

#create and plot pair plot
pairplot = pairs(newiris[,1:4], pch = 20,  cex = 0.5,
                 col = c("#F0E442", "#D55E00", "#0072B2")[km$centers],main="Pair Plot",
                 lower.panel=NULL)
pairplot

