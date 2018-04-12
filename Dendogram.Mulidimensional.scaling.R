# Cluster Dendogram
# Creator: CGBoer
# April 2018
# Version 1.0 

### Loading and preparing data
data <- read.table("data.txt", header=T, row.names = 1, strings=F)

# Preparing data
# make sure data is a numeric matrix
class(data)
scale_data  <- scale(data) # standardize variables 
mydata      <- t(scale_data) # for distsance matrix, ditance matrix clacluated dist between rows

# Ward Hierarchical Clustering - euclidean distances
d <-dist(mydata, method = "euclidean") # distance matrix
w <-dist(mydata, method = "Ward.D2") # distance matrix 

### Plot clustering 
# Dendrogram
hcd = as.dendrogram(d)
par(ann = F, xaxt="n", cex.lab = 1.25, col.lab = "#7C8071")
plot(hcd, axes = F)
axis(side = 2, at = seq(0, 450, 50), col = "#F38630", 
     labels = FALSE, lwd = 2)
mtext(seq(0, 450, 50), side = 2, at = seq(0, 450, 50), line = 1, 
      col = "#A38630", las = 2)

# Multidimensional scale
library(calibrate)
fit <- cmdscale(d, eig=TRUE, k=2)
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Dimension 1", ylab="Dimension 2", xlim=c(-100,100), ylim=c(-100,100))
textxy(x, y, names(data), cex=1.2)

