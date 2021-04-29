my_kmeans <- function(x,K=4,maxiter=10,do_plot=FALSE)
{
  # import packages
  library(pracma) # Practical Math operations, like distmat to compute a distance matrix
  
  ################################
  # R4
  # initialize
  set.seed(9)
  indeces <- sample(x= nrow(x), size = K)
  cluCentroids <- x[indeces,]
  D <- distmat(as.matrix(x), as.matrix(cluCentroids))
  assigned_clusterIDs <- apply(D, 1, which.min)
  
  if (do_plot) {
    plot(x[,1], x[,2], col = palette()[assigned_clusterIDs])
    points(cluCentroids[,1], cluCentroids[,2], col = palette(),pch=19,cex=2)
    Sys.sleep(0.5)
    print(0)
  }
  
  for (i in 1:maxiter) {
    D <- distmat(as.matrix(x), as.matrix(cluCentroids))
    assigned_clusterIDs <- apply(D, 1, which.min)
    for (k in 1:K) {
      cluCentroids[k,] <- apply(x[assigned_clusterIDs == k,], 2, mean)
    }
    ################################
    if (do_plot) {
      plot(x[,1], x[,2], col = palette()[assigned_clusterIDs])
      points(cluCentroids[,1], cluCentroids[,2], col = palette(),pch=19,cex=2)
      Sys.sleep(0.5)
      print(i)
    }
  }
  out <- {}
  out$cluster <- assigned_clusterIDs
  out$centers <- cluCentroids
  return(out)  
  }
