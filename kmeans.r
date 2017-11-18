# File  : kmeans.r
# Author: William Hammond
# Date  : 4 / 29 / 2017

library(fossil)
library(dplyr)

# Helper function used to created mxm distance matrix that represents the 
# distance from one point to all the others
# param
#   x = m x 3 matrix
create_distance_matrix <- function(x) {
  # Compare every point to ever other point, if there arent any other close
  # points, add the index of the point to an array of point sto be removed
  dists <- apply(x, 1, function(point) {
    sqrt(rowSums((x - as.list(point))^2))
  })
  
  # Sort the rows in ascending order
  sorted_dist <- t(apply(dists,1,sort))
}
# Removes noise from a matrix
remove_noise <- function (x) {
  num_rows <- dim(x)[1]
  sorted_dist <- create_distance_matrix(x)
  
  # Iterate over rows, if the average distance of the 10 closest point is
  # less than 10, save index
  for (i in seq(1:num_rows)) {
    curr_dist <- sorted_dist[i,]
    for (j in seq(1:10)) {
      if (curr_dist[j] > 0.1) {
        x <- x[-i,] # deletes a row
        break
      }
    }
  }
  result <- x
}
# Function that computes the distance from every point in a  matrix to a 
# centroid, if the new distance found is smaller than the currently saved 
# distance, the centroid label of that point is updated
# param
#   dat = matrix where: 
#                colomn 1 = x
#                colomn 2 = y
#                colomn 3 = centroid label
#                colomn 4 = distance to that centroid'
#   centroid = matrix of centroid points
compute_centroid_distance <- function(dat,centroids) {
  # we find the number of centroids 
  K = dim(centroids)[1]
  # point is the row of dat
  dat <- as.data.frame(t(apply(dat, 1, function (point) {
    # We iterate over all the centroids
    for (i in 1:K){
      distFromCentroid <- sqrt(rowSums((centroids[i,] - point[1:2])^2))
      if (distFromCentroid < point[4]) {
        point[4] <- distFromCentroid
        point[3] <- i
      }
    }
    # Apply stores the last computed value, so we return the altered point
    # This maybe why the meanx of the centroid is not being updated
    point
  })))
  result <- dat
}

# Computes the kmean of a matrix
# param
#   dat = matrix of points
#   K = numbers of clusters
#   iter = number of iterations
compute_kmeans <- function (dat, K = 5, iter = 10) {
  # We even distribute across the x axis for our starting points
  maxX  <- max(dat$x)
  minX  <- min(dat$x)
  stepX <- ceiling(abs(minX - maxX) / K)
  xs <- seq (minX, maxX, stepX )
  # We choose a middle Y value for our y coordinate
  ys <- rep(mean(dat$y), K)
  # Bind these values into a centroid matrix
  centroids <- as.data.frame(cbind(xs,ys))
  colnames(centroids) <- c('x','y')
  # Colomn to be added to data to represent what centroid it currently 
  # belongs to
  centroid_label <- rep (0, dim(dat)[1])
  # We tract the current best distance to a centroid for a given point
  centroid_dist <- rep (Inf, dim(dat)[1])
  # Add these new vectors to our dataframe
  dat <- cbind(dat, centroid_label, centroid_dist)
  # Compute distance from every data point to every centroid
  dat <- compute_centroid_distance(dat,centroids)
  for (i in 1:iter) {
    # Compute the means to find the new centroids
    xmeans <- (dat %>% group_by(centroid_label) %>% summarise_each(funs(mean), x))$x            
    ymeans <- (dat %>% group_by(centroid_label) %>% summarise_each(funs(mean), y))$y
    for (i in 1:K) {
      centroids[i,]$x <- xmeans[i]
      centroids[i,]$y <- ymeans[i]
    }
    compute_centroid_distance(dat,centroids)
  }
  output <- list(dat,centroids)
  return (output)
}

# Computes the SSE of the kmeans clustering
compute_SSE <- function (dat, centroids, K) {
  # intialize total error 
  error <- 0
  # we iterate over ever cluster
  for (i in 1:K) {
    centroid <- centroids[i,]
    cluster <- dat[dat$centroid_label==i,]
    # we iterate over every point in the cluster
    for (i in 1:dim(cluster)[1]){
      point <- cluster[i,]
      # we sume the error
      error = error + sqrt(rowSums((centroid - point[1:2])^2)) ^ 2
    }
  }
  result <- error
}