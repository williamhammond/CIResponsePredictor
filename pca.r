# Author: William Hammond
# Date  : 5/2/2017

library(MASS)

# Generate random samples for testing
set.seed(10)

w1_sigma <- data.frame(c(1,0,0), c(0,1,0), c(0,0,1))
w1_mu <- c(0,0,0)
w1 <- data.frame(mvrnorm(n = 20, w1_mu, w1_sigma))

w2_sigma <- data.frame(c(1,0,0), c(0,1,0), c(0,0,1))
w2_mu <- c(1,1,1)
w2 <- data.frame(mvrnorm(n = 20, w2_mu, w2_sigma))

# Full data ste
dat <- merge(w1, w2, by = c("X1", "X2", "X3"), all=TRUE)

# Generate 3x3 scatter matrix for a given dataset
# Input:
#       df - dataframe of numeric values
scatter <- function(df) {
  col_means <- colMeans(df)
  scatter_mat <- data.frame(rep(0,3), rep(0,3), rep(0,3))
  for (i in 1:dim(df)[1]) {
    scatter_mat <- scatter_mat + (as.matrix(df[i,] - col_means) %x% 
                                    t(as.matrix(df[i,] - col_means)))
  }
  return(scatter_mat)
}

pca <- function(df, k) {
  eig <- eigen(scatter(df)) 
  eig_vector <- eig$vectors[,1:k]
  
  return (as.matrix(df) %*% as.matrix(eig_vector))
}

# Final transformation
transformed <- pca(dat, 2)
plot(transformed[1:20,], transformed[21:40,], col=c("blue", "red"))
plot(compare$x[1:20,], compare$x[21:40,], col=c("blue", "red"))

