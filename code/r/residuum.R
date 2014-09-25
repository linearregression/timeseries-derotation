# Rainhard Findling
# u'smile Lab, University of Applied Sciences Upper Austria
# 2014/06
# 
# R implementation of quaternion derotation "residuum"
# use "residuum(3D_timeseries_1, 3D_timeseries_2)" to derotate 
# 3D_timeseries_1 towards best fitting 3D_timeseries_2.
# 
qmul <- function(p,q) {
  a0 <- p[1]
  a <- p[2:4]
  b0 <- q[1]
  b <- q[2:4]
  x1 <- a0 * b0  - crossprod(a,b)
  x2 <- a0*b + b0*a + c(a[2]*b[3]-a[3]*b[2], a[3]*b[1]-a[1]*b[3], a[1]*b[2]-a[2]*b[1])
  c(x1,x2)
}

rotquat <- function(p,u) {
  uc <- c(u[1], -u[2:4])
  up <- qmul(u,p)
  upuc = qmul(up,uc)
  upuc
}

quat <- function(phi,axis) {
  a <- axis/sqrt(sum(axis**2))
  u = c(cos(phi/2), sin(phi/2)*a)
  u
}

reser <- function(X,Y,u) {
  e <- 0
  p <- data.frame(x=numeric(0),y=numeric(0),z=numeric(0))
  for(k in 1:dim(X)[[2]]) {
    y <- rotquat(c(0, X[,k]), u)    #rotate the x vectors
    p <- rbind(p, y[2:4])           #store in P=UX
    e <- e + sum((Y[,k]-p[k,])**2)  #sum of norms of differences
  }
  e <- e/dim(X)[[2]]
  list(error=e, derotated=p)
}

residuum <- function(X,Y) {
  # X and Y parameters are dataframes with N obervations of 3 variables
  # internally we work with transposed representation
  X <- t(X)
  Y <- t(Y)
  # correlation matrix
  R <- as.matrix(X) %*% as.matrix(t(Y))
  # matrix holding all rotations
  f <- matrix(c(R[1,1]+R[2,2]+R[3,3], R[2,3]-R[3,2], R[3,1]-R[1,3], R[1,2]-R[2,1],
                R[2,3]-R[3,2], R[1,1]-R[2,2]-R[3,3], R[1,2]+R[2,1], R[1,3]+R[3,1],
                R[3,1]-R[1,3], R[1,2]+R[2,1], -R[1,1]+R[2,2]-R[3,3], R[2,3]+R[3,2],
                R[1,2]-R[2,1], R[1,3]+R[3,1], R[2,3]+R[3,2], -R[1,1]-R[2,2]+R[3,3]), nrow=4)
  # compute eigenvector evvmax of largest eigenvalue ev
  eigens <- eigen(f)
  ev = eigens$values[[1]]
  evvmax = eigens$vectors[,1]
  for(i in 2:4) {
    if(eigens$values[[i]]>ev) {
      ev <- eigens$values[[i]]
      evvmax <- eigens$vectors[,i]
    }
  }
  reser(X,Y,evvmax) #compute error and optimal P=UX  
}


