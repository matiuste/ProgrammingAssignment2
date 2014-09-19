## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly. 
## The following two functions are used to calculate the inverse of a matrix. Saving the result in the cache 
## Only works for matrix that have an inverse

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## Computing the inverse of a square matrix can be done with the solve function in R. For example, 
   ## if X is a square invertible matrix, then solve(X) returns its inverse.
  m <- solve(data)
  x$setinverse(m)
  m
}

##Some examples of using the two functions are described below, getting the inverse of a matrix
Example<-matrix(c(0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,1,0,0,0,0), nrow=5, ncol=5)
c<-makeCacheMatrix(Example)
#Inverse
cacheSolve(c)
a_1<-cacheSolve(c)
#Identity
Example %*% a_1

Example<-matrix(c(1,1,0,1,0,1,0,1,0), nrow=3, ncol=3)
c<-makeCacheMatrix(Example)
#Inverse
cacheSolve(c)
a_1<-cacheSolve(c)
#Identity
Example %*% a_1

Example<-matrix(c(2,3,5,0,0,1,1,0,1), nrow=3, ncol=3)
c<-makeCacheMatrix(Example)
#Inverse
cacheSolve(c)
a_1<-cacheSolve(c)
#Identity
Example %*% a_1

Example<-matrix(c(1,0,1,2,4,0,3,5,6), nrow=3, ncol=3)
c<-makeCacheMatrix(Example)
#Inverse
cacheSolve(c)
a_1<-cacheSolve(c)
#Identity
Example %*% a_1

Example<-matrix(c(4,3,3,2), nrow=2, ncol=2)
c<-makeCacheMatrix(Example)
#Inverse
cacheSolve(c)
a_1<-cacheSolve(c)
#Identity
Example %*% a_1
