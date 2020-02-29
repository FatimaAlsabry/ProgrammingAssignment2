## the pair functions are used to cache the inverse of a matrix.
## assume that the matrix supplied is always invertible.
## makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



cacheSolve <- function(x, ...) {
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
  
  inv <- x$getinverse()
  
  ##If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache.
  if(!is.null(inv)) {
    inv
  }
  data <- x$get()
  inv <- solve(data)##Computing the inverse of a square matrix 
  x$setinverse(inv)
  inv
}

## Sample run:
## > x = cbind(c(1, 2), c(3, 4))
##> m = makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(m)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5