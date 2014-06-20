## This module provides functions that allow multiple usage of the inverse of matrices
## efficiently by caching the inverse of the matrix and ideally not compute it for a single
# matrix more than once.

## Creates a CacheMatrix, which is a thin wrapper around an actual matrix that gives it 
## caching capability to be exploited by the function cacheSolve().
##
## Args:
##  x: The wrapped matrix
##
## Returns:
##  A CacheMatrix object
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  setter <- function(y) {
    x <<- y
    inversee <<- NULL
  }
  
  getter <- function() x
  
  getInverse <- function() inverse
  
  setInverse <- function(i) inverse <<- i
  
  list(set = setter, get = getter, getInverse = getInverse, setInverse = setInverse)
}


## Returns the inverse of a square matrix.
##
## Caching is used to prevent unnecessary multiple computations of the inverse matrix.
##
## Args:
##  x: A CacheMatrix created by makeCacheMatrix()
##  ...: Extra arguments that are passed to the solve() function internally
##
## Returns:
##  The inverse of the input matrix
##
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  if (!is.null(inverse)) {
    message('Getting the inverse from the cache')
  } else {
    inverse <- solve(x$get(), ...)
    x$setInverse(inverse)
  }
  
  return(inverse)
}
