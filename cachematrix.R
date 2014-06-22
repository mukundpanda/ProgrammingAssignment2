## Programming Assignment 2
## This file contains two functions that cache the inverse of a matrix. 

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL               
  set <- function(y) {                             ## Sets the value of the matrix
    x <<- y         
    m <<- NULL
  }
  get <- function() x                             ## Gets the value of the matrix
  setInverse <- function(solve) m <<- solve       ## Sets the value of the inverse matrix
  getInverse <- function() m                      ## Gets the value of the inverse matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()     ## Returns a matrix that is the inverse of the matrix passed in as argument x
  if(!is.null(m)) {       ## If there is a cache, message "getting cached data"
    message("getting cached data")
    return(m)       ## Return the cache
  }
  data <- x$get()         ## If there is no data
  m <- solve(data, ...)   ## Calculate the inverse of the matrix with the solve function
  x$setInverse(m)         ## Set the inverse matrix in x cache
  m                       ## Returns the result
}
