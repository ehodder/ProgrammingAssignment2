## These functions provide a way to create and cache an inverse of a matrix
## so that the inverse only has to be calculated once. Create a new matrix object
## by calling makeMatrix with the original matrix then call cacheSolve with the new
## matrix object to get the inverse

## makeMatrix creates a matrix object with several functions attached which
## provide set and get routines for the original matrix and its inverse and 

makeMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## returns a cached inverse of the matrix, otherwise creates the inverse and caches it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
