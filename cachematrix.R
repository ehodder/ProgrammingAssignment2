## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeMatrix <- function(x = matrix()) {
  ## this is the mean example, needs to be switched to matrix operations!!
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

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  ## per instructions check if mean was calculated and the matrix hasn't changed
  ## double check the x$get won't blow up first time through
  ##  & all(x==x$get())
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
