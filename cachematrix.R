## Put comments here that give an overall description of what your
## functions do

## The following function creates a "cached" matrix object, that has a built-in
## inverse function. the 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list (set = set, get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
  
}


## this function solves the inverse of the matrix that x encapsulates 
## and caches it within x's properties.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
