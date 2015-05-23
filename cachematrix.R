## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix and create a cached version of it.
## The cached version provide setters and getters for the original
## matrix as well as its inverse. There might be no inverse if the
## matrix was just changed and cacheSolve was not yet called.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve takes a cached matrix created with makeCacheMatrix
## and return either the cached version of its inverse or computes
## it if none is available. Optionally argument can be passed and
## will be forwarded to solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if (!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
