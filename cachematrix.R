## These functions calculate the inverse of a matrix, and caches the result for later use.

## First, makeCacheMatrix() accepts a matrix and returns a list of functions
## for that matrix.

## Next, cacheSolve() accepts that list and returns the cached inverse, if stored. If not,
## it calculates the inverse and stores it for later use.


## makeCacheMatrix is a function that can cache the inverse of a matrix passed to it

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function that first checks to see if the inverse of a given
## matrix exists in the cache. If so, it returns that value. If not, it calculates
## the inverse and returns it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
