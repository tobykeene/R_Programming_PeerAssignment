## cachematrix.R
##
## Create a cache matrix object that can be used to
## repeatably solve the inverse of the marix, but only
## calculates the inverse once.
##


## Create a cacheMatrix object for an invertable matrix.

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL    ## Set the cache to NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL ##Reset cache to NULL when new matrix set
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse  ## Caches the result of CacheSolve
  getInverse <- function() cachedInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Return the inverse of an cacheMatrix object

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  invFunc <- x$getInverse()
  ## Check if inverse not already calculated and matrix unchanged
  if(!is.null(invFunc) & identical(x$set, x)) {
    message("getting cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data)
  x$setInverse(invFunc)
  invFunc
}
