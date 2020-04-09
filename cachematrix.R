## Both functions can be used to compute the inverse of a matrix and
## save it in the cache to reuse in the future.

## Function makeCacheMatrix takes a matri as argument and makes a 
## "special" matrix that can be stored in the cache using set and get functions.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## CacheSolve computes, prints and returns the inverse of the matrix
## given in the argument. If the cache has already a stored value it
## simply returns the existing value.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
  }
