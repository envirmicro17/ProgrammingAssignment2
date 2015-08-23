## To create a set of functions for caching matrix inverse to be used in main matrix function solver

## makeCacheMatrix creates four subfunctions to be used in the caching process for the main fuction
## cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(matxinv) inv <<- matxinv
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  
}


## Main function where the function "solve()" is used to compute the matrix inverse from the data
## Conditional statements test whether or not the cache should be used or matrix inverse recomputed

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  else 
  {
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  }
  inv
}
