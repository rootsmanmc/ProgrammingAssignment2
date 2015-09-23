## The  two functions are makeCacheMatrix which creates a special object that stores a matrix and
## CacheSolve which returns the inverse of the matrix and cache's it.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. It contains 4 functions set, get, setInv and getInv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x     ## get is a function that returns the matrix x stored in the main function. Doesn't require any input.
  setInv <- function(inverse) inv <<- inverse     ## stores the value of the input in a variable inv into the main function makeCacheMatrix
  getInv <- function() inv          ##returns the stored value in the  main function makeCacheMatrix 
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.  It first checks to see if the  matrix inverse has already been calculated.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
}
