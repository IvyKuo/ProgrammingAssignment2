## Create a pair of functions that cache the inverse of a matrix

## This function creates a special matrix object that can cache its inverse 
## set the value of matrix 
## get the value of matrix
## set the value of inverse of the matrix
## get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL     
  }
  get <- function () x
  setinv <- function (inverse) inv <<- inverse
  getinv <- function () inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)   
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Test above functions:
## a <- matrix (rnorm(16),nrow=4,ncol=4)
## b <- makeCacheMatrix(a) 
## cacheSolve(b) 
## c <- cacheSolve (b) 