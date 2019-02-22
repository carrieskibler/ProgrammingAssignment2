## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## set value of matrix, get value of matrix, set value of inverse, get value of inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set value of matrix
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function () x
  setInv <- function(inverse) inv <<- solve(x)
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inv(data, ...)
  x$setinv(m)
  m
}
