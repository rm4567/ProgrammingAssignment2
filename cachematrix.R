## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
invr <- NULL
  set <- function(y){
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setInv <- function(ab) invr <<- ab
  getInv <- function() invr
  list(set = set, get = get, 
       setInv = setInv, 
       getInv = getInv)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invr <- x$getInv()
  if (!is.null(invr)) {
    message("getting cached data")
    return(invr)
  }
  mat <- x$get()
  invr <- solve(mat, ...)
  x$setInv(invr)
  invr
}
