## This script describes functions, namely makeCacheMatrix and cacheSolve. 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## Assume that the matrix is of square form and invertible.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <- NULL
  }
  get <- function () x
  set_inv <- function(inverse) {inv <<- inverse}
  get_inv <- function() inv
  list(set=set, get=get,
       set_inv=set_inv, get_inv=get_inv)
}

## cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr, ...)  
  x$set_inv(inv)
  inv
}
