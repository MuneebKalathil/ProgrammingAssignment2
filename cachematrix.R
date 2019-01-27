## This program creates the inverse of a matrix. Inorder to escape from 
## the repitative processing, which use many computations (incase of huge data); 
## we use cache to avoid repitation.

## Two functions are defined, to create a matrix and caches the inverse.

## Function to create a matrix that can cache the inverse of it.

makeCacheMatrix <- function(x = matrix()) {

  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}



## Creates the inverse of matrix created by makeCacheMatrix function 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
## Displays the cached data if already been calculated the inverse
  if (!is.null(inv)) {
    message("Cached Data ..")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
