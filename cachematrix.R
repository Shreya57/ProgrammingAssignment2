## Aim: To write 2 functions namely; 'makeCacheMatrix' and 'cacheSolve' that return and cache the inverse of the matrices for future use
## makeCacheMatrix is a function which creates a special "matrix" object that can cache its inverse for the input
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## This function returns a matrix that is the inverse of 'x'
## Variable 'j' checks if the matrix is already available in the cache or not
cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("Getting cached data!")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}