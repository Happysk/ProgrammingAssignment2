
# Functions:
# making cache matrix 
# get inverse of matrix

# Setting the value of a  matrix
# Getting its value
# Setting its invers and getting its inverse
# Creation of special list matrix whitch contain these data

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  setMatrix <- function(newValue){
      x <<- newValue
      cache <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) cache <<- inverse
  getInverse <- function () cache
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


# Computing value of matrix
# Retrieving a cache value

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
    }
    data <- x$getMatrix()
    inverse <- solve(data)
    x$cacheInverse(inverse)
    
    inverse
}
