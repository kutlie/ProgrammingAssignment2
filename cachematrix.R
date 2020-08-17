## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function makeCacheMatrix() creates a special "vector", which is really a list containing functions

makeCacheMatrix <- function(x = matrix()) {
  theInverse <- matrix()
  set <- function(y){
    x <<- y
    theInverse <<- matrix()
  }
  getData <- function() x
  getInverse <- function() theInverse
  setInverse <- function(inverse) theInverse <<- inverse
  list(set=set, get=getData, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
## The function cacheSolve() calculates the inverse of a matrix if receiving data for the first time, otherwise it gets it if it has already been calculated

cacheSolve <- function(x, ...) {
  theInverse <- x$getInverse()
  if(!is.na(theInverse)){
    message("getting cached data")
    return(theInverse)
  }
  data <- x$get()
  theInverse <- solve(data,...)
  x$setInverse(theInverse)
  theInverse
}
