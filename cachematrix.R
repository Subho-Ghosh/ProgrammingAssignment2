## The functions below provide a means to calculate and cache the inverse of a square matrix 
## Not all square matrices(of order n) are invertible
## Solver function throws errors when the matrix is not invertible

## The order of invocation is: call makeCacheMatrix() function first to initialize and then
## use the returned list to call cacheSolve() 


## This function initializes the environment for the call to the next function
## The get and set functions for the matrix and its inverse are done here
## The setMatrix() function within this function caches the value of inverse 
## and the matrix value to the argument passed as matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  setInv <- function(inverse) m <<- inverse
  getInv <- function() m
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInv = setInv,
       getInv = getInv)
  
}

## This function uses the solver function to compute the inverse of the new matrix created by a
## call to the function makeCacheMatrix



cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  ## check to ensure that an inverse already exists
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if a new matrix is to be inverted check the dimensions to be a square matrix
  ## else solve will fail for sure
  data <- x$getMatrix()
  
  if(nrow(data)!=ncol(data)){
    message("getting cached data : new matrix is not square")
    return(m)
  }
  m <- solve(data, ...)
  x$setInv(m)
  m
}

