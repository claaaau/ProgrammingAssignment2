## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL ##initialing the invMatrix with no value
  setMatrix <- function(y)  ##define the setMatrix function
    
    {
    x <<- y
    invMatrix <<- NULL
    }
  
  getMatrix <- function() x
  setInverse <- function(inverse) invMatrix <<- inverse ##setting the value of the invertible Matrix
  getInverse <- function() invMatrix ##getting the value of the invertible Matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) ##if inverse Matrix is not NULL
    
    {
    message("Getting Cached Matrix")
    return(invMatrix) ##return the invertible Matrix
    }
  
  Matrixdata <- x$getMatrix() ##if value of the invertible Matrix is NULL, get the original Matrix data
  invMatrix <- solve(Matrixdata)
  x$setInverse(invMatrix)
  return(invMatrix) ###return the invertible Matrix
}
