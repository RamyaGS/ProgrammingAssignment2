##R code to optimize computation time of calculating inverse of a matrix by caching the value for future use


## Function to cache a matrix and its inverse by defining set functions and creating access to them using get functions

makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  setMatrix <- function(y){
    x<<-y
    matrixInverse <<-NULL
  }
  getMatrix <- function(){x}
  setInverse <- function(inverse){matrixInverse <<- inverse}
  getInverse <- function(){matrixInverse}
  list(setMatrix=setMatrix,
       getMatrix=getMatrix,
       setInverse=setInverse,
       getInverse=getInverse)
    }
  


#Function to compute inverse of a matrix.
#This function checks if a matrix and its inverse
#exist in cache and returns the  cached inverse if true else computes inverse which is then cached and returned. 

cacheSolve <- function(x, ...) {
        
  matrixInverse <- x$getInverse()
  if (!is.null(matrixInverse)){
    print('getting cached inverse')
    return(matrixInverse)
  }
  ## Return a matrix that is the inverse of 'x'
  data <- x$getMatrix()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
  
}
