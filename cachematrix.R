## The following functions create a special "matrix" object and then cache the inverse of the matrix
## The code assumes the matrix is always invertible.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #initialize inverse
  inverse <- NULL
  
  #set matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  #get matrix
  get <- function() x
  
  #sets value of inverse to the variable inverse
  setinverse <- function(inverseValue) inverse <<- inverseValue
  
  #returns the value of inverse
  getinverse <- function() inverse
  
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.  
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
  inverse <- x$getinverse()
  m <- x$get()
  
  # Check if the inverse already exists and whether the matrix has changed  
  if(!is.null(inverse) && identical(x,m)) { 
    message("Getting cached data for matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
