## Functions below are used to cache the inverse of the matrix

## makeCacheMatrix() stores four functions:
## get() : returns the matrix
## set() : changes the matrix stored in main function
## getInverse() : returns the Inverse of the matrix
## setInverse() : sets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## invMatrix stores the inverse of the matrix
  invMatrix <- NULL
  
  ## to set the matrix
  set <- function(y){
    x <<- y
    invMatrix <- NULL
  }
  
  ## to get the matrix
  get <- function() x
  
  ## to set the inverse of the matrix
  setInverse <- function(inv) invMatrix <<- inv
  
  ## to get the inverse of the matrix
  getInverse <- function() invMatrix
  
  ## The following statement is used to store the four functions
  list(get = get, 
       set = set, 
       getInverse = getInverse, 
       setInverse = setInverse)
}


## cacheSolve() is used to calculate the inverse of the special "matrix" 
## created with above function. If the inverse for the matrix has already been
## calculated, it will return the cached result.

cacheSolve <- function(x, ...) {
  
  ## invMatrix is the Inverse of the matrix
  invMatrix <- x$getInverse()
  
  ## check if the cached inverse is available
  if(!is.null(invMatrix)){
    message("Cache data available... Getting cached data...")
    
    ## Return a matrix that is the inverse of 'x'
    return(invMatrix)
  }
  
  ## cached inverse is not available, so calculating it
  data <- x$get()
  invMatrix <- solve(data)
  
  ## caching the inverse for future use
  x$setInverse(invMatrix)
  
  ## Return a matrix that is the inverse of 'x'
  invMatrix
}
