## Create a special matrix which caches its inverse
## Parameter - an invertible matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ##set the the matrix and reset the inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## get the matrix
  get <- function() x
  
  ## set the matrix inverse
  setinverse <- function(inv) inverse <<- inv
  
  ## get the matrix inverse
  getinverse <- function() inverse
    
  list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
}


## Return the inverse of the given matrix
## If the inverse has been already computed and the matrix did not change, return the cached inverse
## Otherwise, compute and cache the inverse
## Parameter - the special matrix type returned by the makeCacheMatrix funtion 
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  ##inverse already cached
  if(!is.null(inverse)) {
    message("Getting the cached inverse")
    return(inverse)
  }
  
  ##compute and cache the inverse
  matrix <- x$get()
  inverse = solve(matrix, ...)
  x$setinverse(inverse)
  inverse  
}
