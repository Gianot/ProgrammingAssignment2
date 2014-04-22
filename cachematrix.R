## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  i <- NULL
  
  ## Set matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Get matrix
  get <- function() {
    m
  }
  
  ## Set inverse of matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Get inverse of matrix
  getInverse <- function() {
    i
  }
  
  ## Return a list of operations
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Obtain matrix
  data <- x$get()
  
  ## obtain inverse using matrix
  m <- solve(data) %*% data
  
  ## Set inverse of the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
