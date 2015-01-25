## These functions will cache the inverse of a matrix

## This function creates a special 'matrix' object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## create matrix object x with associated functions
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL    
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function computes the inverse of what makeCacheMatrix returns

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  
  if(!is.null(m)) {
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinverse(m)
  
  m
  
}
