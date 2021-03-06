# Assignment: Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly (there are also alternatives to matrix inversion that we will
# not discuss here). Your assignment is to write a pair of functions that
# cache the inverse of a matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())
{
  v <- NULL
  # set the value of a matrix
  set <- function(y) {
    x <<- y
    # value of the inverse v is back to null. 
    v <<- NULL
  }
  
  
  # get the value of a matrix
  get <- function() 
  { x}
  
  
  # set the value of the inverse
  setinverse <- function(inverse)
  {v <<- inverse }
  
  
  # get the value of the inverse
  getinverse <- function() 
  {v}
  
  

  # setting up the list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
  
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  v <- x$getinverse() # x$v
  
  # if statement is used to check whether the inverse has already been calculated; if so, 
  # it gets the inverse from the cache and skips the computation
  
  if (!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  
  # Otherwise, it calculates the inverse of the data and sets the value of 
  # the inverse in the cache via the setinverse function
  data <- x$get()
  v <- solve(data, ...)
  x$setinverse(v)
  v
}
