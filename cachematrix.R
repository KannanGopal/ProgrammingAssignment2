## a pair of functions that cache the inverse of a matrix
## Store and Retrive the cache matrix

## a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # vector to hold the inverse
    xI <- NULL
  
  # set matrix X
    set <- function(y) {
        x <<- y
        xI <<- NULL
    }
  
  # get matrix X
    get <- function() x
  
  # set Inverse of X
    setinverse <- function(solve) xI <<- solve
  
  # get Inverse of X
    getinverse <- function() xI
  
  # list functions in cache
    list (set=set, get=get, 
          setinverse = setinverse,
          getinverse = getinverse)
}


## computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xI <- x$getinverse()
  if(!is.null(xI)) {
    message("getting cached data")
    return(xI)
  }
  data <- x$get()
  xI <- solve(data)
  x$setinverse(xI)
  xI
}
