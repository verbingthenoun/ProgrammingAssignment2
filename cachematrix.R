## The following functions allow us to cache the inverse of a matrix
## which will save computing time.

## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y = matrix()) { ## set the stored matrix to a new matrix 'y'
      x <<- y
      m <<- NULL
  }
  get <- function() x ## returns the stored matrix
  setinverse <- function(inverse = matrix()) m <<- inverse ##sets inverse manually
  getinverse <- function() m ## returns the inverse of the stored matrix
  list(set = set, get = get, ##build the list of functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above or retrieves it from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() ##check to see if we have already calculated inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ##if so, return the inverse we already calculated for 'x'
  }
  data <- x$get()
  m <- solve(data)  ##otherwise, calculate the inverse of 'x'
  x$setinverse(m)     ##cache the inverse
  m                   ##return the inverse
}