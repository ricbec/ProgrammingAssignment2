## The following functions are an implementation of the example for this assignment
## Example of use
## ma <-  rbind(c(1, -1/4), c(-1/4, 1)) 
## v1 <- makeCacheMatrix(ma)
## cacheSolve(v1)

makeCacheMatrix <- function(x = matrix()) {
  ## This function returns an object that stores a matrix and his inverse
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## x is an object created by makeCacheMatrix function
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

