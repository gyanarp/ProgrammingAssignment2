# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# set the value of the matrix and get the value
# set the value of inverse of the matrix and get the value 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve function returns the inverse of the matrix. 
# It assumes that the matrix is always invertible 
# checks if the inverse has already been computed. If yes, gets the result and 
# skips computing. If not, computes the inverse, sets the value in the cache thru
# setinverse function.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
