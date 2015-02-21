# The following two functions are used to
# cache the inverse of a matrix as it is 
# usually a costly computation.

# makeCacheMatrix creates a list containing a function to
# I.   set the value of the matrix
# II.  get the value of the matrix
# III. set the value of inverse of the matrix
# IV.  get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  getinverse <- function() {
    inv
  }
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function assumes that the matrix is always invertible.
# This also prints a message in cases where cached data is found
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Cached data found! getting Cache Data...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}