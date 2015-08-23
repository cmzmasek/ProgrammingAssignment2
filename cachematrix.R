# Functions to calculate the inverse of a square invertible matrix and
# to cache the result.


# In the following is an example how to use these to functions:
#
# m1 <- matrix(c(2,2,3,2), nrow=2, ncol=2)
# cm <- makeCacheMatrix(m1)
# inv <- cacheSolve(cm)
# inv should be:
#      [,1] [,2]
# [1,]   -1  1.5
# [2,]    1 -1.0
#
# Repeat calls to cacheSolve(cm) would return cached results...
#
# m2 <- matrix(c(1,-1,1,2), nrow=2, ncol=2)
# cm$set(m2)
# inv <- cacheSolve(cm)
# inv should be:
#           [,1]       [,2]
# [1,] 0.6666667 -0.3333333
# [2,] 0.3333333  0.3333333
#
# Repeat calls to cacheSolve(cm) would return cached results...


# This function creates a special matrix object that  
# can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  # This initializes the result matrix to null
  inv <- NULL
  
  # This function can be used to set the input matrix
  set <- function(m) {
    x <<- m
    inv <<- NULL
  }
  
  # This function returns the input matrix  
  get <- function() {
    x
  }
  
  # This function sets the result matrix (the inverse)
  # using the "<<-" operator
  setInverse <- function(res) {
    inv <<- res
  }
  
  # This function returns the result matrix (the inverse)
  getInverse <- function() { 
    inv
  }
  
  # This creates a list of the functions   
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


# This function calculates the inverse of the special matrix object
# created by "makeCacheMatrix". If the inverse has already been
# calculated, the cached result is returned (instead of performing
# the calculation again).
cacheSolve <- function(x, ...) {
  
  # This attempts to get the cached result 
  # (which might be null if the inverse has not been calculated)
  cached_res <- x$getInverse()
  
  # If the cached result is not null, it is returned
  if(!is.null(cached_res)) {
    message("getting cached result")
    return(cached_res)
  }
  
  # The following is only executed if no cached result was present:
  
  # This gets the input matrix
  m <- x$get()
  
  # This calculates in the inverse
  res <- solve(m, ...)
  
  # This sets the inverse to be cached
  x$setInverse(res)
  
  # Returns the result
  res
  
}
