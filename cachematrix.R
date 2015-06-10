# This second programming assignment includes R functions that are able to cache potentially 
# time-consuming computations. 
 

# The first function, makeCacheMatrix This function creates a special "matrix" object that can cache its inverse.
# The matrix is a list containing a function to 
# 1.set the value of the Matrix
# 2.get the value of the Matrix
# 3.set the value of the inverse Matrix
# 4.get the value of the inverse Matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setreverse<- function(reverse) inv <<-reverse
  getreverse <- function() inv
  list(set = set, get = get,
       setreverse = setreverse,
       getreverse = getreverse)
  
}

# The second function, cacheSolve creates a the reverse of a matrix. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# For this assignment, assume that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getreverse()
  if (!is.null(inv)) {
    message("getting cached reververse matrix")
    return(inv)
  } else {
    inv <- solve(x$get())
    x$setreverse(inv)
    return(inv)
  }
}
