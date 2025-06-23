## These functions create a special matrix object that can cache its inverse,
## and compute the inverse, caching the result for efficiency.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the cached inverse as NULL
  
  # Set a new matrix and reset cached inverse
  set <- function(y) {
    x <<- y        # Assign new matrix value in parent environment
    inv <<- NULL   # Reset cached inverse since matrix changed
  }
  
  # Get the current matrix
  get <- function() x
  
  # Cache the inverse matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Retrieve the cached inverse matrix
  getInverse <- function() inv
  
  # Return a list of the above functions to access them externally
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated and cached, it retrieves it from cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Get cached inverse if it exists
  
  # If inverse is cached, return it with a message
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, get the matrix data
  data <- x$get()
  
  # Compute the inverse using solve()
  inv <- solve(data, ...)
  
  # Cache the inverse for future calls
  x$setInverse(inv)
  
  # Return the computed inverse
  inv
}
