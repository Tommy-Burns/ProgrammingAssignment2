## Put comments here that give an overall description of what your
## functions do
# The functions provide a way to cache the inverse of a matrix.



## Write a short comment describing this function
# This function creates a special matrix object 
# that can store its inverse in a cache.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL  # Initialize inverse as NULL
  
  set <- function(y) {
    x <<- y  # Set the matrix
    inverse <<- NULL  # Reset inverse when the matrix changes
  }
  
  get <- function() x  # Get the matrix
  
  setinverse <- function(inverse_value) {inverse <<- inverse_value}  # Set the inverse
  
  getinverse <- function() inverse  # Get the inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# This function calculates the inverse of the matrix
# created by makeCacheMatrix. It first checks if the inverse has
# already been computed and cached. 
# If so, it retrieves the cached inverse. Otherwise, it calculates the inverse, caches it, and returns the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()  # Try to get the cached inverse
  
  if (!is.null(inverse)) {  # If inverse is cached
    message("Getting cached data")
    return(inverse)
  }
  
  data <- x$get()  # Get the matrix
  inverse <- solve(data, ...)  # Calculate the inverse
  x$setinverse(inverse)  # Cache the inverse
  inverse  # Return the inverse
}
