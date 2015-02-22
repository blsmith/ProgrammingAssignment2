## Description:  Assignment to write 2 functions, makeCacheMatrix and cacheSolve, that will cache the inverse of a matrix.
##   1. makeCacheMatrix() creates a special "matrix" object that can cache its inverse.
##   2. cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been
##     calculated, then it retrieves the inverse from the cache.


## makeCacheMatrix() creates a special "matrix", which is really a list containing a function to:
##   Set the value of the matrix, get the value of the matrix, set the value of the inverse of the matrix,
##   and get the value of the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
   
   # Initialize to provide a default if cacheSolve has not been invoked.
   invrs_x <- NULL
   
   # Set the value of the matrix.
   set <- function(y) {
      x <<- y
      invrs_x <<- NULL
   }
   
   # Get the matrix value.
   get <- function() x
   
   # Set the value of the inverse.
   setinverse <- function(inverse) invrs_x <<- inverse
   
   # Get the value of the inverse.
   getinverse <- function() invrs_x
   
   # Create a list containing the functions
   list(
      set = set,
      get = get,
      setinverse = setinverse,
      getinverse = getinverse
   )
}


## cacheSolve calculates the inverse of the special "matrix" returned by makeCahceMatrix.
##   If the inverse has already been calculated, it returns the cached inverse.
##   Otherwise, it calculates the inverse of the matrix and then returns the inverse.
cacheSolve <- function(x, ...) {
   
   # Get the inverse value.
   invrs_x <- x$getinverse()
   
   # If the inverse has been calculated, return the cahched value.
   if (!is.null(invrs_x)) {
      message("Getting cached inverse matrix.")
      return(invrs_x)
   }
   
   # If the inverse has not been calculated, compute the inverse and return the value.
   invrs_x <- solve(x$get())
   x$setinverse(invrs_x)
   return(invrs_x)
}