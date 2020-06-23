## The functions within 'cachematrix' are designed to compute the
## inverse of a matrix and store the value into the cache. If an
## inverse of the same matrix is required, the cache will provide
## the value, saving on any potential computation time. This is 
## valuable for large matrices. For any new matrix, the inverse 
## can also be calculated and cached for future use.

## Below is the makeCacheMatrix function. Call this function to
## set the required values for the solve function. Assign makeCacheMatrix
## to a value (example m1 <- makeCacheMatrix()). You will need to 
## create your own matrix as 'x' is not defined.

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setsolve <- function(solve) m <<- solve
   getsolve <- function() m
   list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## cacheSolve is used to output the inverse of your matrix and to 
## store the value to the cache. You must first makeCacheMatrix()
## before cacheSolve() will work properly for any new matrix. 
## (example: cacheSolve(m1)) 
## If you do calculate an inverse matrix you had prior calculated
## with this function, you will get a "getting cached data" message
## instead of the function doing a new calculation.

cacheSolve <- function(x, ...) {
   m <- x$getsolve()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setsolve(m)
   m
}
