## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. The purpose of this assignment is to write a pair of functions that
## cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
     ## Set the value of the special matrix.
     specialInverseMatrix <- NULL
     set <- function(y) {
          x <<- y
          specialInverseMatrix <<- NULL
     }
     
     ## Get the value of the special matrix.
     get <- function() x

     ## Set the value of the matrix's inverse (obtained by "solve").
     setsolve <- function(solve) specialInverseMatrix <<- solve
     
     ## Get the value of the matrix's inverse.
     getsolve <- function() specialInverseMatrix
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
## `makeCacheMatrix` above. If the inverse has already been calculated 
## (and the matrix has not changed), then `cacheSolve` should retrieve 
## the inverse from the cache.
##
## Assumption:  The matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     ## Get the inverse of the matrix "x".
     specialInverseMatrix <- x$getsolve()
     
     ## Try to get matrix inverse from cache first.
     if(!is.null(specialInverseMatrix)) {
          message("getting cached data")
          return(specialInverseMatrix)
     }
     
     ## Cache missed, so calculate the matrix inverse of x (using "solve").
     xMatrixData <- x$get()
     specialInverseMatrix <- solve(xMatrixData, ...)
     x$setsolve(specialInverseMatrix)
     specialInverseMatrix
}
