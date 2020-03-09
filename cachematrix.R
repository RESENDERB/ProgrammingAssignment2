## Put comments here that give an overall description of what your
## functions do

## The functions below are intended to save time for matrix inverse calculations, 
## which are generally time-consuming computations. For this, the values already 
## calculated are stored in cache and consulted before performing the calculation. 
## If they have already been done, they return the cached value, otherwise, the 
## calculation is performed.

## Write a short comment describing this function

## The 'make' function creates a list containing the values of defining and obtaining
## the matrix, and the values of defining and obtaining the inverse of the matrix. 

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


## Write a short comment describing this function

## The 'cache' function calculates the inverse of a matrix, when it has not been 
## previously calculated. If it has been calculated before, it uses the result 
## stored in cache, otherwise it does the calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
