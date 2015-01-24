## R Programming Assignment 2
## This file contains two functions, makeCacheMatrix and solveCached, which are used to cache matrix inversion.  
##      makeCacheMatrix creates a list which acts as a special matrix which caches its inverse.
##      solveCached computes the inverse and caches it.

## Sample usage:
##      > x <- matrix(c(1,2,3,4), nrow=2, ncol=2)
##      > xlist <- makeCacheMatrix(x)
##      > xinverse <- cacheSolve(xlist)
##      > x %*% xinverse
##      [,1] [,2]
##      [1,]    1    0
##      [2,]    0    1
##      > xinverse <- cacheSolve(xlist)
##      returning cached inverse
##      > x %*% xinverse
##      [,1] [,2]
##      [1,]    1    0
##      [2,]    0    1



## Returns a list, which acts as a special matrix which caches its inverse.
## Input: x, a matrix
## Output: a list of the following named functions:
##      set: set the value of the matrix
##      get: get the value of the matrix
##      

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y)
        {
              x <<- y
              inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: Accepts a list created by makeCacheMatrix, checks for a cached copy of its inverse, returns the cached copy if it exists, and computes and
## caches it if not.
## Input: A list created by makeCacheMatrix.  (A "cacheable" matrix.)
## Output: The inverse of the matrix.
## (Side effect: Caching the inverse.)

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if (!is.null(inverse))
        {
                message("returning cached inverse")
                return (inverse)
        }
        data <- x$get
        inverse <- solve(x$get())
        x$setinverse(inverse)
        inverse
}
