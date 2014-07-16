## Caching the Inverse of a Matrix
## By Leanne Trimble
## This script includes 2 functions which are able to create a matrix object and cache
## its inverse. 

## This function makeCacheMatrix creates the special "matrix" which is really a list
## containing a function to 1. set the value of the matrix, 2. get the value of the 
## matrix, 3. set the value of the inverse matrix, and 4. get the value of the inverse
## matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function cacheSolve calculates the inverse of the special "matrix" created with 
## the above function. It first checks to see if the inverse has already been calculated,
## and if so, it gets the inverse from the cache rather than compute it. If it has not
## been computed it does so and sets the value via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i 
}
