## This is a set of 2 functions that works together to provide cache for
## matrix inverse.  makeCacheMatrix function creates an object that holds
## the matrix and its inverse.  solveCache use the object created by 
## makeCacheMatrix and return inverse only compute once and use the cache 
## value in later calls to improve performance.

## makeCacheMatrix creates a object to hold a matrix and its inverse
## and provide 4 accessor methods for the 2 values

makeCacheMatrix <- function(x = matrix()) {
    ## cache for inverse
    inverse <- NULL
    ## allow setting the value for matrix, clearing the inverse cache when reset
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## return the matrix value
    get <- function() x
    ## set the inverse cache
    setinverse <- function(i) inverse <<- i
    ## return cached inverse
    getinverse <- function() inverse
    list (set = set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## solveCache use the object created by makeCacheMatrix and return the 
## inverse of the matrix, first by compute the inverse using the solve function
## when the cache is not there and save the result in cache for later use.

cacheSolve <- function(x, ...) {
       
    ## try to get inverse from cache first
    i = x$getinverse()
    if (! is.null(i)) {
        message("getting cached data")
        return (i)
    }
    ## there is no cache available so compute one by getting the matrix from x
    m <- x$get()
    ## compute the inverse using solve() function
    i <- solve(m, ...)
    ## save the inverse in cache for future use
    x$setinverse(i)
    
    ## Return a matrix that is the inverse of 'x'
    i
}
