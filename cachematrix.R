## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This file contains a pair of functions that cache the inverse of a matrix.

## This function creates a list object that can cache a matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        xinv <- NULL
        
        # set the value of the matrix
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        
        # get the value of the matrix
        get <- function() x
        
        # set the value of the inverse
        setinv <- function(inv) xinv <<- inv
        
        # get the value of the inverse
        getinv <- function() xinv
        
        # return a list containing all above functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the matrix returned by the list from
## makeCacheMatrix above. If the inverse has already been calculated, then 
## the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # try to get the inverse from makeCacheMatrix()
        xinv <- x$getinv()   
        
        # if the inverse exists in the cache, return the inverse
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        
        # otherwise, get the matrix from the cache
        xdata <- x$get()
        
        # calculate the inverse via solve()
        xinv <- solve(xdata, ...)
        
        # set the value of inverse in the cache
        x$setinv(xinv)
        
        # return the inverse
        xinv
}
