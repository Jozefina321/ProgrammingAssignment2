## Two functions that are situated below are used to create
## a special object that stores a matrix and caches the inverse
## of a matrix.


## The following function named "makeCacheMatrix" creates a special
## matrix that can cache the inverse of it.

makeCacheMatrix <- function(x = matrix()) {   ## the main function
        m <- NULL     ## the "place" where the result is stored 
        set <- function(y) {         ## 1st function if we want to substitute 
                                     ## matrix x with the another matrix y
                x <<- y = matrix()
                m <<- NULL
        }
        get <- function() x        ## 2nd function returns the matrix stored in
                                   ## the main function
        setsolve <- function(solve) m <<- solve ## stores the inverse of the matrix
        getsolve <- function() m   ## returns the inverse of the matrix
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve) ## stores all of the 4 functions
}


## This second function is used to compute the inverse of the defined matrix
## returned by "makeCacheMatrix" function. The "cacheSolve"function retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if (!is.null(m)) {
                message("getting cached data")
                return(m) ## if the m is in the memory, it returns the message and the m (the inverse)
        }
        matrix <- x$get()
        m <- solve (matrix, ...)  ## m calculates the inverse of the matrix
        x$setsolve(m)   ## stores the inverse in the object 
        m  ## returns the calculated result
}
