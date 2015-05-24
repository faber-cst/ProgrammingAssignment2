## The function bellow cache the inverse of a matrix, passed as an argument of 
## the first function

## This function reads the matrix and creates a list of functions related to 
## that matrix. It also creates and initializes the local variable "m", where
## tehe inverse of the matrix will be stored, once it's calculated

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set=set, get=get, 
            setinverse=setinverse,
            getinverse=getinverse)      
}

## Return a matrix that is the inverse of 'x'
## This function gets the information from the previous function and see if the 
## inverse matrix  was previously calculated. If the inverse was not calculated
## yet, the function does the calculation and caches the value. If the matrix was
## calculated earlier, then the function just uses the value that was cached before

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        arg_matrix <- x$get()
        m <- solve(arg_matrix, ...)
        x$setinverse(m)
        m
}
