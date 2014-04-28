## This function calculates inverse of a matrix and stores 
## the inverse of a matrix value in an environment variable.
## Caching mechanism is implemented used environment variable,
## to use it for a fast operation.

## This function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## This function computes the inverse of the special matrix 
## and stores the value in cache if the value is not already set
## in the cache. The value can be retrieved by running cacheSolve.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
