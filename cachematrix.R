## Takes a matrix and creates a cache of its inverse
## 

## Creates the cache of a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
                }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## Finds the inverse of a matrix, after first determining 
## whether the inverse has already been found. If it has been
## found, it sends a message before returning the inverse without 
## re-computing it.

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
