## Takes a matrix and creates a cache of its inverse.


## Creates the cache of a matrix and stores its inverse
## from the cacheSolve function below. 

makeCacheMatrix <- function(x = matrix()) {
        ## Indicates whether or not the inverse
        ## of the matrix has already been cached.
        m <- NULL
        ## Allows user to set a different matrix using set function.
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
## whether the inverse has already been found. If the inverse has been
## found, the function sends a message before returning the inverse
## without re-computing it. If it has not been found, it computes 
## the inverse and returns it.

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
