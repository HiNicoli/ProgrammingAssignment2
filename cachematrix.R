## This pair of functions is intended to cache the inverse of a matrix

## the makeCacheMatrix function should create a special "matrix" object
## that can cache its inverse

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

## the cacheSolve function should compute the inverse of the special "matrix"
## returned from the previous function. If the inverse has already been 
## calculated (and the matrix has not changed), 
##the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setsolve(m)
        m
}
