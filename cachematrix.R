## These functions provide a means for creating a matrix object that 
## can cache its own inverse.

## This function takes a matrix and returns a special matrix object
## that provides methods for getting and setting the matrix itself
## and a cache of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes a matrix object created by makeCacheMatrix
## and returns its inverse. If a cached version of the inverse is 
## available, it returns that, otherwise it calculates the inverse, 
## caches it, and returns it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
