# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than compute it
# repeatedly. The following two functions are used to cache the inverse of
# a matrix.

# makeCacheMatrix creates a special "matrix", which is really a list
# containing a function to
#   1. set the value of the matrix
#   2. get the value of the matrix
#   3. set the value of inverse of the matrix
#   4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    
    getinverse <- function() {
        inv
    }
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve calculates the inverse of the special "matrix" created with the
# above function. It first checks if the inverse has already been created. If
# so, it gets the inverse from the cache and skip the computation. Otherwise,
# it calculates the inverse of the data and sets the value of the inverse in
# the cache via the setinverse function.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    
    inv
}
