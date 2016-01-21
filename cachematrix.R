## Call makeCacheMatrix to create a cache in which the inverse of a matrix can be
## stored. Call cacheSolve to compute the inverse if not available, or fetch it 
## when available, and finally return the inverse.

## create a vector of functions that can cache the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    getinverse <- function() i
    setinverse <- function(inverse) i <<- inverse
    list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## compute the inverse if it does not exist, and return the inverse

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
