## Assignment 2: matrix that can cache it's inverse

## makeCacheMatrix returns a matrix object that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(y) xinv <<- y
    getinv <- function() xinv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve uses a makeCacheMatrix() result to return it's inverse
## whether it's already computed (cached) or not.
cacheSolve <- function(x, ...) {
    y <- x$getinv()
    if (!is.null(y)) {
        message("getting cached data")
        return(y)
    }
    m <- x$get()
    y <- solve(m, ...)
    x$setinv(y)
    y
}
