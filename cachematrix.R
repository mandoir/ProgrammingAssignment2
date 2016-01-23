## Matrix inversion is often a resource-costly computation. Calcuating the inverse
## of a matrix once and caching the inverse may be valuable and save computing time.

## Function makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    SetInverse <- function(inverse) inv <<- inverse
    GetInverse <- function() inv
    list(set = set,
         get = get,
         SetInverse = SetInverse,
         GetInverse = GetInverse)
}

## Function cacheSolve will compute the inverse of the matrix object created by function makeCacheMatrix. 
## If the inverse of the matrix has already been computed and the matrix has not changed, 
## function cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$GetInverse()
    if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$SetInverse(inv)
    inv
}
