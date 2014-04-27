## this is an R function to cache potentially time consuming operations
## in this case we are caching the inverse of a matrix

## creates a special matrix which can be used by cachesolve

makeCacheMatrix <- function(x = matrix) {
    mtx_inverse <- NULL
    set <- function(y) {
        x <<- y
        mtx_inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) mtx_inverse <<- inverse
    getInverse <- function() mtx_inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## this function returns the inverse of a matrix
## it uses the "special" matrix from makeCacheMatrix
## to return a cached result if it has already computed the inverse

cacheSolve <- function(x, ...) {
    m <- x$getInverse()         # query the x matrix's cache
    if(!is.null(m)) {           # if there is a cache
        message("getting cached data")
        return(m)           # just return the cache, no computation needed
    }
    data <- x$get()         # if there is no cache
    m <- solve(data, ...)   # compute the inverse of the matrix
    x$setInverse(m)         # save the inverse back to x's cache
    m                       # return the result
}
