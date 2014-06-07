## Caching the Inverse of a Matrix by Clever Use of Lexical Scoping in R
## 
## Contains the following functions:
##   - makeCacheMatrix: creates a special "matrix" object that can cache 
##     its inverse.
##   - cacheSolve: computes the inverse of the special "matrix" returned 
##     by makeCacheMatrix above. If the inverse has already been calculated
##     (and the matrix has not changed), then the cachesolve should 
##     retrieve the inverse from the cache.

## makeCacheMatrix: creates a special "matrix" object that can cache 
##     its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve: computes the inverse of the special "matrix" returned 
##     by makeCacheMatrix

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
