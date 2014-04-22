## makeCachemMatrix and solveMatrix are a pair of functions that allow
## storage of the inverse of a matrix in a cache, and retrieval of the 
## matrix inverse from the cache rather tha recalculating it each time.

## makeCacheMatrix creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## sets the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## gets the value of the matrix
    get <- function() x
    ## sets the inverse of the matrix
    setinverse <- function(solve) m <<- solve
    ## gets the inverse of the matrix
    getinverse <- function() m
    ## a list of set, get, setinverse, and getinverse is returned
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix, either from the cache,
## or via calculation. Argument x is a list from cacheMatrix(yourmatrix).

cacheSolve <- function(x, ...) {
    ## Checks to see if the inverse of the matrix has already been
    ## calculated. If it has, the the cached inverse is returned.
    m <- x$getinverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## If the inverse of the matrix has not already been calculated,
    ## the inverse is then calculated and returned.
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
