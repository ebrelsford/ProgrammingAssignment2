## Cache the inverse of a matrix.
##
## First wrap the matrix with functions that allow for the caching of the
## inverse, using makeCacheMatrix(). Then execute cacheSolve() on the output to
## get the inverse of the matrix. cacheSolve() will cache the result on the
## output of makeCacheMatrix(), so calling cacheSolve() multiple times will only
## invert the matrix once.
##
## For example:
##
##     m = matrix(rnorm(100), nrow = 10, ncol = 10)
##     cm = makeCacheMatrix(m)
##     inverse = cacheSolve(cm)

## Wrap a matrix with functions to access a cached inverse created with
## cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Invert x (created with makeCacheMatrix), returning a cached value if 
## available

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
