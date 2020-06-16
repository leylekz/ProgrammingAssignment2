## Defines a "matrix" object that is able to cache the matrix
## inverse of an input matrix. When the inverse of the matrix
## is requested, the cache is checked, if available the inverse
## is returned, if not it is calculated, stored in the object
## cache and returned

## The makeCacheMatrix creates on "matrix" object that can stored
## the cached inverse of an input matrix

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) minv <<- solve
        getinverse <- function() minv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function returns the inverse of the matrix object
## It checks whether the inverse is cached, if not calculates the inverse,
## caches the inverse and returns the inverse matrix. If it exists,
## it simply returns the pre-calculated inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getinverse()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setinverse(minv)
        minv
}


