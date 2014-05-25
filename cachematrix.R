## When used together, makeCacheMatrix and cacheSolve calculate and produce the inverse of a matrix 
## and cache the result. If the inverse has already been cached, they will produce the cached result
## without recalculating it.

## makeCacheMatrix creates objects which will cache matrix inversions solved by cacheSolve

makeCacheMatrix <- function(x = matrix()) { i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## cacheSolve inverts new matrices, caches them, and produces the inverted matrix. If the matrix has already been cached,
## cacheSolve will not recalcuate the inverse, but will produce the cached matrix.

cacheSolve <- function(x, ...) {  i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}
