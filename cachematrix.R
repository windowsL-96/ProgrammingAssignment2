## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function:
## This function creates a special "matrix" object that can cache its inverse.

## We need to put the value of makeCacheMatrix(Random_Matrix) (a list) 
## into a variable (e.g. matrix_cache). This is the cache that need to
## be read into the "x" argument in the cacheSolve() function below.

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() {x}
        setsolve <- function(solve) {mat <<- solve}
        getsolve <- function() {mat}
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve function:
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

## Let x be the cache created above.
## If x is the matrix, the "$" operator could not work, since "$" is invalid
## on atomic vectors (e.g. a matrix).

cacheSolve <- function(x, ...) {
        mat <- x$getsolve()
        if(!is.null(mat)) {
                message("Getting cached inverse of a matrix")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        x$setsolve(mat)
        mat
}
