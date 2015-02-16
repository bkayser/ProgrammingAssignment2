## The functions makeCacheMatrix and cacheSolve together make a
## utility that caches the inverse of a given matrix, ensuring
## the calculation will only be performed once.

## makeCacheMatrix creates a wrapper around the given matrix
## consisting of a list of functions to access both the matrix
## (e.g., m) and its inverse:
##
##   cm <- makeCacheMatrix(m)
##   m$get           : return the matrix
##   m$set(x)        : set the matrix to x and clear the inverse
##   m$setinverse    : set the inverse of the matrix
##   m$getinverse    : return the inverse of m, or NULL if not set

makeCacheMatrix <- function(source_matrix = matrix()) {
    cached_inverse <- NULL
    get <- function() source_matrix
    set <- function(new_matrix) {
        source_matrix <<- new_matrix
        cached_inverse <<- NULL
    }
    setinverse <- function(inverse) cached_inverse <<- inverse
    getinverse <- function() cached_inverse
    list(get = get,
         set = set,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Given an CacheMatrix created with the makeCacheMatrix() function
## determine the inverse of the underlying data and return it.  If
## the solution has already been calculated, return the cached value.

cacheSolve <- function(matrix_data, ...) {
    inverse <- matrix_data$getinverse()
    if (!is.null(inverse)) {
        message("getting cached matrix solution")
    } else {
        data <- matrix_data$get()
        inverse <- solve(data, ...)
        matrix_data$setinverse(inverse)
    }
    return(inverse)
}