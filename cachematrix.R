# Two functions that together provide cached inversion of matrices.
# This means that inverting the same matrix twice only causes the actual inversion
# to happen first time, the second time it is called, the previously computed
# solution is returned.

## Function makeCacheMatrix creates an object that stores a matrix together with
# methods for inverting it, getting it and setting it.

makeCacheMatrix <- function(mat = matrix()) {
    minverse <- NULL
    set <- function(newmatrix) {
        mat <- newmatrix
        minverse <- NULL
    }

    get <- function() mat
    setminverse <- function(minv) minverse <<- minv
    getminverse <- function() minverse
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
