##  Two functions that together provide cached inversion of matrices.
##  This means that inverting the same matrix twice only causes the actual inversion
##  to happen first time, the second time it is called, the previously computed
##  solution is returned.

##  Function makeCacheMatrix creates an object that stores a matrix, possibly
##  its inverse, and additionally, methods for getting & setting the original matrix
##  and the inverse matrix.

makeCacheMatrix <- function(mat = matrix()) {
    minverse <- NULL

    set <- function(newmatrix) {
        mat <<- newmatrix
        minverse <<- NULL
    }

    get <- function() mat
    setminverse <- function(minv) minverse <<- minv
    getminverse <- function() minverse

    list(set = set,
         get = get,
         setminverse = setminverse,
         getminverse = getminverse)
}


## Function cacheSolve takes a makeCacheMatrix-object and returns the inverse
## of the matrix stored in makeCacheMatrix.
## If the makeCacheMatrix matrix has already been inverted, this result is returned,
## and if not, we call solve on the matrix, sets the result and returns it.

cacheSolve <- function(x, ...) {
    minverse <- x$getminverse()

    if (is.null(minverse)) {
        minverse <- solve(x$get())
        x$setminverse(minverse)
    } else {
        message("Getting cached matrix.")
    }

    minverse
}
