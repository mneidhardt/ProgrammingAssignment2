##  Two functions that provide cached inversion of matrices.
##  Usage: To get the inverse of a matrix, M1, do this:
##
##  mc <- makeCacheMatrix(M1)
##  M1Inv <- cacheSolve(mc)
##
##  To get the inverse of another matrix, M2, call cacheSolve with the new matrix:
##
##  M2Inv <- cacheSolve(mc, M2).
##
##  If M2 is identical to M1, cacheSolve returns the cached 
##  inverse, and otherwise computes it from M2. 
##--------------------------------------------------------------------------------

##  Function makeCacheMatrix creates an object that stores a matrix, 
##  as well as methods for getting & setting the matrix
##  and its inverse.

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


## Function cacheSolve takes a makeCacheMatrix-object and an optional matrix to invert.
## It returns the inverse of the matrix. If mat is not null, it is compared to the matrix
## stored in x, and if not identical, will overwrite the matrix in x.

cacheSolve <- function(x, mat = NULL) {
    if (is.null(mat)) {
        message("mat is null")
        minverse <- x$getminverse()
    } else {
        message("mat not null")
        previousmat <- x$get()
        if (matrixidentity(mat, previousmat)) {
            message("Mat is same as previous mat, so use the cache.");
        } else {
            message("Mat is different from previous mat.");
            x$set(mat)
        }
        minverse <- x$getminverse()
    }

    if (is.null(minverse)) {
        message("minverse null, so we solve.")
        minverse <- solve(x$get())
        x$setminverse(minverse)
    } else {
        message("Getting cached matrix.")
    }

    minverse
}

matrixidentity <- function(m1, m2) {
    is.matrix(m1) && is.matrix(m2) &&
    dim(m1) == dim(m2) && all(m1 == m2)
}
    
