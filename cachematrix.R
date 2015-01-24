##  Two functions that provide cached inversion of matrices.
##
##  Usage: To get the inverse of a matrix, M1, do this:
##      mc <- makeCacheMatrix(M1)
##      M1Inv <- cacheSolve(mc)
##  If you repeat the last call, cacheSolve(mc) will now return the cached
##  version of the inverted matrix.
##
##  To get the inverse of another matrix, M2, call cacheSolve with the new matrix:
##
##      M2Inv <- cacheSolve(mc, M2).
##
##  If M2 is identical to M1, cacheSolve returns the cached 
##  inverse, and otherwise computes it from M2. 
##
## NB. The argument to makeCacheMatrix is optional. If not supplied,
## you must supply a matrix as argument to the function cacheSolve().
##--------------------------------------------------------------------------------

##  Function makeCacheMatrix creates an object that stores a matrix
##  and its inverse, and furthermore contains methods for
##  getting & setting the matrix and its inverse.

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

cacheSolve <- function(mycache, mat = NULL) {
    if (is.null(mat)) {
        message("mat is null, using matrix in cache.")
        minverse <- mycache$getminverse()
    } else {
        message("mat not null, checking if it is identical to cached matrix (if there is one)")
        previousmat <- mycache$get()
        if (matrixidentity(mat, previousmat)) {
            message("Mat is same as previous mat, so use the cache.");
        } else {
            message("Mat is different from previous mat.");
            mycache$set(mat)
        }
        minverse <- mycache$getminverse()
    }

    if (is.null(minverse)) {
        message("minverse null, so we solve.")
        minverse <- solve(mycache$get())
        mycache$setminverse(minverse)
    } else {
        message("Getting cached matrix.")
    }

    minverse
}

## Function to check if two matrices are identical.
matrixidentity <- function(m1, m2) {
    is.matrix(m1) && is.matrix(m2) &&
    dim(m1) == dim(m2) && all(m1 == m2)
}
    
