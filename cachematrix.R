## Functions makeCacheMatrix and cacheSolve together enable the creation
## of a special matrix object that can cache the inverse of a matrix along
## with the matrix itself. The inverse is computed only once and only if
## needed.

## Given an invertible matrix as input, makeCacheMatrix creates an object
## containing the matrix and a holder for its inverse.
##
## Arguments
##   x   An invertible matrix. 
##   
## The result object is a list of three functions:
##
## "get" is function() that returns the matrix;
## "set" is function(newM) that resets the stored matrix to newM, assumed
##       to be invertible, and clears the inverse cache;
## "getInv" is function() that returns the inverse of the matrix;
## "setInv" is function(inv) that sets the inverse to inv. No checks are
##          made to ensure that inv the inverse of the matrix. This function
##          is designed to be used by the companion function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    mInv <- NULL
    set <- function(newM) {
        x <<- newM
        mInv <<- NULL
    }
    get <- function() { x }
    setInv <- function(inv) { mInv <<- inv }
    getInv <- function() { mInv }
    list(
        set = set,
        get = get,
        getInv = getInv,
        setInv = setInv)
}


## Given a special matrix object created by makeCacheMatrix, cacheSolve
## returns its inverse. The inverse is computed only once and cached.
## Arguments
##   x   The special matrix object created by makeCacheMatrix.
##   ... Optional arguments to be passed to the solve() function
##       to compute the inverse.

cacheSolve <- function(x, ...) {
    mInv <- x$getInv()
    if(!is.null(mInv)) {
        return(mInv)
    }
    m <- x$get()
    mInv <- solve(m, ...)
    x$setInv(mInv)
    mInv
}
