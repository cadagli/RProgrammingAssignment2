## Functions makeCacheMatrix and cacheSolve together enable the creation
## of a special matrix object that can cache the inverse of a matrix along
## with the matrix itself. The inverse is computed only once and only if
## needed.

## Given an invertible matrix as input, makeCacheMatrix creates an object
## containing the matrix and a holder for its inverse.
## Arguments
##   m   An invertible matrix. 
##   
## The result object is a list of three functions:
##
## "get" is function() that returns the matrix;
## "set" is function(m) that resets the function to m, assumed to be
##       invertible, and clears the inverse cache;
## "getInv" is function() that returns the inverse of the matrix;
## "setInv" is function(mi) that sets the inverse to mi. No checks are to
##          confirm that mi the inverse of the matrix. This function is
##          designed to be used by the companion function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    mInv <- NULL
    set <- function(newM) {
        m <<- newM
        mInv <<- NULL
    }
    get <- function() { m }
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
        message("getting cached inverse")
        return(mInv)
    }
    m <- x$get()
    mInv <- solve(m, ...)
    x$setInv(mInv)
    mInv
}
