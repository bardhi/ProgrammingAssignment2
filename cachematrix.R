## This file contains two functions. They are used to create a matrix, compute
#  its cache, and run the inverse on it. 
#  Sample usage:
# 1. create a normal matrix: 
# normal <- matrix(c(2, 0, 2, 0, 1, 2, 0, 0, 2), 3)
# 2. create a cache (aka magic) matrix:
# magic <- makeCacheMatrix(normal)
# 3. compute and get the inverse:
# cacheSolve(magic)
# 4. subsequent calls to cacheSolve will get the inverse from the cache
# cacheSolve(magic); cacheSolve(magic); cacheSolve(magic)


## This function creates a list that contains functions as members. The member functions
# allow setting a matrix, getting it back, computing its inverse, and returning the 
# inverse, if already computed
makeCacheMatrix <- function(matrixVal = matrix()) {
    solve <- NULL
    set <- function(newVal) {
        matrix <<- newVal
        solve <<- NULL
    }
    get <- function() {
        matrixVal
    }
    
    runSolve <- function(...) {
        solve <<- solve(matrixVal, ...)
    }
    
    getSolve <- function() {
        solve
    }
    
    list(set = set, get = get, runSolve = runSolve, getSolve = getSolve)
}


## This takes as an argument a cache matrix created via makeCacheMatrix above, and
# computes the inverse of it by calling solve on the matrix, or returning the already
# computed inverse that is stored in the cache. The inverse is computed using the solve
# function. If you wish to pass in arguments to the solve function, you can pass them
# in to cacheSolve, after the first argument.

cacheSolve <- function(cacheMatrix, ...) {
    solve <- cacheMatrix$getSolve()
    if (is.null(solve)) {
        message("will cache data")
        cacheMatrix$runSolve(...)
        solve <- cacheMatrix$getSolve()
    }
    else {
        message("getting cached data")
    }
    solve
}
