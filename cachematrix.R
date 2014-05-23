## Because using R's solve(matrix) function to find a matrix's inverse
## can burden system resources, these functions create a specialized
## "matrix" object that allows the inverse of the matrix to be cached
## for quick retrieval later on.

## the makeCacheMatrix function creates a special "matrix" object
## that is actually a list of four functions, written to
## 1) cache the value of the matrix arg, via the set() function
## 2) return the value of the matrix, via the get() function
## 3) cache the value of the inversed matrix, via the setSolve() function
## 4) return the value of the inversed matrix, via the getSolve() function

makeCacheMatrix <- function(x = matrix()) {
    S <- NULL # S is the inverse of the matrix. Initialize to NULL.
    set <- function(y) { # the set() function will take one arg y,
        x <<- y          # set local x to value of y (the matrix), for caching,
        S <<- NULL       # and reset S to NULL. Because the matrix has changed,
    }                    # the inverse of the matrix S must be recomputed later
    get <- function() x  # the get() function will simply return the matrix x
    setSolve <- function(Solve) S <<- Solve # the setSolve() function caches the 
                                            # matrix inverse as S
    getSolve <- function() S # the getSolve() function returns the cached matrix 
                             # inverse value S
    return(list(set = set, get = get,
           setSolve = setSolve,
           getSolve = getSolve))
}

## the cacheSolve function takes the "matrix" object created
## by the makeCacheMatrix() function and takes one of two actions:
## 1) if the inverse of the "matrix" object has already been set, return it
## 2) otherwise, use solve(matrix) to find the inverse, and cache it using
## the "matrix" object's setSolve method, for later use

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    S <- x$getSolve()
    if(!is.null(S)) {
        message("getting cached data")
        return(S)
    }
    data <- x$get()
    S <- solve(data)
    x$setSolve(S)
    return(S)
}
