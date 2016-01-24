## makeCacheMatrix and cacheSolve are used to convert a matrix to an object
## that is able to store and retrieve a cached version of the given matrix and
## of its inverse matrix

## makeCacheMatrix creates an object (a list) containin four functions, used to
## set the matrix, set its inverse, read the matrix and read its inverse
## This approach exploits the Lexical Scoping feature of R: the two variables
## containing the matrix and its inverse (x and inverse respectively) are
## created when the cacheMatrix object is instantiated, and updated through
## the methods associated to the object

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solved) inverse <<- solved
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes a matrix created through makeCacheMatrix, checks if its
## inverse has been already calculated, and returns either the cached value
## or it inverts the matrix (through the solve() function), caches the inverse
## and then returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
