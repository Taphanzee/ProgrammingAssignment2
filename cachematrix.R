## These two functions are designed to reduce the timely computation of inverted matrices by 
## caching any previously computed inverted matrices and referencing them as needed.

## makeCacheMatrix is a function creates a matrix object that can cache its inverse.
## The cacheSolve function uses this list to avoid re-calculating inverted matrices.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversematrix <- function(inverse) m <<- inverse
        getinversematrix <- function() m
        list(set=set, get=get, setinversematrix=setinversematrix, getinversematrix=getinversematrix)
}


## cacheSolve is a function that returns the inverse of a matrix.  If the matrix already exists in
## makeCacheMatrix, then the stored result is displayed and no computation of the inverse matrix is
## performed.  If the matrix does not exist in makeCacheMatrix, then the function computes the inverse
## and stores it in makeCacheMatrix for future reference.  This function assumes that all matrices 
## entered are invertible.

cacheSolve <- function(x, ...) {
        m <- x$getinversematrix()
        if(!is.null(m)) {
                message("Getting cached data...")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)    ## Return a matrix that is the inverse of 'x'
        x$setinversematrix(m)
        m
}
