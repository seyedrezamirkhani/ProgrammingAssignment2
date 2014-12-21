## The makeCacheMatrix and cacheSolve functions work together to cache (using
## global variables) and return the inverse of a matrix. Then ensures the 
## processing required for generating the inverse for a given matrix is only
## performed once
## !NOTE! It is assumed that the matrix supplied is always invertible 
## !NOTE! Use the det function to see if this is the case

## makeCacheMatrix creates a special "vector" (list) containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "vector" created with the
## makeCacheMatrix function. This function first checks if inverse has already
## been calculated. If so, it gets the inverse from the cache and skips the 
## computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the
## inverse in the cache via the setinverse function.
## This function generates a diagnostic message to indicate that cached data is 
## used

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } 
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
