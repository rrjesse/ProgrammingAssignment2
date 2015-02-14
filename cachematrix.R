## These two functions work together to cache the inverse of a matrix to save time recomputing
## the inverse over and over again (especially important for looping)

## This function creates a vector of functions that will help to cache the inverse of a matrix.  The original call to
## The original call should look like this:  x <- make CacheMatrix(invertible_matrix) where "invertible_matrix" 
## represents a matrix that must have an inverse.  The cacheSolve function can then be called as x as its variable.
## The inverse of the matrix should be returned.  Another call to cacheSolve, without resetting the input matrix,
## will return the inverse from cache and be denoted with a message "getting cached data".  A new matrix can be 
## entered with the x$set(another_matrix) command.  x$get() can be used to check the contents of the input matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y)  {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function finds the inverse of a new matrix or will return the cached value of the given matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i))  {
                message("getting cached data")
                return(i)
        }
        data <- matrix()
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}
