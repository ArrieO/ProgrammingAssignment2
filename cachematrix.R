# makeCacheMatrix is a function that returns a list of functions
# The purpose is to store a matrix and a cached value of the inverse of the
# matrix. It contains the following functions:
# * setMatrix set the value of a matrix
# * getMatrix get the value of a matrix
# * setInverse set the cached value (inverse of the matrix)
# * getInverse get the cached value (inverse of the matrix)

# example how to run the function:
# a <- makeCacheMatrix( matrix(c(1,2,12,13), nrow = 2, ncol = 2) );
# cacheSolve(a)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # get the cached value
        inverse <- x$getInverse()
                if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # otherwise get the matrix, calculate the inverse and store in cache
        data <- x$getMatrix()
        inverse <- solve(data)
        x$setInverse(inverse)
        # return the inverse
        inverse
}
