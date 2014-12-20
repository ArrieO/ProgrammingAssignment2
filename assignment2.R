# read the R script
# replace the "path/to/file" with the directory you save the file into
# or you can read the file directly from the web
source("path/to/file/assessment3.R")

# create a *square* matrix (because `solve` only handles square matrices)
# create the matrix during the call of makeCacheMatrix()
a <- makeCacheMatrix( matrix(c(1,2,12,13), nrow = 2, ncol = 2) );

summary(a);
#>              Length Class  Mode    
#> setMatrix    1      -none- function
#> getMatrix    1      -none- function
#> cacheInverse 1      -none- function
#> getInverse   1      -none- function

a$getMatrix();
#>      [,1] [,2]
#> [1,]    1   12
#> [2,]    2   13

cacheSolve(a)
#> [,1]        [,2]
#> [1,] -1.1818182  1.09090909
#> [2,]  0.1818182 -0.09090909

# the 2nd time we run the function,we get the cached value
cacheSolve(a)
#> getting cached data
#> [,1]        [,2]
#> [1,] -1.1818182  1.09090909
#> [2,]  0.1818182 -0.09090909

Alternatively, the matrix can be created after calling a makeCacheMatrix without arguments.

# read the R script
# replace the "path/to/file" with the directory you save the file into
# or you can read the file directly from the web
source("path/to/file/assessment3.R")

# call makeCacheMatrix without arguments
a <- makeCacheMatrix();
summary(a);
#>              Length Class  Mode    
#> setMatrix    1      -none- function
#> getMatrix    1      -none- function
#> cacheInverse 1      -none- function
#> getInverse   1      -none- function

# create a square matrix (reason `solve` only handles square matrices )
a$setMatrix( matrix(c(1,2,12,13), nrow = 2, ncol = 2) );
a$getMatrix();
#>      [,1] [,2]
#> [1,]    1   12
#> [2,]    2   13

cacheSolve(a)
#> [,1]        [,2]
#> [1,] -1.1818182  1.09090909
#> [2,]  0.1818182 -0.09090909

# the 2nd time we run the function, we get the cached value
cacheSolve(a)
#> getting cached data
#> [,1]        [,2]
#> [1,] -1.1818182  1.09090909
#> [2,]  0.1818182 -0.09090909




============================================================================
# See README.md for instructions on running the code and output from it
# The assignment states that running the code is not part of the grading
# but I have the instructions anyway.
# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a matrix and a cached value of the inverse of the
# matrix. Contains the following functions:
# * setMatrix set the value of a matrix
# * getMatrix get the value of a matrix
# * cacheInverse get the cached value (inverse of the matrix)
# * getInverse get the cached value (inverse of the matrix)
#
# Notes:
# not sure how the "x = numeric()" part works in the argument list of the
# function, but it seems to be creating a variable "x" that is not reachable
# from the global environment, but is available in the environment of the
# makeCacheMatrix function

makeCacheMatrix <- function(x = numeric()) {
        # holds the cached value or NULL if nothing is cached
        # initially nothing is cached so set it to NULL
        cache <- NULL
        # store a matrix
        setMatrix <- function(newValue) {
                x <<- newValue
                # since the matrix is assigned a new value, flush the cache
                cache <<- NULL
        }
        # returns the stored matrix
        getMatrix <- function() {
                x
        }
        # cache the given argument
        cacheInverse <- function(solve) {
                cache <<- solve
        }
        # get the cached value
        getInverse <- function() {
                cache
        }
        # return a list. Each named element of the list is a function
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The following function calculates the inverse of a "special" matrix created with
# makeCacheMatrix

cacheSolve <- function(y, ...) {
        # get the cached value
        inverse <- y$getInverse()
                # if a cached value exists return it
                if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # otherwise get the matrix, caclulate the inverse and store it in
        # the cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        # return the inverse
        inverse
}

/**********************************************************/

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

/**********************************************************/

makeCacheMatrix <- function(x = numeric()) {
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


cacheSolve <- function(y, ...) {
        inverse <- y$getInverse()
                if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- y$getMatrix()
        inverse <- solve(data)
        y$setInverse(inverse)
        inverse
}

