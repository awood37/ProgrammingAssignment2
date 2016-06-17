## Coursera Week 3 assignment created by ALEX WOOD

## The first function will create a matrix object that
## is able to cache its own inverse,
## while the second function will compute the inverse of the same matrix
## returned by makeCacheMatrix; but if the inverse has been
## calculated, then the second function should get the inverse from the cache
## that was created by the first function.


# the first function, which will create a matrix 
# object that chaches its inverse.

makeCacheMatrix <- function(x = matrix()){
    invert <- NULL
    set <- function(y) {
        x <<- y
        invert <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invert <<- inverse
    getInverse <- function() invert
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# This is the second function that will compute the inverse of the matrix
# object created by the function above.

cacheSolve <- function(x, ...){
    invert <- x$getInverse()
    if(!is.null(invert)) {
        message("getting cached data")
        return(invert)
    }
    matrixI <- x$get()
    invert <- solve(matrixI, ...)
    x$setInverse(invert)
    invert
    # returns the inverse
}