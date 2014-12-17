## The purpose of the functions is to calculate and store the inverse
## values of a matrix

## Function that creates a matrix and initializes its inverse 

makeCacheMatrix <- function(x = matrix()) {
    ##initializing the inverve value with NULL in the global envirionment
    inv <- NULL
    ##setting the values of the matrix in the global environment
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ##function for getting the values in the matrix
    get <- function() x
    ## function for setting the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    ##function for retreiving inverse matrix from the cache
    getInverse <- function() inv
    
}


## Function the calculates the inverse of a matrix if the matrix's inverse
## cannot be looked up in the cache

cacheSolve <- function(x, ...) {
    ## check whether the inverse is in the cache
    inv <- x$getInverse()
    ## if it was calculated before, return the stored inverse values
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## if it wasn't calculated before, calculate it and store it for later use
    data <- x$get()
    ##calculate the inverse of the matrix
    inv <- solve(data, ...)
    ## store the value in the cache
    x$setInverse(inv)
    
    ## Return the inverse matrix
    inv
    
}
