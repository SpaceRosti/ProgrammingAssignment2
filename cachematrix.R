## These functions create a special object who stock a matrix,
## compute its inverse and keep it in the cache

## This function creates a special matrix and returns a list of function to get and set the value
## of a matrix and to get and set the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the matrix created
## by the function makeCacheMatrix and if the inverse is stored
## in the cache, she returns the inverse from the cache instead 
## of doing the computation

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i       
    ## Return a matrix that is the inverse of 'x'
}
