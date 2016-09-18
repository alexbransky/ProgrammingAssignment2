## Matrix inversion is "usually a costly computation" and 
## there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly. The
## following 2 functions are used to cache the inverse of a matrix.


## makeCacheMatrix creates a list
## the list contains a function which:
## sets the value of the matrix
## gets the value of the matrix
## sets the value of inverse of the matrix
## gets the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## cacheSolve gets the inverse of the matrix from the cache if 
## it has already been computed

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
