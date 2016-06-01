## Assignment: Caching the Inverse of a Matrix
## Pair of functions that cache the inverse of a matrix.

##makeCacheMatrix function

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

## cacheSolve function

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Retrieving cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

##Sample Run from R
##x <- matrix(1:4, nrow=2, ncol=2)
##m <- makeCacheMatrix(x)
##s <- cacheSolve(m)
##x
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4

##s
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##s2 <- cacheSolve(m)
##Retrieving cached data.
##s2
 ##    [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
 

