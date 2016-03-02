## Functions that cache the inverse of a matrix
##
## Usage example:
##
## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(3, 0, 0, 3), c(2, 2)))
##> cacheSolve(m)
##         [,1]      [,2]
##[1,] 0.3333333 0.0000000
##[2,] 0.0000000 0.3333333
##> cacheSolve(m)
##getting cached data
##          [,1]      [,2]
##[1,] 0.3333333 0.0000000
##[2,] 0.0000000 0.3333333

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}