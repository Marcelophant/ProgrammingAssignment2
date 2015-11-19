## These two functions use a matrix as input and then store both the matrix
## and its inverse in its cache. The matrix has to be square, i.e. invertible.

## Teh makeCacheMatrix function takes a matrix as input and returns a list of
## functions which can be used to cache both the matrix and its inverse. The
## functions "set" and "setinverse" set the cached matrix or its inverse while
## "get" and "getinverse" return the cached matrix or its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve takes as input an object created by makeCacheMatrix
## and then checks if the inverse of the cached matrix in x exists. If it does
## not exist the function coputes the inverse and stores it in the object x.
## In either case it will return the inverse matrix.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached Inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
