#### Write pair of functions and cache the inverse of the matrix

## Create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Cache the inverse of the matrix
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if (!is.null(m)){
                message("getting cached data")
                return (m)
        }
        data <- x$get()
        m <- solve(data, ...)## Return a matrix that is the inverse of 'x'
        x$setsolve(m)
        m
}

## Sample run
## x <- matrix(1:4, 2, 2)
## > a <- makeCacheMatrix(x)
## > a$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(a)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(a)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5