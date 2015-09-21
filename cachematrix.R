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
