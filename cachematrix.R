# Test run of this inverted matrix calculation and caching functions.
# testM <- matrix(rnorm(4), nrow=2, ncol=2)
# testMinverse <- makeCacheMatrix(testM)
# cachesolve(testMinverse)


## This function creates a special "vector", which is really a list containing a function to hold cache inverse matrix.
## m caches and returns inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        #1. set the value of the matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #2. get the value of the matrix
        get <- function() x
        #3.set the value of the inversed matrix
        setInverse <- function(inverse) m <<- inverse
        #4.get the value of the inversed matrix
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## m caches and returns inverse matrix

cachesolve <- function(x, ...) {
        #first check if inverse of matrix has already been calculated and cached for this matrix
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #otherwise, calculate the inverse matrix using solve
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
