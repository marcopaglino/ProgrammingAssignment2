## The function makeCacheMatrix creates a special "matrix" object (a list)
## that can cache the inverse of a matrix.
## The function cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix.

## The makeCacheMatrix has one argument: a square matrix that it's 
## supposed to be invertible. It returns a list of four functions to get or set 
## the value of the matrix and to set or get the inverse of the matrix to or from
## the cache.

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve has the first argumets x that is the special "matrix" computed
## by makeCacheMatrix; the second argument is the original matrix: if not specified
## it's defaulted to the cached matrix. Further arguments can be passed to the
## solve method. If the original matrix has changed, the cached matrix value
## and its inverse are updated. If the original matrix has not changed and
## the inverse has already been calculated, the inverse is retrieved from the cache

cacheSolve <- function(x, m=x$get() ,...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (identical(m,x$get())) {
                if(!is.null(inv) ) {
                        message("getting cached data")
                        return(inv)
                }  
        } else {
                message("matrix changed")
                x$set(m)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
