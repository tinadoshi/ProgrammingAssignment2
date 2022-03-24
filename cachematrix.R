## These two functions cache the inverse of a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     list(set = set, get = get, setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculate (and
## the matrix has note changed), cacheSolve retrieves the inverse from
## the cache. (This assumes the matrix is invertible.)

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached matrix")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}
