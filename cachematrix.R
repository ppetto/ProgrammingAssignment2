## These functions work similarly to the example makeVector and cachemean functions, but have been
## rewritten to work on square invertible matrices, computing an inverse instead of a mean.

## They DO NOT check the input to make sure it is a square invertible matrix.


## This is a function-containing list that includes four functions to:
## set - set the value of the matrix
## get - get the value of the matrix
## setinverse - set the value of the inverse
## getinverse - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      mat <- NULL
      set <- function(y) {
            x <<- y
            mat <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) mat <<- solve
      getinverse <- function() mat
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## this function calculates the inverse of the special "matrix" created with the above function,
## if it has NOT already been calculated -- if it has, it retrieves the inverse from cache

cacheSolve <- function(x, ...) {
      mat <- x$getinverse()
      if(!is.null(mat)) {
            message("getting cached data")
            return(mat)
      }
      data <- x$get()
      mat <- solve(data, ...)
      x$setinverse(mat)
      mat
}
c