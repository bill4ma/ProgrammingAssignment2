## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly. The
## following functions speed up to solve matrix inversion.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      cached_inverse <- NULL
      set <- function(y) {
            x <<- y
            cached_inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse_m) cached_inverse <<- inverse_m
      getinverse <- function() cached_inverse
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse_m <- x$getinverse()
      if (!is.null(inverse_m)) {
            message("Use cached inverse")
            return(inverse_m)
      }
      data <- x$get()
      inverse_m <- solve(data)
      x$setinverse(inverse_m)
      inverse_m
}
