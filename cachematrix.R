## The makeCacheMatrix function creates the special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      a <- NULL
      set <- function(y) {
            x <<- y
            a <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) a <<- inverse
      getinverse <- function(inverse) a
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
      
}


## The cacheSolve function computes the inverse of the special matrix from the function makeCacheMatrix.

cacheSolve <- function(x, ...) {
      a <- x$getinverse()
      ## Checks whether the inverse has already been calculated
      ## If it has, returns the inverse:
      if(!is.null(a)){
            return(a)
      }
      ## If it hasn't, cacheSolve computes the inverse and returns it.
      data <- x$get()
      a <- solve(data, ...)
      x$setinverse(a)
      a
      
}
