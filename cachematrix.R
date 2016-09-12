## makeCacheMatrix() creates a special matrix object that can cache 
##    its inverse matrix

## cacheSolve() computes the inverse of the special matrix returned
##    by makeCacheMatrix above. If the inverse has already been calculate
##    and the matrix has not been changed, it will retrieve the inverse from the cache.

## To use: m is a square matrix
##    x<- makeCacheMatrix(m) 
##    cacheSolve(x)

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(Inverse) m <<- Inverse
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached Inversed Matrix")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m
}
