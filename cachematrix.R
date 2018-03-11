## These are a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
       i <- NULL
       set <- function(y) {
              x <<- y
              i <<- y
       }
       get <- function() x
       setinverse <- function(solve) i <<- solve
       getinverse <- function() i
       list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
          i <- x$getinverse()
          if(!is.null(i)) {           ## Return a matrix that is the inverse of 'x'
                  message("getting cached data")
                  return (i)
           }
          data <- x$get()
          i <- solve(data, ...)
          x$setinverse(i)
          i
}
