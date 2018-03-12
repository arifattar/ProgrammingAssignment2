## These are a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {   # initialising of x as a function argument
       i <- NULL                              # initialising of i to be used later
       
       set <- function(y) {
              x <<- y                         # assigns the input argument to the function to object x in the parent environment
              i <<- NULL                      # assigns NULL to i and clears any value of i it may have had from a previous execution of cacheSolve
       }
       get <- function() x                    # defines the getter for matrix x
       
       setinverse <- function(solve) i <<- solve  # defines the setter for the inverse matrix i
       
       getinverse <- function() i             # this function defines the getter for inverse matrix i
       
       ## This part of the code assigns each of the functions defined above as an element within a list()
       ## and returns it to the parent environment

       list(set = set,                        # gives the name 'set' to the set() function defined above
            get = get,                        # gives the name 'get' to the get() function defined above
            setinverse = setinverse,          # gives the name 'setinverse' to the setinverse() function defined above
            getinverse = getinverse)          # gives the name 'getinverse' to the getinverse() function defined above
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
          i <- x$getinverse()                 # calling the getinverse() function to retreive an inverse of the matrix passed as the argument
          
           ## The code then checks if the result of the above call is NULL. Since the makeCacheMatrix() functions sets the cached inverse to NULL
           ## whenever a new matrix is set into the object, if the value here is not equal to NULL, we have a valid cached inverse and can return
           ## it to the parent environment

           if(!is.null(i)) {           
                  message("getting cached data")
                  return (i)
           }
           
           ## If the result of !is.null(i) is FALSE, cacheSolve() gets the matrix from the input object, calculates an inverse by solve(), uses the setinverse() function 
           ## on the input object to set the inverse in the input object, and then returns the value of the inverse to the parent environment by printing the inverse object.

          
          data <- x$get()
          i <- solve(data, ...)
          x$setinverse(i)
          i
}
