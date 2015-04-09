t give an overall description of what your
## functions do

## Write a short comment describing this function

#The following two functions are used to cache the calculation 
#of the inverse of a matrix. The caching of the matrix is done by 
#makeCacheMatrix, that creates a list containing a function to: 

# 1. set the value of a matrix
# 2. get the value of a matrix
# 3. set the value of the inverse of a matrix
# 4. get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
      inve <- NULL
      set <- function(y) {
      x <<- y
      inve <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inve <<- inverse
    getinverse <- function() inve
    list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}



## Write a short comment describing this function

# The following function first checks if the inverse of the matrix
# has already been computed. If so, it gets the result and skips the
# computation. If the inverse has not yet been computed, it computes
# the inverse, and sets the value in the cache,
# using the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
      inve <- x$getinverse()
      if(!is.null(inve)) {
          message("getting cached data")
          return(inve)
      }
      data <- x$get()
      inve <- solve(data)
      x$setinverse(inve)
      inve
}


