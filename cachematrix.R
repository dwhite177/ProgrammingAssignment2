## This script allows the user to calculate and cache the  
## inverse of a matrix. 

## When providing a matrix in the argument of this function,
## the user can cache the inverse of that matrix and also have
## access to four functions. 1) set the vector, 2) get
## the vector, 3) cache the inverse, 4) get the cached
## inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) inv <<- inverse
     getinv <- function() inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## When providing a matrix in the argument of this function,
## the function will return the inverse of the given matrix.
## If the inverse has been cached, it will return the cached
## inverse. If it has not been cached, it will calculate, 
## cache, and return the inverse of the matrix.

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}
