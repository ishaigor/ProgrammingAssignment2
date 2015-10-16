## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## function to set the data (matrix)
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## function to retrieve the data (matrix)
  get <- function() x
  ## function to set the inverse of data (matrix)
  setInv <- function(inverse) inv <<- inverse
  ## function to retrieve the inverse of data (matrix)
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  ## use cached value if present
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## calculate the inverse and store the result for re-use
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
