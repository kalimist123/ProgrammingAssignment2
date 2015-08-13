## makeCacheMatrix returns functions to get or set a cached inverted Matrix
makeCacheMatrix <- function(x = matrix()) {
  
  cache <- NULL
  set <- function(y) {
    # use '<<-' to assign a value to an object in an environment 
    # outside the current environment. 
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cache <<- inverse
  getInverse <- function() cache
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## cacheSolve produces the inverse of a matrix returned from makeCacheMatrix
## If the inverted matrix does not yet exist it is created and cached

cacheSolve <- function(x, ...) { 
  inverse<- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  # solve() function solves equation a %*% x = b for x, where b is a vector or matrix.
  inverse<- solve(data, ...)
  x$setInverse(inverse)
  inverse
  
}
