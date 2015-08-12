
## makeCacheMatrix returns functions to get or set a cached inverted Matrix
makeCacheMatrix <- function(x = matrix()) {
  # stores the cached value
  # initialize to NULL
  cachedMatrix <- NULL
  
  # create the matrix in the working environment
  set <- function(y) {
    x <<- y
    cachedMatrix <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  # store inverse in cache
  setInverse <- function(inverse) cachedMatrix <<- inverse
  # get the inverted matrix from cache
  getInverse <- function() cachedMatrix
  
  # return the created functions to the working environment
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## cacheSolve produces the inverse of a matrix returned from makeCacheMatrix
## If the inverted matrix does not yet exist it is created and cached

cacheSolve <- function(x, ...) {
  # get the cached matrix
  cachedMatrix <- x$getInverse()
  
  # return matrix if exists
  if (!is.null(cachedMatrix)) {
    message("getting cached data")
    
    # display matrix in console
    return(cachedMatrix)
  }
  
  # matrix does not exist so create from makeCachedMatrix.get()
  matrix <- x$get()
  
  # use try catch to handle exceptions
  tryCatch( 
  {
    # solve() function solves equation a %*% x = b for x, where b is a vector or matrix.
    cachedMatrix <- solve(matrix, ...)
  },
  error = function(e) 
  {
    message("Solving this matrix caused an Error:")
    message(e)
    
    return(NA)
  },
  warning = function(w) 
  {
    message("Solving this matrix caused a Warning:")
    message(w)
    
    return(NA)
  },
  finally = 
  {
    # set inverted matrix in cache
    x$setMatrix(cachedMatrix)
  } 
  )
  
  return (cachedMatrix)
}
