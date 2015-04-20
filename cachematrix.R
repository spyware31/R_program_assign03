
## First function starts here
makeCacheMatrix <- function(x = matrix()) {
  
  # initialize inv to NULL
  inv <- NULL
  
  # create the matrix in the working environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # invert the matrix and store in cache
  setinverse <- function(inverse) inv <<- inverse
  
  # get the inverted matrix from cache
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## 2nd function starts here

cacheSolve <- function(x, ...) {
  ## check to see if the inverse of the matrix is stored in cache
  cache <- x$getInverse()
  
  # returns inverted matrix from cache if it exists
  # else create the matrix in working environment
  if (!is.null(cache)) {
    message("getting cached data")
    
    # display matrix in console
    return(cache)
  }
  
  # create matrix since it does not exist
  matrix <- x$get()
  
  # make sure matrix is square and invertible
  # if not, handle exception cleanly
  tryCatch( {
    # set and return inverse of matrix
    cache <- solve(matrix, ...)
  },
  error = function(e) {
    message("Error:")
    message(e)
    
    return(NA)
  },
  warning = function(e) {
    message("Warning:")
    message(e)
    
    return(NA)
  },
  finally = {
    # set inverted matrix in cache
    x$setMatrix(cache)
  } )
  
  # display matrix in console
  return (cache)
}
