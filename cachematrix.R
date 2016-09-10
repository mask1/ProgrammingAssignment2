# These two functions take a matrix input and compute and output
# its inverse matrix.
#
## This function initializes the matrix 'x' and its 
## inverse in its own environment and in cache.
makeCacheMatrix <- function(x = matrix()){
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverseMatrix <<- solve
  getinverse <- function() inverseMatrix
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}
#
## cacheSolve takes any square matrix and checks whether its 
## inverse had been cached and retrieves it;
## if not, the inverse is computed.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)){
    message("getting cache data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setinverse(inverseMatrix)
  inverseMatrix
}