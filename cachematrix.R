## avoid repeating same work of finding inverse of a square matrix


## make an object that holds basic operations and data

makeCacheMatrix <- function(x = matrix()) {
  mInverse <- NULL
  set <- function(y) {
    x <<- y
    mInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(m) mInverse <<- m
  getInverse <- function() mInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## smartly find or compute the inverse. It stores the result the first time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
  ## check if it is a mtrix
  if (!is.matrix(x))
    return(NA)
    
  ## check if it is a SQUARE matrix
  if (!(nrow(x) == ncol(x)))
    return(NA)
  
  ## check if the solution exists.  If yes, return it.
  mInverse <- x$getInverse()
  if(!is.null(mInverse)) {   
    message("getting cached data")
    return(mInverse)
  }
  
  ## if no existing solution, 
  data <- x$get
  
  ## make sure the inverse exists by checking its determinant
  d <- det(data)
  if(d == 0)
    return(NA)
  
  ## compute inverse and store it
  mInverse <- solve(data)
  x$setInverse(mInverse)
  
  ## return result
  mInverse
}
