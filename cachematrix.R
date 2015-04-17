## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
  if (!is.matrix(x))
    return(NA)
  
  if (!(nrow(x) == ncol(x)))
    return(NA)
  
  mInverse <- x$getInverse()
  if(!is.null(mInverse)) { 
    message("getting cached data")
    return(mInverse)
  }
  
  data <- x$get
  d <- det(data)
  if(d == 0)
    return(NA)
  
  mInverse <- solve(data)
  x$setInverse(mInverse)
  mInverse
}
