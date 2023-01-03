## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.
cacheSolve <- function(x, ...) {
## Grabbing inverse data from 'x' if it is not NULL
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
## Grabbing data from 'x'
  data <- x$get() 
  
## Computing inverse of 'x'
  m <- solve(data, ...) 
  
## Saving/Passing inverse of 'x' to makeCacheMatrix
  x$setsolve(m) 
  
## Return a matrix that is the inverse of 'x'
  m
}
