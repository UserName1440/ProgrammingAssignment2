## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  Original <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, Original = Original,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.
cacheSolve <- function(x, ...) {
## Grabbing inverse data from 'x' if it is not NULL
  MatrixInverse <- x$getInverse()
  if(!is.null(MatrixInverse)) {
    message("getting cached data")
    return(MatrixInverse)
  }
  
## Grabbing data from 'x'
  MatrixOld <- x$Original() 
  
## Computing inverse of 'x'
  MatrixInverse <- solve(MatrixOld, ...) 
  
## Saving/Passing inverse of 'x' to makeCacheMatrix
  x$setInverse(MatrixInverse) 
  
## Return a matrix that is the inverse of 'x'
  MatrixInverse
}
