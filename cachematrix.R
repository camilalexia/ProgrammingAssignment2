#Programming Assignment 2 coursera
## This function creates a special "matrix" object that can cache its inverse

#We will use the solve function that returns the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache using the function SOLVE()
#If the matrix had already been created before, and you wany to have his inverse again, you can call the function makeCacheMatrix and you can get the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of "x"
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


