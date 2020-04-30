makeCacheMatrix <- function(m = matrix()) {
  invrs <- NULL
  set <- function(f) {
    m <<- f
    invrs <<- NULL
  }
  get <- function() m
  setInverse <- function(inverse) invrs <<- inverse
  getInverse <- function() invrs
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(funm, ...) {
  inv <- funm$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- funm$get()
  inv <- solve(mat, ...)
  funm$setInverse(inv)
  inv
}

