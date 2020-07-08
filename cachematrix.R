makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function() inverse <<- solve(x)
  getInverse <- function() inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inverse)) {
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  inverse
}