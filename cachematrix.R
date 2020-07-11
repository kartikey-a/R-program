#1. makeCacheMatrix function
#This function creates a special matrix that can be cached later
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y 
    inverse <<- NULL #Cached here
  }
  
  #Below three are one-line new functions created to put in the CacheSolve function below. 
  #These one-liners make the code for the next function shorter.
  get <- function() x
  setInverse <- function() inverse <<- solve(x)
  getInverse <- function() inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
#2. cacheSolve function
cacheSolve <- function(x, ...) { #This function gives the inverse of the cached matrix
  inv <- x$getInverse()
  if (!is.null(inverse)) {
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  inverse
}
