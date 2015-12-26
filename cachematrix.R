## Inverse of matrix is a high cost computacional function, it's better
## cache the valued to don't repeat this calculate.

## Create a object to contain the matrix and functions to 
## set and get the "value" of matrix and set and get the value of inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function determine if the value of inverse was calculated, 
## if not, it calculate and set this value, otherwise return 
## previous calculated value
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
