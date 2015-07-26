## The functions contained in this file enable the creation and use of an
## object that represents a matrix whose inverse (if the cacheSolve function
## is used each time the inverse is required) is calculated at most once. The
## matrix represented by the object is assumed to be invertible.

## This function creates an object that represents the matrix x and supports
## caching of the calculation of x's inverse. It returns a list of functions
## that can be used to access and modify the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  getInverse <- function() inv
  setInverse <- function(i) inv <<- i
  list(get = get,
       set = set,
       getInverse = getInverse,
       setInverse = setInverse)
}


## This function returns the inverse of a matrix created by makeCacheMatrix.
## This calculation is cached if possible. The matrix is assumed to be
## invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (is.null(inv)) {
    ## Caluculate the inverse of x if necessary.
    inv <- solve(x$get())
    x$setInverse(inv)
  }
  inv
}
