## cached matrix inversion functions for 
## R Programming programming assignment 2
## demonstrates lexical scoping
## https://github.com/VetEricChan/


makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse to NULL
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  set_inv <- function(inverse) i <<- inverse
  get_inv <- function() i
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}

## Write a short comment describing this function
## cache solves the results
## if the matrix have been calculated 
## returns cached results instead

cacheSolve <- function(x, ...) {
  i <- x$get_inv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # else calculate, store and return it
  data <- x$get()
  i <- solve(data, ...)
  x$set_inv(i)
  i
}

## Return a matrix that is the inverse of 'x'