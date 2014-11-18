## The following functions cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NA
  set_m <- function(y) {
    x <<- y
    inv <<- NA           ## if a matrix has changed, its reverse is undefined
  }
  get_m <- function() x
  set_inv <- function(inv_x) inv <<- inv_x
  get_inv <- function() inv
  list(set_m = set_m, get_m = get_m, set_inv = set_inv, get_inv = get_inv)
  
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  
  inv <- x$get_inv()
  
## Checking if the inverse is already calculated
  if(is.matrix(inv)) {
    message("retrieving the inverse of a matrix from the cache")
    return(inv)
  }
  m <- x$get_m()
  inv_x <- solve(m)
  x$set_inv(inv_x)
  inv_x           ## Return a matrix that is the inverse of x
}

