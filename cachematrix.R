##Here we create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {
  inverse <- NULL
  set <- function(x) {
    mtx <<- x;
    inverse <<- NULL;
  }
  get <- function() return(mtx);
  set_inverse <- function(inv) inverse <<- inv;
  get_inverse <- function() return(inverse);
  return(list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse))
}

## This function computes the inverse of the matrix returned by `makeCacheMatrix`. If the inverse has
## already been calculated (and the matrix is the same), then ## `cacheSolve` should return 
## the inverse matrix from the cache.

cacheSolve <- function(mtx, ...) {
  inverse <- mtx$get_inverse()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- mtx$get()
  invserse <- solve(data, ...)
  mtx$set_inverse(inverse)
  return(inverse)
}