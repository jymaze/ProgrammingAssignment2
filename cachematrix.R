## This file contains 2 functions that cache the inverse of a matrix.
## I chose to use explicit variable names and emphasize structure.
## "Readability counts" Tim Peters

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(stored_matrix = numeric()) {
  
  stored_inverse <- NULL
  
  set <- function(input_matrix) {
    stored_matrix <<- input_matrix
    stored_inverse <<- NULL
  }
  
  get <- function() {
    return(stored_matrix)
  }
  
  set_inverse <- function(input_inverse) {
    stored_inverse <<- input_inverse
  }
  
  get_inverse <- function() {
    return(stored_inverse)
  }
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(target_matrix, ...) {
  
  queried_inverse <- target_matrix$get_inverse()
  
  if(!is.null(queried_inverse)) {
    message("getting cached data")
    return(queried_inverse)
  }
  
  else {
    matrix_to_solve <- target_matrix$get()
    calculated_inverse <- solve(matrix_to_solve, ...)
    target_matrix$set_inverse(calculated_inverse)
    return(calculated_inverse)
  }
}
