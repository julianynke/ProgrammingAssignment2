## This function receives as input a square matrix that can be inverted and 
## creates a "special" matrix. This "special" matrix object can cache its 
## inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## 'x' is a matrix representing the matrix which we would like to cache 
  ## the inverse.
  
  ## Returns a list, the "special" matrix, which can be used to cache the matrix
  ## and its inverse.
  
  # Initialize the inverse variable.
  inv <- NULL
  
  # Define the functions to set and get the matrix.
  set <- function(y) {
    # Assign value to the variables x and inv.
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  # Define the functions to set and get the inverse of the matrix.
  set_inv <- function(inverse_matrix) inv <<- inverse_matrix
  get_inv <- function() inv
  
  # Create the "special" matrix aka the list.
  list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}


## This functions receives as input a list, also considered a "special" matrix 
## and determines whether the inverse of the matrix in the list has been 
## calculated. If it has been it retrieves the inverse, otherwise the function 
## calculates the inverse and caches it. This function returns the inverse of a 
## matrix. 
cacheSolve <- function(x, ...) {
  ## 'x' is a list, a "special" matrix, this contains a matrix and can contain 
  ## the cached inverse of a matrix.
  
  ## Return a matrix that is the inverse of 'x'.
  
  # Determine if the inverse of x has been cached, if it has the function 
  # returns is.
  inv <- x$get_inv()
  if (!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  }
  
  # Load the matrix and calculate the inverse.
  data <- x$get()
  inv <- solve(data)
  
  # Cache the new inverse.
  x$set_inv(inv)
  inv
}
