## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse cache as NULL
  set <- function(y) {
    x <<- y  # Assign the new matrix to the matrix object x
    inv <<- NULL  # Reset the cached inverse to NULL because the matrix has changed
  }
  get <- function() x  # Retrieve the matrix stored in x
  setinv <- function(m_inverse) inv <<- m_inverse  # Set the cached inverse value
  getinv <- function() inv  # Retrieve the cached inverse
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()  # Try to get the cached inverse from the object
  if(!is.null(inv)) {
    message("getting cached data")  # If the inverse is cached, return it directly
    return(inv)
  }
  data <- x$get()  # Get the matrix data
  inv <- solve(data, ...)  # Compute the inverse of the matrix using `solve()`
  x$setinv(inv)  # Store the computed inverse in the cache
  inv  # Return the computed inverse
}

matrix_data <- matrix(c(2, 1, 3, 4, 2, 1, 3, 4, 5), nrow = 3, ncol = 3)
cached_matrix <- makeCacheMatrix(matrix_data)
inv_matrix <- cacheSolve(cached_matrix)
print(inv_matrix)