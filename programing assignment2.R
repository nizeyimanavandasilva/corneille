# Function to create a special matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverse to NULL
  inverse <- NULL
  
  # Setter function to set the matrix
  set <- function(y) {
    x <<- y
    # When the matrix is set, invalidate the cached inverse
    inverse <<- NULL
  }
  
  # Getter function to get the matrix
  get <- function() {
    x
  }
  
  # Function to compute the inverse
  cacheSolve <- function(...) {
    # Check if the inverse is already cached
    if (!is.null(inverse)) {
      message("Getting cached inverse")
      return(inverse)
    }
    
    # If not, calculate the inverse
    inverse <- solve(x, ...)
    # Cache the inverse
    x <<- inverse
    inverse
  }
  
  # Return a list of functions
  list(set = set, get = get, cacheSolve = cacheSolve)
}

# Example usage:
# Create a matrix
m <- makeCacheMatrix(matrix(1:4, 2, 2))

# Get the matrix
m$get()
# Set the matrix
m$set(matrix(c(4, 3, 2, 1), 2, 2))

# Get the matrix again
m$get()

# Compute the inverse (it will cache it)
m$cacheSolve()

# Get the cached inverse