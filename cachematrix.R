# Purpose: This function creates a special "matrix" object that can cache its inverse.
# Parameters: x - this function takes one parameter of type matrix.
# Return value: list - this function returns a list of functions that can be performed on the given matrix x.

# Example:
# m <- matrix(c(1,2,3,4), nrow=2, ncol=2) # creates a matrix with 2 rows and 2 columns and assigns it to variable m
# matrixObject <-makeCacheMatrix(m) #creates the "matrix object" 
# matrixObject$get() # returns m
# matrixObject$getInverseMatrix() # returns NULL as we haven't set the inverse matrix yet

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL
  
  #set the value of the matrix 'x'
  set <- function(y) {
    x <<- y
    # reset the inverMatrix value when setting a new matrix
    inverseMatrix <<- NULL
  }
  
  #retrieve the  value matrix 'x'
  get <- function() x
  
  #set the value of the inverse matrix of 'x' to the given value
  setInverseMatrix <- function(computedInverseMatrix) inverseMatrix <<- computedInverseMatrix
  
  #retrieve the value of the inverse matrix of 'x' 
  getInverseMatrix <- function() inverseMatrix
  
  
  #the value returned by the 'makeCacheMatrix' function - which is a list of functions available for a given matrix 'x'
  list(set = set, 
       get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix
  )
}

# Purpose: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# Assumption: matrix supplied is always invertible

# Parameters: x - This function takes a special "matrix" object x 
# Return value: inverseMatrix - a matrix which is the inverse of the matrix returned by x$get()

# Example
# m <- matrix(c(1,2,3,4), nrow=2, ncol=2) # creates a matrix with 2 rows and 2 columns and assigns it to variable m
# matrixObject <- makeCacheMatrix(m) #creates the "matrix object" 
# matrixObject$get() # returns m
#       [,1] [,2]
#[1,]    1    3
#[2,]    2    4

# matrixObject$getInverseMatrix() # returns NULL as we haven't set the inverse matrix yet
# NULL

# cacheSolve(matrixObject) # since matrixObject$getInverseMatrix() is NULL, the function will compute the inverseMatrix

# matrixObject$getInverseMatrix()  # this should now return the value of the inverse matrix

# cacheSolve(matrixObject) #running this function again should now retrieve the value of the inverse matrix stored in the special "matrix" object

# Returning the cached inverse matrix
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

cacheSolve <- function(x, ...) {
  
  # Retrive the value of the inverse matrix of the given "matrix" object 
  inverseMatrix <- x$getInverseMatrix()
  
  # If the reverse has already been set(cashed) return that value and exit the function
  if(!is.null(inverseMatrix)) {
    message("Returning the cached inverse matrix")
    return(inverseMatrix)
  }
  
  # At this point the inverse matrix has not been set yet so we need to compute it.
  
  # First retrieve the matrix for which we need to compute the inverse
  originalMatrix <- x$get()
  
  # Compute the inverse matrix (as per assumption the matrix given will always be invertible)
  inverseMatrix <- solve(originalMatrix)
  
  # Store the value of the inverseMatrix for future use by setting it in the special matrix object
  x$setInverseMatrix(inverseMatrix)
  
  # Return the inverse matrix
  inverseMatrix
  
}