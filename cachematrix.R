## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix takes in a matrix and has the following functions
## setMatrix - setter method to set the Matrix
## getMatrix - getter method to retrieve the Matrix
## setInverse - setter method to set the Inverse
## getInverse - getter method to retrieve the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
        matrix_value <- NULL
        setMatrix <- function(y) {
        x <<- y
        matrix_value <<- NULL
         }
        getMatrix <- function() x
        setInverse <- function(inv) matrix_value <<- inv
        getInverse <- function() matrix_value
  
         list(setMatrix = setMatrix, getMatrix = getMatrix,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve function checks for whether inverse is available or not
## If inverse is not available, matrix is got from getMatrix function 
## and inverse is got from solve function 
## the value is then stored 
## If inverse is available,it returns the matrix value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_value <- x$getInverse()
        if(!is.null(matrix_value)) {
        message("getting cached data")
        return(matrix_value)#retrieving from stored matrix
        }
        data <- x$getMatrix()
        matrix_value <- solve(data, ...)
        x$setInverse(matrix_value)
        matrix_value
}
