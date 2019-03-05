## makeCacheMatrix is a function that takes a matrix "x" and apply 4 functions on "x"

makeCacheMatrix <- function(x = matrix()) {
  inverse_result <- NULL
## 1- set matrix values
  setMatrix <- function(y) {              
    x<<-y
    inverse_result <<- NULL
  }
##2- get matrix values  
  getMatrix <- function() x
## 3- set inverse matrix value
  setInverse <- function(inverse) 
    inverse_result <<- inverse
## 3- set inverse matrix value
  getInverse <- function() inverse_result
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}



## cacheSolve returns the matrix inverse result if exist,
##otherwise calculate tha matrix invers
cacheSolve <- function(x, ...) {
## get the curent inverse valuse  
  inverse <- x$getInverse()
## return the current inverse value if exist
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
## calculate and return the matrix inverse if not exist before
  data <- x$getMatrix()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
