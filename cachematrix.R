## Coursera R-Programming rprog-031
## Assignment: Caching the Inverse of a Matrix 

## This function creates a special "matrix" 
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  SetInvMatrix <- function(InvMatrix) m <<- InvMatrix
  GetInvMatrix <- function() m
  list(set = set, get = get,
       SetInvMatrix = SetInvMatrix,
       GetInvMatrix = GetinvMatrix)
} 


## The following function calculates the inverse of the matrix
## created in the function above.
## However, it first checks to see if the inverse
## has already been calculated.

cacheSolve <- function(x, ...) {
  MatrixInverse <- x$getinvmatrix() 
  if(!is.null(m)) {  #Check to see if the inverse is cached (not null)
    message("Getting Cached Data")
    return(m)
  }
  data <- x$get()  #not cached, calculate the inverse
  MatrixInverse <- solve(data, ...)
  x$setinvmatrix(MatrixInverse)
  MatrixInverse
  }
