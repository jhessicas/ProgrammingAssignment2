## PROGRAMMING ASSIGNMENT
## ASSIGMENT: CACHING THE INVERSE OF A MATRIX

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly (there are also alternatives to matrix inversion that we 
## will not discuss here). Your assignment is to write a pair of functions 
## that cache the inverse of a matrix.

## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## For this assignment, assume that the matrix supplied is always invertible.

## Write the following functions:

## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  
  ## Initialize the matrix inverse
  matInverse <- NULL
  
  ## Method to set and get the matrix
  set <- function(y) {
    x <<- y
    matInverse <<- NULL
  }
  get <- function(){ 
    x 
  }

  ## Method to set and get the inverse of a matrix
  setInverse <- function(inverse) matInverse <<- inverse
  getInverse <- function() matInverse

  ## Return
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## created by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then it should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'x'
  matInverse <- x$getInverse()

  if (!is.null(matInverse)) {
    message("getting cached data")
    return(matInverse)
  }

  ## Get the matrix, evaluate the inverse and set the inverse
  matriz <- x$get()
  matInverse <- solve(matriz, ...)
  x$setInverse(matInverse)

  ## Return
  matInverse
}

## Test
## matriz1 <- matrix(rnorm(16, 2, 0.5), 4, 4)
## matriz1
## matriz2 <- makeCacheMatrix(matriz1)
## cacheSolve(matriz2)