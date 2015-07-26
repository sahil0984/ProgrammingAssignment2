## This file provides helper functions to calculate and cache the inverse of a matrix.
## There are 2 functions in the file:
## ****makeCacheMatrix****: This function creates a special matrix object which returns a list of 
## functions that help set and get the matrix and its inverse.
## ****cacheSolve****: This function tries to retrieve the inverse of the matrix of the special matrix 
## object created using makeCacheMatrix from the cache and if it cannot find it in the cache, it computes 
## the inverse and returns it.

## NOTE: The assumption is that the matrix supplied is always invertible.

## Example:
## > aMat <- matrix(c(4,3,3,2), nrow = 2, ncol = 2)
## > aMatObj <- makeCacheMatrix(aMat)
## > cacheSolve(aMatObj)
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4

## ****makeCacheMatrix****: This function creates a special matrix object which returns a list of functions
## that help set and get the matrix and its inverse. The list of functions returned are:
## 1. set(y): Sets matrix y to the envioronment variable x of the makeCacheMatrix environment
## 2. get(): Retrieves the matrix x from the makeCacheMatrix environment
## 3. setInv(inv): Sets the inverse of matrix to xInv in makeCacheMatrix
## 4. getInv(): Retrieves the inverse of the matrix xInv from the makeCacheMatrix environment

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  ##Sets the matrix in the special object
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  ##Gets the matrix from the special object
  get <- function() x
  ##Sets the inverse of matrix in the special object
  setInv <- function(inv) xInv <<- inv
  ##Gets the inverse of matrix in the special object
  getInv <- function() xInv
  ##Returns the special object as a list of functions
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## ****cacheSolve****: This function tries to retrieve the inverse of the matrix, using the special matrix 
## object created with makeCacheMatrix, from the cache and if it cannot find it in the cache then it 
## computes the inverse and returns it.

cacheSolve <- function(x, ...) {
  ##Try to get the matrix inverse from the cache
  xInv <- x$getInv()
  ##Check if the matrix inverse exists in the cache and return it
  if(!is.null(xInv)) {
    message("getting cached data")
    return(xInv)
  }
  ##If not cached, get the matrix from the special object
  data <- x$get()
  ##Compute the matrix inverse
  xInv <- solve(data)
  ##Store the matrix inverse in the cache for future use
  x$setInv(xInv)
  ##Return the computed matrix inverse
  xInv
}
