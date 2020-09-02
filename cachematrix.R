## The first function, makeCacheMatrix, creates an R object that stores 
## a matrix and its inverse.

## The second function, cacheSolve, requires an argument that is 
## returned by makeCacheMatrix in order to retrieve the inverse matrix from the
## cached value that is stored in the makeCacheMatrix() object's environment.

## Steps to execute:
## 1) If not already in your environment, load the matlib library: library(matlib)
## 2) Source the file: source("cacheMatrix.R")
## 3) Create a matrix:  
##    Sample 1: myMatrix <- makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
##    Sample 2: myMatrix <- makeCacheMatrix(matrix(c(1/2,-1/4,-1,3/4),nrow=2,ncol=2))
## 4) Test the "get" function to output your matrix and to try to retrieve the inverse
##    myMatrix$get()
##    myMatrix$getInverse - should return NULL
## 5) Test the cacheSolve function:
##    cacheSolve(myMatrix) - execute a second time to show the cached version is retrieved.


## makeCacheMatrix takes in a matrix argument and create the environment
## for setting and retrieving variables that will contain the matrix and the 
## inverse matrix
makeCacheMatrix <- function(x = matrix()) {

      inv <- NULL

      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
  
      get <- function() x
  
      setInverse <- function(inverse) {inv <<- inverse}
  
      getInverse <- function() {inv}
  
      list(set = set, 
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)

}


## cacheSolve takes the passed in object and determines if it has
## already been cached.  If not, proceed with calculating the inverse and
## set it in the parent environment, as well as output the inverse to the screen.

cacheSolve <- function(x, ...) {

      ## Attempt to retrieve an inverse matrix from the input object  
      inverseMat <- x$getInverse()
  
      if(!is.null(inverseMat)) {
        message("Getting cached inverse martix data")
        return(inverseMat)
        }
      
      ## Retrieve the original matrix created by the call to makeCacheMatrix
      origMatrix <- x$get()
      
      ## Calculate the inverse of the original matrix
      inverseMat <- inv(origMatrix)
      
      ## Set the inverse matrix value in the parent environment
      x$setInverse(inverseMat)
  
      inverseMat
  
  }
