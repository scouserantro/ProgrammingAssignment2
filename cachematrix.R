## This script file contains two functions:
## 1. makeCacheMatrix: this creates an object with four methods, and can be used
## to store the inverse of a matrix in memory. It assumes that the matrix is
## always inversible.
## 2. cacheSolve: used to return the inverse if a matrix. If this operation has
## been performed before, it returns the inverse result from memory else
## it performs the inverse and stores it in the object

## makeCacheMatrix
## Objective: create an object that allows us to store the inverse of a matrix in memory
## Contains four methods:
## get: simply returns the original matrix
## set: redefines the object with a new matrix, and sets flag to NULL
## setInvMatrix: sets the inverse matrix in the object
## getInvMatrix: returns the inverse matrix if it exists

makeCacheMatrix <- function ( matrixIn = matrix() ) {
  InvMatrix = NULL
  ## define setting nex matrix in object method
  set <- function (IncomingMatrix) {
    matrixIn <<- IncomingMatrix
    InvMatrix <<- NULL    
  }
  ## define the get method for this object
  getOrgMatrix <- function() { matrixIn }
  
  ## define the set inverse matrix method
  setInverseMatrix <- function ( matrixIn ) { InvMatrix <<- matrixIn }
  
  ## define the get inverse of matrix method
  getInverseMatrix <- function() { InvMatrix }

  ## define method calls
  list ( set = set, get = getOrgMatrix, setInvMatrix = setInverseMatrix, getInvMatrix = getInverseMatrix )
}

## cachesolve is the function to access the objects methods
cacheSolve <- function ( mMatrix ) {
  
  SolvedMatrix <- mMatrix$getInvMatrix()
  ## test to see if we found matrix in object
  if (is.null(SolvedMatrix)) {
    ## now matrix found, so create entry, inverse matrix and 'save' it in the object
    print("Setting new inverse matrix")
    data <- mMatrix$get()
    ## create inverse of the matrix
    SolvedMatrix <- solve( data )
    ## set inverse matrix in object
    mMatrix$setInvMatrix(SolvedMatrix)
  } else  { ## we found the matrix so don't need to do anything here
    print("We have a cached value, so we return it...")
  }
  SolvedMatrix
}
