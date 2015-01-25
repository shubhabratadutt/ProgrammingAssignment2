## makeCacheMatrix function is used to build a special "matrix" object 
## that provides functons to set and get the values of the invertibe 
## matrix and its inverse.
## The cacheResolve function gets the inverse 
## matrix cached in the speciaal "martix" object. If no cached 
## inverse is available, this function will calculate the 
## inverse and save it in the cache in the special "matrix" object. 
## Assumptions are:
## 1. The matrix is invertible
## 1. The matrix has not changed since its inverse
##    was calculated and cached

## The following function creates and returns a special "matrix" 
## object that provides the following accessor functions to 
## manipulate the matrix and its inverse 
## 1. setMatrix - to store the matrix to be inverted
## 2. getMatrix - to get the stored matrix
## 3. setInverse - to set the value of the inverse cache
## 4. getInverse - to get the value of the inverse cache
## The object returned is actually a list

makeCacheMatrix <- function(x = matrix()) {
  
  ##Placeholder for storing the matrix inverse
  inverseMatrix <- NULL
  
  ##The following function sets the value of the matrix
  set <- function(matrix){
    x <<- matrix
    inverseMatrix <<- NULL
  }
  
  ##The following function gets the value of the matrix
  get <- function() x
  
  ##The following function sets the value of the inverse cache
  setInverse <- function(inverse) inverseMatrix <<- inverse
  
  ##The following function gets the value of the inverse cache
  getInverse <- function() inverseMatrix
  
  ## Return the special "matrix" object which is
  ## a list.
  list(setMatrix = set, getMatrix = get, setInverse = setInverse, getInverse = getInverse)
}

  

## The following function returns the inverse of the special
## matrix created using the "makeCacheMatrix" function above.
## It first checks if the inverse cache is already populated
## and returns the cache if true. Otherwise the function proceeds
## to calculate the inverse, sets the inverse cache and then 
## returns the inverse of thee matrix

cacheSolve <- function(x, ...) {
  
  ##Get the value of the cached inverse
  inverse <- x$getInverse()
  
  ##Check if the cached inverse holds any value and 
  ##return the same if true
  if(!is.null(inverse)) {
    message("Returning cached inverse")
    return (inverse)
  }
  
  ##The following code executes if the cached 
  ##inverse is null.
  
  ##Get the matrix that needs to be inverted
  matrix <- x$getMatrix()
  
  ##Calculate the inverse
  inverse <- solve(matrix)
  
  ##Set the value of the cache with the 
  ##newly calculated inverse
  x$setInverse(inverse, ...)
  
  ##Return the inverse of matrix x
  inverse  
}
