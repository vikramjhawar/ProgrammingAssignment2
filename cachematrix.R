#For complex and cost-intensive calculations, caching in R can be used to store and fetch results. 
#In this assignment, we will create 2 functions for computing the inverse of a matrix and cache the result 
#to be used later.

# 1st Function: To create a special "matrix" object which can cache its inverse

makeCacheMatrix <- function(a= matrix()) {
  
  # Initialize objects 
  
  InvertedMatrix <- NULL
  # Set the matrix   
  setMatrix <- function(b) {
    
    a <<- b
    InvertedMatrix <<- NULL
  }
  # Retrieve the matrix
  
  getMatrix <- function() a
  
  # Set the inverted matrix
  
  setInvertedMatrix <- function(Invert) InvertedMatrix <<- Invert
  
  # Retrieve the inverted matrix  
  
  getInvertedMatrix <- function() InvertedMatrix
  
  # create a new object by returning the list
  
  list(setMatrix= setMatrix,
       getMatrix= getMatrix,
       setInvertedMatrix= setInvertedMatrix,
       getInvertedMatrix= getInvertedMatrix)               
}

# 2nd function:This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(a,...) {
  
  # Return matrix that is inverse of a
  
  InvertedMatrix <- a$getInvertedMatrix()
  
  #Check whether cache is available and return it if available
  
  if(!is.null(InvertedMatrix)) {
    message("fetching cached data")
    return (InvertedMatrix)
  }
  # Compute inverse if cache is not available       
  else {
    
    M <- a$getMatrix()
    InvertedMatrix<- solve(M,...)
    a$setInvertedMatrix(InvertedMatrix)
    InvertedMatrix
  }
}
