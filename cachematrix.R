## makeCacheMatrix and cacheSolve functions are used to:
## (1) Calculate the inverse of a matrix
## (2) Store the inverse of the matrix in a cache and
## (3) Return the inverse from cache if it exists

# Call function makeCacheMatrix() to generate a list of four functions
# namely, setmat(), getmat(), setinv() and getinv()
# Example: x <- makeCacheMatrix()
#
# Call function setmat() to set the matix value
# Pass a matrix as an argument to the "setmat()" function
# Example1: x$setmat(matrix(c(1,2,3,4), 2, 2))
# Example2: mat <- matrix(c(9,8,7,6,5,4,3,2,1), 3, 3)
#           x$setmat(mat)
#
# Call funcion getmat() to display the matrix
# Example: x$getmat()
#
# Call function setinv() to cache the inverse of the matrix
# Pass the inverse of the matrix as an argument to setinv()
# Example: inv <- solve(mat)
#          x$setinv(inv)  
#
# Call function getinv() to display the inverse of the matrix
# Example: x$getinv()
#
makeCacheMatrix <- function(x = matrix(numeric())) {   
  maticache <- NULL 
  setmat <- function(y) { 
    x <<- y
    maticache <<- NULL
  }
  getmat <- function() x
  setinv <- function(inv) maticache <<- inv
  getinv <- function() maticache
  list(setmat = setmat, 
       getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}

# Call function cachesolve() to return the inverse of a matrix
# It returns the inverse from the cache if the inverse is already cached 
# If not: (1) it calculates the inverse, 
#         (2) calls the setinv() function to cache the inverse
#         (3) returns the inverse
#
# cachesolve() function is used in conjunction with makeCacheMatrix() function
# Example:
#         Step1:  
#         x <- makeCacheMatrix() ## Generates a list of four functions namely,
#                                ## setmat(), getmat(), setinv() and getinv()
#                                ## and assigns the list to x
#         Step2:  
#         x$setmat(matrix(c(1,2,3,4), 2, 2)) ## Sets the value of the matrix whose
#                                            ## inverse has to be calculated.
#         Step3:
#         cacheSolve(x)      ## cacheSolve is called with argument x.  
#                            ## x is a list with four functions generated in Step1 
#
cacheSolve <- function(x, ...) {
  maticache <- x$getinv()
  if(!is.null(maticache)) {
    message("getting cached data")
    return(maticache)
  }
  mat <- x$getmat()
  maticache <- solve(mat, ...)
  x$setinv(maticache)
  maticache
}
