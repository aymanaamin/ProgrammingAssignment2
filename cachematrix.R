## In this small project we have a pair of functions: makeCacheMatrix and cacheSolve 
## that cache the inverse of a square invertible matrix, rather than computing it repeatedly.
## Note: the functions assume the matrix is inverible square.

library(matrixcalc) # required library to check whether the matrix is square.

## makeCacheMatrix: This function creates a special  -inverible square- "matrix" object
# that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      Inv <- NULL
      
      setMatrix <- function(y){
            x <<- y
            Inv <<- NULL
      }
      getMatrix <- function() x 
      
      setMatrixInv <- function(inverse) Inv <<- inverse
      getMatrixInv <- function() Inv 
      list=c(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setMatrixInv = setMatrixInv,
             getMatrixInv = getMatrixInv)
}


## cacheSolve: This function computes the inverse of the special -inverible square- "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then 
# cacheSolve should retrieve the inverse from the cache, 
# and if the matrix is singular or non-square, then 
# the inverse is not calculated and cacheSolve return warning message.

cacheSolve <- function(x, ...) {
      Inv <- x$getMatrixInv()
      
      if (!is.null(Inv)){
            message("getting cached Matrix Inverse")
            return(Inv)
      }
      mat <- x$getMatrix()
      if (is.square.matrix(mat)==T && det(mat) != 0 ) {
            Inv <- solve(mat)
            x$setMatrixInv(Inv)
            return(Inv)} else { print("Warning: Your matrix is singular or non-square!") }
}


#=== examples
# 1. inverible square matrix:
x = makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(x)

# 2. singlar square matrix:
x = makeCacheMatrix(matrix(c(1,1,1,1),2,2))
cacheSolve(x)

# 3. Non-square matrix:
x = makeCacheMatrix(matrix(1:6,2,3))
cacheSolve(x)

