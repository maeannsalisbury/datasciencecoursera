## The 'makeCacheMatrix' function and the 'cacheSolve' function work together to 
## compute the inverse of a square matrix.  The 'makeCacheMatrix' takes a square
## matrix as an input, stores the matrix inversion value in its local variables, 
## also creates a list of functions that 'cacheSolve' can call.  The 'cacheSolve 
## function then uses that list of functions to compute the inversion of the
## square matrix.  The purpose of these functions is to provide a quick way
## to report the matrix inversion if it has already been computed.

## The 'makeCacheMatrix' creates a list of functions using a square matrix
## as an input argument.  The list of functions created will set the matrix
## input to a local variable, retrieve the matrix input, set matrix inversion
## to a global variable, or get the value representing the matrix inversion
## stored in the global variable.


makeCacheMatrix <- function(x = matrix()) {
        matrixInv <- NULL
        setMatrix <- function(AltMatrix) {
                x <<- AltMatrix
                matrixInv <<- NULL
        }
        getMatrix <- function() x
        setMatrixInv <- function(solve) matrixInv <<- solve
        getMatrixInv <- function() matrixInv
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setMatrixInv = setMatrixInv,
             getMatrixInv = getMatrixInv)       
}


## The 'cacheSolve' function takes the object of 'makeCacheMatrix' to compute
## the inversion of a square matrix.  If the matrix inversion has already been 
## computed, then it retrieves it from cache -- which is stored in the variable
## from 'makeCacheMatrix'.

 cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         matrixInv <- x$getMatrixInv()
         if(!is.null(matrixInv))  {
                 message("getting cached data")
                 return(matrixInv)
         }
         data <- x$getMatrix()
         matrixInv <- solve(data, ...)
         x$setMatrixInv(matrixInv)
         matrixInv
}
