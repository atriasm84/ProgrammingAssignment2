## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix:
#         The function makeCacheMatrix encapsulates the behaviour of a matrix 
#         than can cache the value of its inversed matrix once it is calculated
#         the method returns a list with 4 methods, the setter and getter for 
#         the matrix and its inverse setter and getter respectively
# set:
#        in the setter of the matrix we set the inverseMatrix to null as it has 
#        to be recomputed
# get:  
#        returns the current value of the matrix
# setInverseMatrix:
#        set the value of the setInverseMatrix
# getInverseMatrix:
#        returns the cached value of the inverseMatrix, note that it can return
#        null it the function "cacheSolve" has not been called since the matrix 
#        has been set
makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix<-NULL
        set <- function(y){
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(inverseValue) inverseMatrix<<-inverseValue
        getInverseMatrix <- function() inverseMatrix
        list(set = set, 
                 get=get, 
                 setInverseMatrix=setInverseMatrix,
                 getInverseMatrix=getInverseMatrix)
}


## Write a short comment describing this function
# the cacheSolve should be called over an object obtained by the makeCacheMatrix
# method.
# It would obtain the inverseMatrix from the cache (getInverseMatrix)
# In case that it is null it would obtain the matrix values, compute the inverse
# and set the value of the inverse on the cache.
cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getInverseMatrix()
        if(!is.null(inverseMatrix)) {
                message(" getting cached data")
                return(inverseMatrix)
        }
        matrix<-x$get()
        inverseMatrix<-solve(matrix)
        x$setInverseMatrix(inverseMatrix)
        inverseMatrix
        ## Return a matrix that is the inverse of 'x'
}
