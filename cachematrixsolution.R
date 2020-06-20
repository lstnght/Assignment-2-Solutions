## Matrix Inversion Caching
## The following function presents a series of assigned values & logical expressions
## which create a special "matrix" object that can cache its inverse.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(a) {
               x <<- a
               i <<- NULL
        }

        get <- function() x
          setInverse <- function(inverse) i <<- inverse
          getInverse <- function() i
          list(set = set, get = get,
               setInverse = setInverse,
               getInverse = getInverse)
}
## This next function solves the inverse of the "matrix" created by MakeCacheMatrix.
## The expression allows the inverse to be retrieved from the cache if the inverse has been
##calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       i <- x$getInverse()
        if (!is.null(i)) {
                message("retreiving cached data")
                return(i)
        }
                clone <- x$get()
                i <- solve(clone,...)
                x$setInverse(i)
                i
        }

##TESTING SOLUTIONS
##REMEMBER TO CLEAR VARIABLES FROM PREVIOUS SESSIONS BEFORE
#inputting These Solutions!
##  source("~/GitHub/ProgrammingAssignment2/cachematrixsolution.R")
# file path will vary based on file location.
## The following are examples of testing commands used

cloned_matrix <- makeCacheMatrix(matrix(4, 4, 2, 8, 4 ,4))
cloned_matrix$get()
cacheSolve(cloned_matrix)
cloned_matrix$getInverse()
