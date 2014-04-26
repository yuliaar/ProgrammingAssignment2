## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object 
## that can cache its inverse. Use set() function with a created object
## to actually assign data to it. 
## For example: mat <- makeCacheMatrix(); mat$set(matrix(c(2,0,0,2),2,2))

makeCacheMatrix <- function(x = matrix()) {         
         inv <- NULL
         set <- function(y) {
                 x <<- y
                 inv <<- NULL
         }
         get <- function() x
         setinverse <- function(inverse) inv <<- inverse
         getinverse <- function() inv
         list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached date")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv 
}
