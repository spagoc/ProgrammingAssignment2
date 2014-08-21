## Calculate inverse matrix of an input matrix caching the result. 
## Two functions are used: makeCacheMatrix and cacheSolve.
## 
## Example usage:
##      > mymatrix<-matrix(1:4,2,2)
##      > b<-makeCacheMatrix(mymatrix)
##      > cacheSolve(b)
##      > cacheSolve(b)
##      > mymatrix %*% cacheSolve(b)
## Other usage:
##      > b<-makeCacheMatrix(matrix(1:4,2,2))
##      > b$get()
## If the input matrix is not invertible this message is given:
##      "Error in solve.default(data, ...)"



## The following function makeCacheMatrix()
## creates a special "matrix", which is really a list containing functions to
## set the value of the matrix, get the value of the matrix, set the value of 
## the inverse matrix, get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function cacheSolve() calculates the inverse matrix of the special 
## "matrix" created with the above function. However, it first checks to see if 
## the inverse matrix has already been calculated. If so, the function get it from the cache 
## and skips the computation. Otherwise, the function calculates the inverse matrix and sets 
## its value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
