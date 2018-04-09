## This function creates a special "matrix", which is really a list containing a function to:
## set the value of the matrix, get the value of the matrix, set the value of the inverse of
## the matrix and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setmat <- function(y){
                x <<- y
                inv <<- NULL
        }
        getmat <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(setmat = setmat, getmat = getmat, setinverse = setinverse, getinverse = getinverse)
        
}


## This function returns a matrix that is the inverse of 'x' after checking whether
## the inverse of 'x' is alreaddy calculated and cached

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$getmat()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
