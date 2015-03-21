## These functions help in faster retrieval of matrix inverse by
## caching the results so that the inverse computation doesn't need to done
## repeatedly in case the matrix is unchanging.

## This function returns a list of functions to get the matrix, set the matrix,
## get the inverse of matrix and set the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL

    set <- function(y)  {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) i <<- inverse
    
    getinverse <- function() i
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of the matrix contained in the list generated
## by makeCacheMatrix. It checks if the inverse is already present in the cache
## and returns that if it exists, else it calculates the inverse, stores that in
## cache, and returns the result.

cacheSolve <- function(x, ...) {
      
    i <- x$getinverse()
      
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }  
    
    data <- x$get()
    
    i <- solve(data, ...)
    
    x$setinverse(i)
    
    i
}
