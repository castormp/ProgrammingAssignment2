## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object
## that can cache the inverse of a matrix avoiding recomputation 
## of the inverse if the matrix has not changed

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #inverse value
    
    #helper function to initialize the matrix and restart its inverse 
    set <- function(y) {
     #   if (!identical(x,y)){
            x <<- y
            i <<- NULL
      #  }
    }
    
    #helper function to get the matrix
    get <- function() x
    
    #helper function to set the inverse of the matrix
    setinverse <- function (inv) i <<- inv
    
    #helper function to get the inverse of the matrix
    getinverse <- function () i
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    if (!is.null(i)) {
        message("getting cached data")
        return(i)        
    }
    
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
