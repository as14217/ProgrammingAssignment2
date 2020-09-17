## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        
        # Line 14 defines a function to set the matrix, X, to a new matrix, Y, and 
        # resets the inverse, inv, to NULL.
        set <- function(Y) {
                X <<- Y
                inv <<- NULL
        }
        
        get <- function() X # returns the matrix, x 
        
        setinverse <- function(inverse) inv <<- inverse  # sets the mean, m, to mean
        
        getinverse <- function() inv # returns the mean, m
        
        list(set = set,       
             get = get,         
             setinverse = setinverse,  
             getinverse = getinverse)  
        
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache. For this assignment, we're assuming that the matrix supplied is always invertible.

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'X'
        
        inv <- X$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- X$get()
        inv <- solve(data, ...)
        X$setinverse(inv)
        inv
}
