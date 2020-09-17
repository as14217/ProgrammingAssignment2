## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        
        # The set line defines a function to set the matrix, X, to a new matrix, Y, and 
        # resets the inverse, inv, to NULL.
        set <- function(Y) {
                X <<- Y
                inv <<- NULL
        }
        
        get <- function() X # returns the matrix, X 
        
        setinverse <- function(inverse) inv <<- inverse  # sets the inverse of the matrix, inv, to inverse
        
        getinverse <- function() inv # returns the inverse, inv
        
        list(set = set,       
             get = get,         
             setinverse = setinverse,  
             getinverse = getinverse)  
        
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache. For this assignment, we're assuming that the matrix supplied is always invertible.

cacheSolve <- function(X, ...) {
        ## This function returns a matrix that is the inverse of 'X'
        
        # It first checks the cache memory 
        inv <- X$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # If the inverse of this matrix hasn't already been solved, then it will do so now, and then save it back into the object X.
        data <- X$get()
        inv <- solve(data, ...)
        X$setinverse(inv)
        inv
}
