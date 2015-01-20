## These two functions are for Assignment 2
## There are two functions: "makeCacheMatrix" and "cacheSolve".
## Method descriptions are in comments above each function.

## Returns an object that contains the matrix passed into 'x' with the addition of a few additional
## functions to retrieve and set data in the object.
makeCacheMatrix <- function(x = matrix()) {
    
    # A bit of error checking to make sure that a matrix is infact being passed in.
    if(class(x) != "matrix") stop("'x' Must be of class 'matrix'.")
        
    cachedInverse <- NULL
    
    setMatrix <- function(y){
        if(class(y) != "matrix") stop("'y' Must be of class 'matrix'.")
        x <<- y
        # Our matrix changes within this function. Our old cached value is no good, so reset back to NULL
        cachedInverse <<- NULL
    }
    
    getMatrix <- function(){
        return(x)
    }
    
    setInverse <-function(matrixInverse){
        # The only thing that we check is to make sure that a valid matrix is massed in.
        # One thing that is uncertain is if the matrix is the CORRECT inverse of 
        # this current object's matrix.  
        if(class(matrixInverse) != "matrix") stop("'matrixInverse' Must be of class 'matrix'.")
        cachedInverse <<- matrixInverse
    }
    
    getInverse <- function(){
        return(cachedInverse)
    }
    
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve function should really be implemented within the "makeCacheMatrix" function. But 
## because this stuf R file implements it externally of that function, I can only assume that this 
## is by design of the instructor. 

## The "cacheSolve" calculates the matrix inverse of the object passed in.
cacheSolve <- function(x, ...) {
    # returns matrix inverse if it exists in cache
    if(!is.null(x$getInverse())) {
        print("Retrieving from cache.")
        return(x$getInverse())
    }
    
    # Calculate and set the new inverse. This will auto cache the new inverse
    x$setInverse(solve(x$getMatrix()))
    # Return a matrix that is the inverse of 'x'
    return(x$getInverse())
}
