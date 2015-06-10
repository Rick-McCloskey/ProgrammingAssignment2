################################################################################
# Date: 06/10/2015
#
# Author:       Richard McCloskey
# Class:        rprog-15
# Assignment:   Caching the Inverse of a Matrix - Week 2 Homework
#
# Assignment description and requirements at: 
#       https://github.com/Rick-McCloskey/ProgrammingAssignment2
#
################################################################################

# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly. 
# The following functions cache the inverse of a matrix.


# makeCacheMatrix: This function creates a special "matrix" 
# object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        # Set "inverse" to a value of NULL to start
        inverse <- NULL 
        
        # set" the value of the matrix
        set = function(y) {
                # <<- is used to assign a value to an object 
                # in an environment that is different from the 
                # current environment. In this case y is assigned to x.
                x <<- y 
                
                # "inverse" is assigned a value of NULL
                inverse <<- NULL 
        }

        # "get" the value of the matrix
        get = function() x
        
        # "setinverse" to the inverse value of the matrix
        setinverse = function(inv) inverse <<- inv
        
        # "getinverse" value of the matrix
        getinverse = function() inverse
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
                
}

# cacheSolve: This function computes the inverse of the special 
# "matrix" returned by makeCacheMatrix above. If the inverse has 
# already been calculated (and the matrix has not changed), 
# then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Assign "getinverse" from x to "inverse"
        inverse <- x$getinverse()
        
        # Check to see in inverse is not NULL
        # if it is not NULL then return the cached value
        if(!is.null(inverse)) {
                message("getting cached data.")
                
                #return the cached data in "inverse" for output.
                return(inverse)
        }
        
        # If the "inverse" is NULL do the following steps:
        
        # Retrieve the value x and assign it to "data"
        data <- x$get()
        
        # solve for the inverse of the matrix and store it in "inverse".
        inverse <- solve(data)
        
        # Cache the value of "inverse" in "setinverse" in x
        x$setinverse(inverse)
        
        #print the contents of "inverse" to the console.
        inverse
}
