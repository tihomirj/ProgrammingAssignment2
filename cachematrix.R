# Matrix inversion is time-consuming and it is better to be cached when possible. 
# When inversion is done for the first time it is computed and then stored in cache. 
# When the inverse matrix is needed again it is retrieved from the cache.
# The functions below create a matrix which inverse is stored in cache for future retrieval.

makeCacheMatrix <- function(x = matrix()) {
# makeCacheMatrix() is a function that creates a list which elements are 
# four other functions:
# 1. the function set() that sets the value of the matrix
# 2. the function get() that gets the value of the matrix
# 3. the function setinverse() that sets the inverse of the matrix
# 4. the function getinverse() that gets the inverse of the matrix
    
    cache <- NULL               # the cache, in the beginning it is NULL
   
    set <- function(y) {        # sets the matrix
        x <<- y
        cache <<- NULL
    }
    
    get <- function() {         # returns the matrix
        x 
    }
    setinverse <- function(inverse) { # puts the inverse into the cache 
        cache <<- inverse 
    } 
    
    getinverse <- function() {  # retrieves the inverse from the cache
        cache
    }
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
# cacheSolve() returns the inverse of the matrix. 
# If the inverse has already been computed the inverse matrix is retrieved from 
# cache and the computation is skipped. 
# If the inverse has not been computed yet, the function makes the inverse and 
# places it in the cache via the setinverse() function.   
        
    cache <- x$getinverse()
   
    if(!is.null(cache)) {       # check if the inverse has already been computed 
        message("getting cached data.")
        return(cache)
    }
                                 
    data <- x$get() 
    
    cache <- solve(data)        # computes the inverse
    
    x$setinverse(cache)         # stores the inverse in the cache
    
    cache
}
