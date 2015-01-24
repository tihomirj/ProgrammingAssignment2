# Matrix inversion is time-consuming. When the inverse is needed several times
# and it does not change, it is good to be cached, so that it can be retrieved when 
# needed.
# When inversion is done for the first time it is computed and then stored in cache. 
# When the inverse matrix is needed again it is retrieved from the cache.
# The functions below create a matrix which inverse is stored in cache for future retrieval.

makeCacheMatrix <- function(x = matrix()) {
# makeCacheMatrix() returns a list which elements are 
# four functions:
# 1. set() - sets the value of the matrix
# 2. get() - gets the value of the matrix
# 3. setinverse() - sets the inverse of the matrix
# 4. getinverse() - gets the inverse of the matrix
    
    cache <- NULL               # the cache, in the beginning is NULL
   
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
# If the inverse has already been computed, the inverse matrix is retrieved from 
# the cache and the computation is skipped. 
# If the inverse has not been computed yet, the function does the inverse and 
# places it in the cache via the setinverse() function.   
        
    cache <- x$getinverse()     # reads the cache
   
    if(!is.null(cache)) {       # checks if the inverse has already been computed if yes 
        message("Retrieving cached data.")
        return(cache)           # returns the stored inverse from the cache
    }
                                 
    data <- x$get()             # gets the matrix
    
    cache <- solve(data)        # computes the inverse
    
    x$setinverse(cache)         # stores the inverse in the cache
    
    cache                       # returns the inverse
}
