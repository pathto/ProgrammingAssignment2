## Create an object to store a matrix and cache its inverse

## This function creates an object to store a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        # initiate the object.
        set <- function(y = matrix()) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        # store the inverse.
        setInverse <- function(y) inv <<- y
        
        getInverse <- function() inv
        
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)

}


## First check if the inverse has already been calculated. If so, it get the 
## inverse from the cache. Otherwise, it calculates and store the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting the cached inverse")
                return(inv)
        }
        # The inverse is not cached. Then calculate and store the inverse.
        m <- x$get()
        inv <- solve(m)
        x$setInverse(inv)
        inv
}
