## The first function input a matrix and and a placeholder which can be filled with the inverse. 
## The second function computes the inverse if the placeholder is Null, otherwise does nothing.

makeCacheMatrix <- function(x = matrix()) {
        ##defines a matrix and an initial value of Null for the inverse that can be later added.
        ##The inverse can then be added. Also if the matrix is changed the inverse resets to Null.
        inv <- NULL
        set <- function(y) {   
                x <<- y   
                inv <<- NULL     
        }
        get <- function() x       
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Returns the inverse of the input matrix, 
        ## however only computes it if 
        ## the inverse is not already saved in cache.
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        
}
