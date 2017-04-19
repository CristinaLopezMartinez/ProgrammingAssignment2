## Functions to compute the inverse of a matrix with caching. 

## Caching function for a matrix argument.  Returns a vector of getters and setters for the matrix itself, as well as 
## placeholders for the matrix inverse calculation.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Given a makeCacheMatrix object, returns the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...) 
        x$setinverse(m)
        m
}
