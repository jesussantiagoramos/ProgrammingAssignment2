# This pair of functions allows to compute and cache the inverse of a matrix. Thus,
# if needed again, the inverse can be retrieved from the cache rather than recomputed.

# The first function, makeCacheMatrix, takes a matrix as an argument
# and returns a special "matrix" object that can cache the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {                   # Sets the content of the matrix.
                x <<- y
                i <<- NULL
        }
        get <- function() x                    # Gets the content of the matrix.
        setinverse <- function(inv) i <<- inv  # Sets the content of the matrix inverse.
        getinverse <- function() i             # Gets the content of the matrix inverse.
        list(set = set, get = get,             # Returns a list containing the four
                setinverse = setinverse,       # previous functions.
                getinverse = getinverse)
}


# The second function, cacheSolve, takes the special "matrix" object returned by 
# makeCacheMatrix as an argument and computes the matrix inverse. If the inverse 
# has already been calculated (and the matrix has not changed), then cacheSolve 
# retrieves it from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()                    # Query the cache of x.
        if(!is.null(i)) {                      # If the inverse of the matrix has already
                message("getting cache data")  # been calculated, the function retrieves it
                return(i)                      # from the cache, and a message is printed.
        }
        data <- x$get()                        # Otherwise, the inverse
        i <- solve(data, ...)                  # of the matrix is calculated
        x$setinverse(i)                        # and saved to the cache.
        return(i)                              
}
