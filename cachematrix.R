## makeCacheMatrix function sets up the environment to store input matrix "x" along with it's inverse "x_inv"
## This function returns list of functions that can be subset and accessed by follow-on function, cacheSolve

makeCacheMatrix <- function(x = matrix()) {

        x_inv <- NULL
        
        # Function to set input matrix and null out any inverse
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        
        # Function to return input matrix
        get <- function() x
        
        # Function to set matrix inverse to function environment value
        setinv <- function(inv) x_inv <<- inv
        
        # Function to return matrix inverse
        getinv <- function() x_inv
        
        # A list of the functions that can be subset
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve is expected to work together with makeCacheMatrix
## cacheSolve will return the inverse matrix of "x".  First it will check to see if inverse is present in makeCacheMatrix env
## If inverse matrix to "x" is not available from previous calculation, it will compute and store in cache for later usage.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

		# Retrieve cached matrix inverse
        x_inv <- x$getinv()
        
        # If matrix inverse is present, then return cached version.  Otherwise, calculate matrix inverse.
        if(!is.null(x_inv)) {
                message("getting cached data")
                
                # Return matrix inverse (from cache)
                return(x_inv)
        }
        # Get input matrix from makeCacheMatrix function
        data <- x$get()
        
        # Calculate matrix inverse
        x_inv <- solve(data, ...)
        
        # Set matrix inverse to makeCacheMatrix function
        x$setinv(x_inv)
        
        # Return matrix inverse
        x_inv		
}
