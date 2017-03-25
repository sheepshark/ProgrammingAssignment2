## The following functions will save a matrix in cache memory, then solve for
## the inverse of the matrix and save the inverse in cache memory.

## By assigning the return of the makeCacheMatrix function to a variable, the
## function will save a matrix (x), an empty variable (m) for the inversion of
## the saved matrix, and a list of four functions to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        ## Clear the value of "m", assign "m" a null value.
        m <- NULL
        ## Create function "set" that will assign argument matrix "x" to parent
        ## evironment. Assign "m" null value.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## Create function "get" that will access argument matrix "x".
        get <- function() x
        ## Create function "setinvrs" that will assign inverse matrix to
        ## variable "m".
        setinvrs <- function(invrs) m <<- invrs
        ## Create function "getinvrs" that will access variable "m".
        getinvrs <- function() m
        ## makeCacheMatrix returns list of functions.
        list(set = set, 
             get = get,
             setinvrs = setinvrs,
             getinvrs = getinvrs)
}


## The cacheSolve function will calculate the inverse of the saved matrix, 
## save the inverse of the saved matrix using the "setinvrs" function defined
## in the makeCacheMatrix function, and finally, return the inversion of the
## matrix. Subsequent executions of the function will return the matrix
## inversion saved in cache memory using the "getinvrs" function, and will
## also return a message indicating the function is "getting cached data."

cacheSolve <- function(x, ...) {
        ## Use saved function "getinvrs" to access variable "m", matrix
        ## inversion or null.
        m <- x$getinvrs()
        ## If "m" is not null, print message indicating function will return
        ## cached data, and return value saved in variable "m".
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Use saved function "get" to access matrix saved in variable "x".
        data <- x$get()
        ## Calculate inversion of saved matrix, and assign to variable "m".
        m <- solve(data, ...)
        ## Use saved function "setinvrs" to assign matrix inversion to 
        ## variable "m"
        x$setinvrs(m)
        ## Return a matrix that is the inverse of 'x'
        m
}