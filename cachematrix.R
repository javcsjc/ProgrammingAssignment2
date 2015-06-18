## Programming Assignment 2: Lexical Scoping
## Data Science Specialization Track
## Name: Jose Alberto Valdez Crespo
## Date: June 18, 2015

## makeCacheMatrix is a function that creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Variable "cache" is used to store the cache value
        ## Needs to be initialized to NULL
        cache <- NULL
        ## Creating the matrix
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        ## Getting the value of the matrix        
        get <- function()x
        ## Here we are inverting the matrix and storing the value on the 
        ## variable cache
        setinv <- function(solve) cache <<- solve
        getinv <- function () cache              
        ## Here we are returning the created functions     
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## cacheSolve is a function that computes the inverse of the special "matrix"
## returned by makeCacheMatrix function above. If the inverse of the matrix
## has already been calculated (and the matrix has not changed!), then the 
## cachesolve function should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
        ## Return the inverted matrix from cache if it exists
        cache <- x$getinv()
        ## Checking cache is not empty. If it is not empty, then we are pulling
        ## the cache data
        if (!is.null(cache)){
                message("Pulling data from cache")
                ## returning the inverse matrix from cache
                return(cache)
        }
        ## Create matrix since it doesn't exist yet
        matrix <- x$get()
        cache <- solve(matrix, ...)
        x$setinv(cache)
        ## Display matrix in console
        return(cache)   
}
