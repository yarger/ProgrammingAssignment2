## CACHE MATRIX ASSIGNMENT
## The following two functions allow to use  cache 
## while calculating the inverse of a matrix.
## I -- makeCacheMatrix() II -- cacheSolve()

##---------------------------------------------------------------------------
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv_matrix <- NULL ## cached inverse matrix - default value
        
        set <- function(y) {
                x <<- y ## setmatrix value
                inv_matrix <<- NULL
        }
        
        get <- function() x ## return matrix value
        
        setinv <- function(solve) inv_matrix <<- solve ## inverse the given matrix
        
        getinv <- function() inv_matrix ## get solved/cached matrix inverse value  
        
        ## Return the list of functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

##---------------------------------------------------------------------------
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv_x <- x$getinv()
        if(!is.null(inv_x)) {
                message("getting cached precalculated inverse matrix")
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setinv(inv_x)
        ## Return a matrix that is the inverse of 'x'
        inv_x
}
