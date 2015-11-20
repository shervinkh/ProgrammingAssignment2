## This function makes a inverse-cached matrix object. This object 
## can be used with `cacheSolve` function to calculate its inverse.
## The inverse will be calculated only in the first time and returned
## from the stored variable next time.
## The cache will be invalidated upon change using $set function

makeCacheMatrix <- function(x = matrix()) {
    ## inv variable stores cached value. Defaults to null
    inv <- NULL
    
    ## set function is used to assign a matrix to this object
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## get function is used to get the stored matrix
    get <- function() x
    
    ## setinverse function is used to set the cached inverse value
    setinverse <- function(inverse) inv <<- inverse
    
    ## getinverse function is used to retrieve cache inverse value
    getinverse <- function() inv
    
    ## Return a list containing these functions as cached-inverse matrix object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes a cached-inverse matrix object created using
## `makeCacheMatrix` function. It either calculates the inverse of the
## matrix (if it's the first time) or returns the result from cache value

cacheSolve <- function(x, ...) {
    ## Get cached value from inside of the cached matrix object
    inv <- x$getinverse()
    
    ## Check if there's a cached value and return if exists
    if(!is.null(inv)) {
        message("Getting cached data...")
        return(inv)
    }
    
    ## If there's no cache calculate inverse and store it inside
    ## the object using `setinverse` function and return it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
