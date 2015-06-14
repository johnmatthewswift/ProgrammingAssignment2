## The functions contained in this file allow caching of the inverse of a matrix
## to save computational resource.

## makeCacheMatrix creates a list of 4 functions which:
## 1. set - sets the matrix;
## 2. get - gets the matrix;
## 3. setinv - calculates and sets the matrix inverse;
## 4. getinv - returns the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    invmat <- NULL
    
    set <- function(y) {
        x <<- y
        invmat <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(solve) invmat <<- solve
    
    getinv <- function() invmat
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## cacheSolve returns the inverse matrix of x. 
## If the inverse has already been calculated and cached, it returns this matrix
## without calculation; 
## if not, it caclulates and caches the matrix for future usage before returning
## it.

cacheSolve <- function(x, ...) {

    invmat <- x$getinv()
    
    if(!is.null(invmat)) {
        message("getting cached data")
        return(invmat)
    }
    
    mattoinv <- x$get()
    
    invmat <- solve(mattoinv, ...)
    
    x$setinv(invmat)
    
    invmat
    
}
