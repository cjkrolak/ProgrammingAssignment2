## Put comments here that give an overall description of what your
## functions to cache an inverse matrix using the solve() function
##
## use:
## xv <- MakeCacheMatrix(x)  # cache the inverse matrix
## xinv <- cacheSolve(xv)    # retrieve the cached value
##
## verification:
## x %*% xinv = unity matrix (after rounding)
##
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
##  Save matrix to cache for faster processing.
##  x = input matrix
##  return:
    
    ## initialize the inv matrix
    m <- NULL

    # define the set method
    set <- function(y = matrix()) {
        x <<- y
        m <<- NULL
    }
    get <- function() x  # define the get method
    setinv <- function(solve) m <<- solve  # define the setinv method
    getinv <- function() m  # define the getinv method

    # expose methods
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
## inputs:
##      x = input matrix
## returns
##      inverse matrix
    
    ## return cached matrix if possible
    m <- x$getinv()  # retrieve the inverse matrix
    if (!is.null(m)) {
        message("using cached inverse matrix")
        return(m)
    }
    ## inverse matrix
    data <- x$get()  # get input matrix
    message("calculating inverse matrix...")
    m <- solve(data, ...)  # calculate inverse
    x$setinv(m)  # cache inverse
    m  # return inverse
}
