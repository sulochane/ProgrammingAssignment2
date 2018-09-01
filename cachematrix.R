## These functions creates a special "matrix" object that can store and return the
## inverse of a matrix.


## creates a special matrix that retains in the chache with it's inverse
makeCacheMatrix <- function(x = matrix()) {
    message("set special matrix object")
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## returns the inverse of a matrix, either from cache if 
## it is available in the cache, otherwise, compute the inverse of a 
## matrix and returns 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    message("getting the inverse of a matrix")
    i <- x$getinverse()
    if (!is.null(i)) {
        message ("getting cached data")
        return (i)
    }
    # inverse is not available in the cache, hence computed and stroed in the cache
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}


## Testing code
## ------------------------------------
# x = matrix(c(1,3,4,
#              5,3,2,
#              7,2,1), nrow=3, ncol=3)
# 
# x
# 
# z <- makeCacheMatrix(x)
# 
# x.iv <- cacheSolve(z)
# x.iv
# x.iv <- cacheSolve(z)
# 
# y = x %*% x.iv
# y



