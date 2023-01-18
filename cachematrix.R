

# Together, these two functions create a special object that stores a 
# matrix and caches its inverse, the calculation of which is usually 
# quite costly.



# The first function, `makeCacheMatrix` creates list containing functions to
#   1.  set the elements of the matrix
#   2.  get the elements of the matrix
#   3.  set the elements of the inverse
#   4.  get the elements of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



# The following function inverts the "matrix" created above. It first 
# checks to see if the inverse has already been calculated, in which 
# case it `get`s the mean from the cache and skips the computation. 
# Otherwise, it inverts the matrix and sets the value of the inverse
# in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
