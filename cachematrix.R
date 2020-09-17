## The 2 following function are to calculate and cache
## the inverse matrix of an input matrix

## The function 'makeCacheMatrix' creates a list of functions
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse and store it in var 'i' in the parent env.
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## The function 'makeCacheMatrix' takes x (result of the 1st function)
## as argument and returns the inverse matrix if it's already cached in 'x'
## otherwise calculates the inverse matrix using 'solve' function 
## store/cache it in 'x' & returns it

cacheSolve <- function(x, ...) {
    ## Retrieve the inverse matrix if already cached
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## Else, retrieve the original input matrix and compute the inverse
    data <- x$get()
    i <- solve(data, ...)
    ## Save the newly calculated inverse in cache
    x$setinverse(i)
    ## Return the inverse matrix
    i
}
