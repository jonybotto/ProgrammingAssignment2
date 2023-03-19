## Matrix inversion can be a costly computation. Rather than computing it 
## repeatedly, there may be some benefit to caching the result of such operation.
## Hence, taking advantage of the scoping rules of the R language, this caching
## process can be made using two functions that create a special object which
## stores a matrix and caches its inverse.

## This first function, makeCacheMatrix, creates a special "matrix", which is in
## fact a list of functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix's inverse
## 4. get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set.inverse <- function(inverse) inv <<- inverse
    get.inverse <- function() inv
    list(set = set, get = get, set.inverse = set.inverse, get.inverse = get.inverse)
}


## The second function, cacheSolve, will print an inverse of the matrix passed
## in as the argument. If there is an already cached inverse (and the matrix has
## not changed), it will print that result. However, if we're inputting a new
## matrix as the argument, makeCacheMatrix will set the cached inverse to NULL,
## and cacheSolve will not only compute the inverse of this new matrix (printing
## it to the console), but also use the set.inverse function to cache this new
## inverse.

cacheSolve <- function(x, ...) {
    inv <- x$get.inverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set.inverse(inv)
    inv
}
