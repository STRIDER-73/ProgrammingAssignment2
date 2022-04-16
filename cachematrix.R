## The cachematrix.R file contains two functions, 
## makeCacheMatrix() and cachesolve(). 

## The first function in the file, makeCacheMatrix() creates an R object 
## that stores a matrix and its inverse. 

## The second function, cacheSolve() requires an argument 
## that is returned by makeCacheMatrix() in order to retrieve the inverse 
## from the cached value that is stored in the 
## makeCacheMatrix() object's environment.



## makeCacheMatrix() builds a set of functions 
## and returns the functions within a list to the parent environment. 
## That is, " aMatrix <- makeCacheMatrix(matrix(1:4,2,2)) "
## results in an object, aMatrix, 
## that contains four functions: 
## set(), get(), setinverse(), and getinverse(). 
## It also includes the two data objects, x and inversemat.


makeCacheMatrix <- function(x = matrix()) {
        inversemat <- NULL
        set <- function(y) {
                x <<- y
                inversemat <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inversemat <<- inv
        getinverse <- function() inversemat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve() is required to populate or retrieve 
## the inverse from an object of type makeCacheMatrix()

cacheSolve <- function(x, ...) {
        inversemat <- x$getinverse()
        if(!is.null(inversemat)) {
                message("getting cached data")
                return(inversemat)
        }
        data <- x$get()
        inversemat <- solve(data, ...)
        x$setinverse(inversemat)
        inversemat
}

