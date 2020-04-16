## The first function makeCacheMatrix will take in the target 
## matrix to be inversed as argument and return a list, which 
## contains 4 functions, to be used by the second function, 
## cacheSolve which will calculate and return the inverse of  
## the target matrix if the value has not been calculated and
## otherwise retrieve and return the cached value from the  
## makeCacheMatrix function.
 
## This function takes in the desired matrix to be inversed as 
## argument and returns a list of 4 functions: set, get, setinv
## and getinv to be passed to the cacheSolve function to 
## calculate inverse.The cached value of the inversed matrix 
## can be saved in the invmat variable.

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        set <- function(y) {
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) invmat <<- inverse
        getinv <- function() invmat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## The function will first check if the return value of getinv
## is NULL, if it is not NULL, that means the inverse had 
## already been calculated and the cached value invmat is 
## retrieved. If it is NULL, the target matrix is retrieved 
## using get function and the inverse is caculated and then 
## stored with setinv.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat <- x$getinv()
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data, ...)
        x$setinv(invmat)
        invmat
}
