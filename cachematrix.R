## THe function of this set of functions is to calculate
## the inverse of a matrix

## The following makeCacheMatrix function set the matrix,
## get the matrix, set the inverse of matrix,
## get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y){
        x <<- y
        Inv <<- NULL
    }
    get <- function() x
    setInv <- function(i) Inv <<- i
    getInv <- function() Inv
    list(set = set, get = get, 
         setInv = setInv,
         getInv = getInv)
}


## The cacheSolve function calculate the inverse of the matrix that 
## created in above function. It checks if the inverse has been calculated.
## If so, gets the result. Otherwise, calculate it.

cacheSolve <- function(x, ...) {
    Inv <- x$getInv()
    if(!is.null(Inv)){
        message("Inverse has been calculated")
        return(Inv)
    }
    InvCalc <- x$get()
    Inv <- solve(InvCalc, ...)
    x$setInv(Inv)
    Inv
}
