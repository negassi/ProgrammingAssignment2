


##the following function, 
##makeCacheMatrix creates a special "vector", that has a list containing a function to 
##1.set the value of the vector
##2.get the value of the vector
##3.set the value of the matrix
##4.get the value of the matrix



makeCacheMatrix <- function(x = matrix()) {
 mtx <- NULL
        set <- function(y) {
                x <<- y
                mtx <<- NULL
        }
        get <- function()x
        setSolve <- function(solve) mtx <<- solve
        getSolve <- function() mtx
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)

}

##The following function calculates the inverse of the special "vector" created 
##with the above function. However, it first checks to see if the inversse of the matrix has already
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setSolve function.


cacheSolve <- function(x, ...) {
mtx <- x$getSolve()
        if(!is.null(mtx)) {
                message("getting cached data")
                return(mtx)
        }
        data <- x$get()
        mtx <- solve(data, ...)
        x$setSolve(mtx)
        mtx

}
