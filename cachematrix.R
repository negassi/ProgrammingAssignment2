

##################
# This function creats a matrix that cache its inv# The cost of calculating the inverse is high. 
# to minimize the the cost this program will create a cache of the matrix
# to avoid multiple costly calculationserse
##################

# This function creates a cache of a inverse matrix
makeCacheMatrix <- function(x = matrix()) {
 if (ncol(x) !=nrow(x)){stop("Matrix should have equal dimensions to calculate inverse!")}
   inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setInv_mat <- function(z) 
    inv_mat <<- z
  getInv_mat <- function() inv_mat
  list(set = set,
       get = get,
       setInv_mat= setInv_mat,
       getInv_mat = getInv_mat)
}

# This function calculates the inverse of the funcion created by 'MakeCaheMatrix' function
# If the inverse is previously calculated for the same matrix, it fetches the output from cache.
cacheSolve <- function(x, ...) {
  
  inv <- x$getInv_mat()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv_mat(inv)
 inv
}
