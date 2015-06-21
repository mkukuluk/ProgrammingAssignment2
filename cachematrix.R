##Caching inverse of matrices help speed up future calculations 
##that might use the same inverse because 
##calculating the inverse is a computationally expensive task.


##The following function :
##sets the matrix value
##gets the matrix value
##sets the inverse of matrix
##gets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Checks if inverse has already been calculated.
##If not, calculate inverse.

cacheSolve <- function(x, ...) {
        inv = x$getinv()
  
  ## if the inverse exists
  if (!is.null(inv)){
    return(inv)
  }
  ##if inverse doesnt exist, calculate
  else{
    mat.data = x$get()
    inv = solve(mat.data, ...)
    x$setinv(inv)
    return(inv)
  }
}
