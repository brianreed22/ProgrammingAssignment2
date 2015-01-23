#This function creates a list containing 4 functions: get, set, setInv, and getInv.
#It uses the ' <<- ' operator in order to prevent exposure to the exterior environment.

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL 
  set <- function(y) {
    x <<- y
    xinv <<- NULL 
  }
  get <- function() x 
  setInv <- function(inv) xinv <<- inv 
  getInv <- function() xinv 
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


#This function returns the inverse of a matrix created in the makeCacheMatrix function.
#If cached inverse is available, cacheSolve retrieves it.
#If cached inverse is not available, it will compute, cache it, and return it.

cacheSolve <- function(x, ...) {
  m <- x$getInv()  #Calculating the inverse of matrix x.
  if(!is.null(m)) { 
    message("getting cached data")
    return(m) 
  }
  data <- x$get()
  m <- solve(data) 
  x$setInv(m) 
  m 
}

