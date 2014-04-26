
## This function makes a vector which is a list containing functions for
## get set and set values of inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
  
}


## This function calculates first checks if inverse is calculated, if not, it calculates it and saves it

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) { #check if already calculated
    message("getting cached data") 
    return(m)
  }
  else{ # otherwise calculate it and set to cache
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    return(m)
  }
}


