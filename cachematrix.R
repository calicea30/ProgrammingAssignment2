## This function will create an matrix and will cache its inverse
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

## Checks if the inverse is already calculated if so use the cached 
## data, otherwise it caculates the inverse
## https://www.geeksforgeeks.org/inverse-of-matrix-in-r/


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

