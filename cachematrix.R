## This function creates a special matrix object which can cache its reverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
                  x <<- y
                  m <<- NULL
            }
      get <- function() x
      setMatrix <- function(solve) m <<- solve
      getMatrix <- function() m
      list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
      
}


## This function computes the inverse of the special "matrix".
## If reserve is already calculated it will return the reverse from cache, and print information 
## to user that cache was accessed.

cacheSolve <- function(x = matrix(), ...) {
      
      ## 
      m <- x$getMatrix()
      if(!is.null(m)) {
            message("Getting cached data...")
            return(m)
      }
      
      matrix <- x$get()
      m <- solve(matrix, ...)
      x$setMatrix(m)
      m
      
}



