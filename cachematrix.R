
## 1 creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  value <- NULL #will store the cached inverse matrix
  set <- function(y) {
    x <<- y
    value <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) value <<- solve
  getinverse <- function() value
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 2 computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  value <- x$getinverse()
  if(!is.null(value)) {
    message("getting cached data")
    return(value)
  }
  data <- x$get()
  value <- solve(data, ...)
  x$setinverse(value) #Cache the inverse
  value
}
