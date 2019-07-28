## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Return a list storing matrix (x) defined in parent environment,
## and the functions to get, solve, and retrieve it
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve 
  getinv <- function() m
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## Takes a vector made with makeCacheMatrix, and either gets the cached inverse
## value if it exists or solves and stores it if it does not
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


