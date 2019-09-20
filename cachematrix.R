## Put comments here that give an overall description of what your
## 

## This function takes a matrix as a argument, caches it's value and instantiates 
## the inverse of that matrix to NULL. The function returns a list of functions.
## set() and get() can set and get the matrix, while setinv() and getinv() set
## and get the inverse for the matrix.  The set() function will force the inverse 
## of the matrix to re-cache, if you will, back to NULL. This ensures that the
## inverse gets recalculated whenever the matrix is updated (re-set).

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


## Pass the list from the previous function (makeCacheMatrix) into this function (cacheSolve).
## cacheSolve will get the inverse from the cache and check it's value.  If the value is null
## cacheSolve will take the matrix value from the cache and compute the inverse (calling the
## solve function).  It will then re-set the cache to reflect the result from solve - thus
## storing the inverse by calling the setinv function that is baked into the calling argument
## of cacheSolve (result of makeCacheMatrix).

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}