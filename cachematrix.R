# makeCacheMatrix creates a special 'matrix' object which can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

# cacheSolve computes the inverse of the special 'matrix' returned by
# makeCacheMatrix. If the inverse has already been calculated (and the matrix
# has not changed), then the cachesolve will retrieve the inverse from the cache.
#
# params
#   x: a cache matrix created with makeCacheMatrix
cacheSolve <- function(x) {
  # see https://class.coursera.org/rprog-004/forum/thread?thread_id=993 for an
  # explanation why I don't allow additional parameters `...`.
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
