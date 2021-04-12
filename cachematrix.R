# Computing the inverse of a square matrix 
# Storing the calculated inverse in a cache
# Retrieving the cached inverse


# Creating a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) i <<- solve
  get_inverse <- function() i
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


# Retrieving cached inverse of the matrix; 
# If not cached, computing the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$get_inverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_inverse(i)
  i
}

# *** END ***