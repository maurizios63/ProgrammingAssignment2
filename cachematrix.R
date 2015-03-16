## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Function makeCacheMatrix creates and cache a Matrix together with its 
# inverse
# It uses 2 internal variables
# x to store the the matrix itself
# y its inverse
#
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # store inverse; initialized to null
# Set store matrix and reset inverse to null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
# get returns the matrix
  get <- function() x
# setinv use the function solve to returns the matrix
  setinv <- function(solve) inv <<- solve
# getinv return the cached inv value
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# The function operares o a cached Matrix object
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
# Retrieve inverse
  inv <- x$getinv()
# if not null inv has been previously computed and the cached value an be taken
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
# Otherwise retireve data and comupte the cache value
  data <- x$get()  # Retrieve data
  inv<- solve(data, ...) # Compute the inverse value
  x$setinv(inv)  # Store it in function cache
  inv
}
