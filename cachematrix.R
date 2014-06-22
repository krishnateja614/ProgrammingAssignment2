## Put comments here that give an overall description of what your
## functions do

 
# Caching function for a matrix,which returns a vector of get(x) and set(m) 
# for the matrix itself, as well as  the matrix inverse calculation.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


#Calculates the inverse of a given square matrix, or will retreive a previously
# calculated inverse through cache function defined above,given a list argument
# from the same

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setinverse(m)
  m
}
