## Put comments here that give an overall description of what your
## functions do

## this function creates a matrix 
## and sets global variable, m, to NULL

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x<<- y
    m<<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## this function takes a matrix and checks if its inverse is stored in m, and if not
## it uses solve() to get the inverse

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)){ ##checking the cache
    message("getting cached data") ##returning cached data
    return(m)
  }
  ##getting the inverse because it wasn't cached
  matrix <- x$get()
  m <- solve(matrix,...) 
  x$setmatrix(m)
  m
}
