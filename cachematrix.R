## This code has two functions: makeCacheMatrix and cacheSolve
## The purpose of "makeCacheMatrix" function is to create the matrix
## The purpose of "cacheSolve" function is to calculate the inverse of the matrix

## makeCacheMatrix function consists of set(), get(), setinverse(), getinverse()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x<<-y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse)inv <<- inverse 
  getInverse <- function ()inv 
  list (set = set, 
        get = get, 
        setInverse =setInverse, 
        getInverse = getInverse)
}


## cacheSolve function consists of getInverse and inv <- solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting chached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve.default(mat,...)
  x$setInverse(inv)
  inv
}
