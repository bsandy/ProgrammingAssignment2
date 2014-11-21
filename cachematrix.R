## This file contains two classes:
##    makeCacheMatrix()
##    cacheSolve()

## CLASS: makeCacheMatrix() ###############################################
## Properties:
##    x       (parameter) Martix in it's original form.  
##    m       Matrix to be modified in cacheSolve
## Methods:
##    set     Sets the property X to the parameter x.
##    get     Outputs property x.
##    setinv  Sets the property m to the matrix from cacheSolve
##    getinc  Outputs the property m.
## Notes:
##    If no methods are chosen, returns an object of the class makeCacheMatrix()
##    When this class is instanciated, parameter M is set to NULL
##    When this class in instanciated, Sets the property X to the parameter x.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  

  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x

  setinv <- function(inv) m <<- inv

  getinv <- function() m

  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv
  )
}

## CLASS: cacheSolve() ###############################################
## Properties:
##    x       (parameter) Object of the class makeCacheMatrix().  
##    m       Matrix to be modified inverted by the function solve
##  When Called:
##    Sets m in passed class to inverted x from passed class.
##  Note:
##    If m from passed class is not NULL, returns m.
##  Returns:
##    Inverted matrix.

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
