## makeCacheMatrix and cacheSolve are used to store and calculate the inverse of a matrix in parent environments 
## for efficient use in multiple caclulations 

## makeCacheMatrix is a function that stores a list of four functions
## get is a function that returns the base matrix x stored in the parent environment 
## set is a function that changes the base matrix x stored in the parent environment
## setinv and getinv are functions very similar to set and get. 
## they simply store to and get from the parent environment a (supposed) inverse of the base matrix 

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y) {
        x<<-y
        i<<-NULL
  }
  get<-function() x
  setinv<-function (inverse) i<<-inverse
  getinv<-function () i
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve calculates the inverse of a matrix
## input of cacheSolve is the object where makeCacheMatrix is stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
