## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object and a list of functions. 
## Functions in this list are used to set/get matrix value, 
## set/get matrix inverse value

makeCacheMatrix <- function(x = matrix()){
  
  invOfX <- NULL
  
  set <- function(y){
    x <<- y
    invOfX <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setInv <- function(inv){
    invOfX <<- inv
  }
  
  getInv <- function(){
    invOfX
  }
  
  list(set = set, get = get, setInv =  setInv, getInv = getInv)

}


## This function returns the inverse of matrix x. If inverse is available in
## cache then Functions skips calculating the inverse and returns the cache value
## Else it calculates the inverse and returns the calculated inverse value.

cacheSolve <- function(x, ...) {
  
  invOfX <- x$getInv()
  if(is.null(invOfX))
  {
    data <- x$get()
    invOfX <- solve(data, ...)
    x$setInv(invOfX)
  }
  else
  {
    message("getting cached data")
  }
  
  ## Return a matrix that is the inverse of 'x'
  invOfX
}
