## Put comments here that give an overall description of what your
## functions do

## This function create a list from a matrix which allows to cache the results of its inverse
## matrix computation. This function works in conjuction with the cacheSolve function.
##
## The matrix and its computed inverse it's not stored directly in the list, but in the enviroment
## of the function which created the list.
##
## This function receives as argument a matrix and returns a list which contains the following
## functions as named elements of it:
## - get: returns the matrix.
## - set: replace the matrix for the one provided as a parameter.
## - setInverse: save the inverse of the matrix,
## - getInverse: get the previously saved inverse of the matrix. If no value was previously saved
##               returns NULL. 


makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y){
    x <<- y
    mi <<- NULL
  }
  get <- function(){
    x
  }
  setInverse <- function(nmi){
    mi <<- nmi
  }
  getInverse <- function(){
    mi
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This functions returns the inverse of a matrix, caching the value of the computation so 
## the inverse is only computed the first time this function is invoked for a particular matrix.
##
## This function receives as an argument a list created with the makeCacheMatrix which
## is a special data structure allows to access a matrix an optionally the inverse of that matrix.
## Refer to the makeCacheMatrix function for more information on this list.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mi <- x$getInverse()
  if (!is.null(mi)){
    print("Returning cached matrix inverse")
    return(mi)
  }
  print("Calculating matrix inverse")
  mi <- solve(x$get())
  x$setInverse(mi)
  mi
}
