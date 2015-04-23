## AUTHOR: Mala Viswanath
## Date: 4/22/2015
## Version 1.0


###########################################################################
# NAME: makeCacheMatrix
# 
# ARGUMENT: Matrix
#
# PURPOSE: 
# This function creates a special vector that contains four functions as follows:
# function set: sets the value of vector via super assignment
# function get: gets the value of vector
# function setinverse: sets the value of inverse via super assignment
# function getinverse: gets the value of inverse
# 
# RETURN
# The function returns a list of the above four functions.
###########################################################################

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##############################################################################
# NAME: cacheSolve
#
# ARGUMENT:
# Special vector created by function makeCacheMatrix.
# The vector needs to contain a list of four functions
# set, get, setinverse, getinverse
#
# PURPOSE:
# The purpose of this function is to return the inverse of the matrix that
# is sent in as part of the argument. The function will first search the cache
# for the inverse. If it finds the inverse (stored in global variable m), it will
# return the inverse from the cache.
#
# If the cache does not have the inverse stored in there, this function will use 
# R inbuilt function "solve" to compute the matrix inverse. It will save the inverse
# in cache by calling "setinverse". It will return the inverse to the caller of the
# function.
#
# RETURN VALUE: Inverse of the matrix. 
# If the data was retrieved from the cache, you will 
# additionally get a message on the screen that says "getting cached data"
##############################################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
