## Put comments here that give an overall description of what your
## functions do

#       Coursera : R Programming Week 3 Assignment
#       Code for cacheing a matrix

#       The first function, makeCacheMatrix creates a special "vector",
#       which is really a list containing a function to
#               set the value of the matrix
#               get the value of the matrix
#               set the value of the inverse of the matrix
#               get the value of the inverse of the matrix

#       The second function cacheSolve() calculates the inverse of the matrix created 
#       with the makeCacheMatrix() function. It first checks to see if the inverse has 
#       already been calculated. If so, it gets the inverse from the cache and skips 
#       the computation. Otherwise, it calculates the inverse of the matrix and sets the 
#       value of the inverse in the cache via the setinverse() function.


## Write a short comment describing this function


# makeCacheMatrix is defined as function which takes
# a matrix as an arguement. Function matrix() converts arguements into a matrix
 
# m is initialised to NULL
  
# in the function set() the arguement y is assigned to x which is not in the current 
# environment using the "<<-" operator. Please note both x and m are not in the
# environment of set().
  
# get() returns the value of matrix object x
  
# setinverse calls generic function solve() which is used to calculate inverse of matrix
# and the inverse is assigned to m
 
# getinverse returns the value of m which could be NULL if not assigned the
# inverse of matrix by setinverse() i.e first time else it returns the inverse
 
# makes a special vector which is a list of functions to set and get the value 
# of matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function

#  The arguement of cacheSolve() is an instance of special matrix created by 
#  makeCacheMatrix()

#  local variable m is assigned the value returned by the function getinverse() 
#  defined in makeCacheMatrix()
  
#  Checks if the inverse has already been calculated and stored. If getinverse() 
#  did not return NULL (value stored in local variable m) then it is returned 
#  and function terminates. If m is NULL that means the inverse has not been calculated
#  the following code is executed.

#  stores the matrix value in local variable data

#  Calculates inverse of data and stores it in m
 
#  sets the value of inverse in the special matrix instance by calling function setinverse
#  on the makeCacheMatrix() instance  
 
#   returns the value of inverse so calculated  

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

