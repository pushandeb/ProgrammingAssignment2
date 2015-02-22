#===========================================================================================================
#Purpose : Square Matrix Incersion using R finc solve(x). Make use of memory cacahing so that once computed, 
#there is no need to evaluate inverse till the input matrix changes
#Author : Pushan Deb
#Version 1.0
#date 21.02.2015
#===========================================================================================================

#makeCacheMatrix is an interface function that accepts square matirx 
#and used to initialize( read set ) and return (read get) matrix, it will return a list of get/set function

makeCacheMatrix <- function(x = matrix()) {
  # x is a square matrix
  
  m <- NULL
  
  #function 1 : to set or initialize the matrix to argument value
  #can not call the func without obj ref, so first call makeCacheMatrix in a global env.
  setmat <- function(y){
    
    x <<-y
    m <<- NULL
    
                       }
  #function 2: to get the cached value of x, no need to pass any argument
    getmat <- function()  x
  
  #function 3:  set inverse value of a given matrix, accepts argument invmat
  setinvmat <- function(invmat) m <<- invmat
  
  #function 4:  get inverse value of a given matrix, no need to pass any argument
  
  getinvmat <- function()  m
  
  #following statement is to return a list of function
  list(setmat = setmat, getmat = getmat, setinvmat = setinvmat, getinvmat = getinvmat)
  
}

#cacheSolve will not have any get/set function
cacheSolve <- function(x, ...) {

#assign inverse matrix result that was created by passing X, if x value had chaged during 2 successive function call
#then getinvmat() returns nothing. i.e NULL

  m <- x$getinvmat()
  if(!is.null(m)) {
    message("getting cached data from previous calculation cache")
    return(m) #function closes here is it finds a valid cache otherwise goes to next block
  }
  # assign new value which was input by using setmat(y), now get it by getmat()
  data <- x$getmat()
  
  #now calculate the inverse of matrix "data" above for first time, if ypu sent 1 arg to solve , it will calc inverse
  m <- solve(data, ...)
  #now call setinvmat(invmat) defined in makeCacheMatrix, 
  # R compiler will use lexical search to find definition of setinvmat
  #also cache the output
  x$setinvmat(m)
  return (m)
}