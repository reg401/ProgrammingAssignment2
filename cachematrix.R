## The following file contains 2 functions to address the requirments set in:R Programming - Programming Assignment 2 - Caching the Inverse of a Matrix
## The file also contains 2 test functions - one using the cache to retrieve the inverse matrix from the cache and the other doesn't.
##
## Please note that it is assumed that the matrix supplied to the functions is always invertible.
##

######################################################### F U N C T I O N S #########################################################

## This function creates a special "matrix" object that can cache its inverse.
## The following functions are available for the matrix object created:
## set - set the value of the matrix
## get - get the value of the matrix
## setInv - set the value of the inverse matrix
## getInv - get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  ## First, set the inverse matrix variable to NULL
  im <- NULL
  
  ## This function applies the passed matrix (pm) to the special "matrix" object (x) and sets the inverse matrix variable (im) to NULL 
  set <- function(pm) {
    x <<- pm
    im <<- NULL
  }
  
  ## This function returns the special "matrix" object 
  get <- function() x
  
  ## This function sets the inverse matrix of the original matrix in the special "matrix" object
  setInv <- function(solve) im <<- solve
  
  ## This function gets the inverse matrix of the original matrix from the special "matrix" object
  getInv <- function() im
  
  ## The list of available functions
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}



## This function computes (and returns) the inverse of the special "matrix" (x) returned by makeCacheMatrix function above. 
## If the inverse has already been calculated then this function should retrieve the inverse matrix from the special "matrix" cache.
cacheSolve <- function(x, ...) {
  ## First, try to get the inverse matrix (im) from the passed matrix (x)
  im <- x$getInv()
  ## Check if the im is null, if it's not, we are actually retrieving the im from the cache, return the im value and exist the function
  if(!is.null(im)) {
    message("getting inverse matrix cached data")
    return(im)
  }
  
  ## Too bad, no im value in the cache, let's get the original matrix data
  data <- x$get()
  ## Let's create the inverse matrix (im)
  im <- solve(data, ...)
  ## Set the inverse matrix in the special "matrix" object
  x$setInv(im)
  ## Return the inverse matrix
  ## im variable can be used
  ## However,  returning the inverse matrix stored in the special "matrix" (X) ensures that the set function is working properly
  x$getInv()
}


######################################################### T E S T I N G #########################################################

##Test the above functions, not getting the cached inverse matrix
testNoCache <- function(){
  
  ## Create the test matrix
  tm <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
  ## Add the matrix to the special "matrix"
  mt <- makeCacheMatrix(tm)
  ## inverse the matrix and print it
  im <- cacheSolve(mt)
  im
}


##Test the above functions, get the cached inverse matrix
testCache <- function(){
  
  ## Create the test matrix
  tm <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
  ## Add the matrix to the special "matrix"
  mt <- makeCacheMatrix(tm)
  ## inverse the matrix
  im <- cacheSolve(mt)
  ## get the inverse matrix from the cache and print it. This should also trigger a "getting inverse matrix cached data" message.
  cim <- cacheSolve(mt)
  cim
}

