## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function defines creation of special matrix. It lists the functions...
# set the value of the Matrix
# get the value of the Matrix
# set the inverse of the Matrix
# get the inverse of the Matrix
makeCacheMatrix <- function(x = matrix()) {
  #cache for inverse
  invMat <- NULL
  #set the Matrix, initiating the inverse to null.
  setMatrix <- function(y){
    x<<-y
    invMat<<-NULL
  } 
  #returns the matrix
  getMatrix <- function()x
  #sets the matrix inverse
  setInvMatrix <- function(invMatP) invMat <<- invMatP
  #gets the matrix inverse
  getInvMatrix <- function() invMat
  #functions defined for invoking
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}

## Write a short comment describing this function

# The following function calculates the inverse of the special matrix created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the invers of the matrix and sets the inverse in the cache via the setInvMatrix function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## get the inverse matrix
  invMatRet <- x$getInvMatrix()
  ## check and see if it is null
  if(!is.null(invMatRet)){
    #if not null, indicates it is already cached. So get it
    message("getting cached data")
    return(invMatRet)
  }
  #if this is not null, set the matrix
  dataMatrix <- x$getMatrix()
  #...set the inverse
  invMatRet <- solve(dataMatrix)
  x$setInvMatrix(invMatRet)
  #...return it
  invMatRet
}
