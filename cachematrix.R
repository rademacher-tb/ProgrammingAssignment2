## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(tMat = matrix()) {
  
  ##create a temp matrix variable
  tMatInv <- NULL
  
  ##create a function to house the inverse of the Matrix
  fnSetMat <- function(mat){
    tMat <<- mat  
    tMatInv <<- NULL
    
  }
  fnGetMat <- function() tMat
  fnSetMatInv <- function(MatInv) tMatInv <<- MatInv
  fnGetMatInv <- function() tMatInv
  list(fnSetMat = fnSetMat, fnGetMat = fnGetMat, fnSetMatInv = fnSetMatInv,
       fnGetMatInv = fnGetMatInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
  
  ## Call the getMat function to so if the Inverted Matrix already
  ## exists
  
    
  tMatInv <- x$fnGetMatInv()
  
  tNewInv <- x$fnGetMat()
  ##print(tNewInv)
  tNewInv <- solve(tNewInv)
  ##print(tOldMatInv)
  ##print(tNewInv)
  
  ## If the getMat function is not null, an inverse of the matrix
  ## already exists.  Return the stored inverse.
  
  ## I found this matrix comparison code snippet from
  ## http://r.789695.n4.nabble.com/Compare-two-matrices-in-r-td4633082.html
  
  if(!is.null(tMatInv) &&
     !is.null(tNewInv) &&
      is.matrix(tMatInv) && 
      is.matrix(tNewInv) && 
       dim(tMatInv) == dim(tNewInv) && 
       all(tMatInv == tNewInv)
     ){
    
  ## end of matrix comparison code snippet  
    message("Inv already exists")
    return(tMatInv)
  }
  
  ## If the getMat returns a null, create the inverse matrix and
  ## return it.
  
  else {
    ##print("inv matrix does not exist")
    tMat <- x$fnGetMat()
    tMatInv <- solve(tMat)
    x$fnSetMatInv(tMatInv)
    tMatInv
    
  }
  
}
