## These to functions implement caching to help speed up large operations on a matrix

## - create a list of functions that get and set a matrix, and get and set the solved version of a matrix
## usage is matrixList <- makeCacheMatrix(matrix)
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- null
  }
  get <-function() x                       # get the matrix back out via matrixList$get()
  setsolve <- function(solve) s <<- solve  # set the solved function with matrixList$setsolve(solve(matrix))
  getsolve <- function() s                 # get the solved matrix with matrixList$getsolve()  
  list(set = set,   
       get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)                # create and return the list of functions
}


## Check if a matrix has already been solved and cached or not
cacheSolve <- function(x) {
    s <- x$getsolve()      #fetch the solved matrix (null if it hasn't been done)
    if(!is.null(s)) {
      message ("Returning cached solution")
      return(s)            # yippy we already have a chached version, carry on
    }
    # no solution is already cached, lets make one
    data <- x$get()        # get the data out of our list of functions
    s <- solve(data)       # solve for our matrix (assuming it is solvable, no error checking as per assignment)
    x$setsolve(s)          # set the solution in cache
    s                      # return the solution
}

# to test these functions try:
#  solveableMatrixMaker <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") } 
#  solveableMatrix <- solveableMatrixMaker(8)
#  matrixList <- makeCacheMatrix(solveableMatrix)
#  matrixList$get()
#  cacheSolve(matrixList)  # not cached yet
#  cacheSolve(matrixList)  # cached, you will see the "returning cached solution
#  


