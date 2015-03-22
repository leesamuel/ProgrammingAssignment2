# makeCacheMatrix creates a matrix with the argument provided. 
# It must be square (2x2) matrix
# Use makeCacheMatrix() to create the 2x2 matrix to test
# for example, a <- makeCacheMatrix(matrix(1:4,2,2))


makeCacheMatrix <- function(x = matrix()) {  # Create the 2x2 matrix to test
  m <- NULL  # clear any previous value of m
  if (!is.matrix(x))
  {
    print("Enter Matrix Object")  # error message in case non-matrix entered
    return(NULL)  # NULL value when non-matrix entered
  }else{
    set <-function(y) 
      x <<- y  # set matrix via lexical scoping
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve  # use solve function to get inverse matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## Write a short comment describing this function

cacheSolve <- function(x=matrix(),...) {  # use to solve the sample matrix
  if(!is.null(m)) {
    message("getting cached data")  # prevent recalculation if it has already been solved and show message
    return(m)  # return the inverse matrix that has already been calculation before
  }
  matrix <- x$get()
  m <- solve(matrix,...)  #solve the matrix if it has not been calculated yet
  x$setmatrix(m)  
  m
  
}

       
