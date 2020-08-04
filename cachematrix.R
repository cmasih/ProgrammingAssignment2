## Programming Assignment 2 
## Writing a pair of functions that cache the inverse of a matrix

## Step 1: Create a matrix object that can cache its inverse

makeCacheMatrix <- function(x=matrix()){  #initialising x as a matrix
  i <- NULL   # initialising i  within the function environment for future use
  set<-function(y){
    x<<-y  # assigning  input value of y to x within the function environment
    i <<- NULL
  }
  get<- function() x  # retrieves value of x from function environment
  setinverse <- function(inverse) i <<- inverse # assing input value to i 
  getinverse <- function() i  # retrieves the value of i 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse=getinverse) # create a list with each function as named elements
}


## Step 2: Compute the inverse of matrix returned by makeCacheMatrix() 

cacheSolve <- function(x,...){
  i <- x$getinverse() # assigns inversed matrix of inputted matrix (x) to i
  if(!is.null(i)){  # determine if inversed matrix is cached 
    message("getting cached data") # if cached, prints message
    return(i) # returns the matrix assigned to i 
  }
  data <- x$get() # retrieve matrix from input value/argument
  i <- solve(data,...)  # inverse the matrix
  x$setinverse(i) # set i to the setinverse element of the list in makeCacheMatrix()
  i # Return a matrix that is the inverse of 'x'
}

