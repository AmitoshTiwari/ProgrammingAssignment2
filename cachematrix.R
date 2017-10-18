## makecachematrix  function returns a list which has 4 elements  
## m is declared outside makecachematrix fuction so that inverse of the matrix is passed to it, it remains even after invocation of function


m <- NULL
makeCacheMatrix <- function(x = matrix()) {
  set_matrix <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get_matrix <- function()x
  
  set_inverse_matrix <- function(inmat){m<<-inmat}
  
  get_inverse_matrix <- function()m
  
  list( get_matrix = get_matrix,
        set_matrix = set_matrix,
        set_inverse_matrix = set_inverse_matrix,
        get_inverse_matrix = get_inverse_matrix)
  
}


## cachesolve function gets inverse of a matrix and display a message if it is reteived from cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$get_inverse_matrix()
  if(!is.null(m))
  {message("getting cached data")
    return(m)
  }
  data<- x$get_matrix()
  data1<- solve(data)
  m<-x$set_inverse_matrix(data1)
  m
  
  }
