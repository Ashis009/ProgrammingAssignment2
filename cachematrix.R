
## It returns a list containing  functions which can be called to set or get the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
  x_inv= null
  
  set = function(y){
       x <<- y
       x_inv <<- null
  } 
  
  get = function() x
  
  
  set_inv = function(inv)  x_inv= inv
  get_inv = function() x_inv
  
  
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## It creates the inverse of a square matrix if the inverse is not already cached.

cacheSolve <- function(x, ...) {
         x_inv =  x$get_inv()
  
  
  if(!is.null(x_inv)){
    
    message("getting the cached data")
    return(x_inv)    
  }
  
  data= x$get()
  x_inv = solve(data, ...)
  
  x$set_inv(x_inv)
  
  x_inv
}
