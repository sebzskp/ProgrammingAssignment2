## This set of two functions ('makeCacheMatrix' and 'cacheSolve') are used
## to compute and cache a matrix inverse. 


## The first function, 'makeCacheMatrix', takes for input a matrix; it assigns a NULL
## value to a variable called 'inv', which will potentially be used to store/cache the
## matrix inverse computed in 'cacheSolve'. Its ouput is a list of functions that 
## work on its input or 'inv'.

makeCacheMatrix <- function (x = matrix()) {
    inv <- NULL 
    
    set <- function (y) {
        x <<- y 
        inv <<- NULL
    } 
   
    get <- function () x
    
    set_inv <- function (solve) inv <<- solve
   
    get_inv <- function () inv
    
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
 }

## The second function, 'cacheSolve', should take the ouput of 'makeCachMatrix'
## as its input. It retrieves the value of 'inv' from 'makeCachMatrix' and returns it
## but if the value of 'inv' is NULL, it computes the inverse of the input to 
## 'makeCacheMatrix', sets it as the new 'inv' value and returns it. 

cacheSolve <- function(x, ...) {
    inv <- x$get_inv()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inv(inv)
    
    inv   
}
