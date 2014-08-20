## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The input matrix is square, inversible and valid.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #inverse saver variable
    set <- function(y) { #Will get the expected matrix
        x <<- y #setting the value of the matrix
        i <<- NULL # reseting the mean value and the new matrix
    }
    get <- function() x #gets the value of the matrix
    setinv <- function(inv) i <<- inv# gets the inverse value and sets it
    getinv <- function() i #retrieves the inverse value
    list(set = set, get = get, #sets the output list. 
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
#This one recieve the super matrix created above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()#checks if the inverse was calculated
    if(!is.null(i)) {
        message("getting cached data")
        return(i)#finish the function with the already calculated value
    }
    data <- x$get()#if not calculates it
    i <- solve(data, ...)#caculates
    x$setinv(i)#save the value to the super matrix
    i#returns it
}
