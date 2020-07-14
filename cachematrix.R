## The following functions only work together, as cacheSolve requires to get a value from makeCacheMatrix. Because objects are cached 
## and a list is returned by makeCachematrix, objects can be accessed outside of the function environments.

# The first function makeCacheMatrix consists of setters and getters for x and inv, a matrix and its inverse
# Since a list is returned, objects can be accessed by : aMatrix <- makeCacheMatrix(typeinmatrix) and then: aMatrix$get(...), aMatrix$set(...)

# example matrix: x <- matrix(data = c(2,1,0,-1,2,-1,0,-2,1), 3,3)

makeCacheMatrix <- function(x = matrix()) {       # x is defined as empty vector, no further definition necessary
        inv <- NULL                               # inv is defined as a NULL vector for later use of inverse calculation 
        
        set <- function(y) {                      
                x <<- y                           # the value of the input argument y is assigned to x 
                inv <<- NULL                      # inv is set to NULL again in case inv already has a value in makeCachematrix
        }
        
        get <- function() x                    # getter function: get retrieves the value for x, which is not defined in get() and
                                               # therefore taken from the makeCacheMatrix environment
        setinverse <- function(solve) inv <<- solve   # setter function for the inverse inv needs to be taken from the parent environment by <<-
        
        getinverse <- function() inv                # inv is taken from the parent environment again, so after setinverse has been calculated
        
        list(set = set, get = get,             # creates a list of both values and functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve only works with makeCacheMatrix. it takes the value by getinverse(), returns the inverse or calculates it from x (by get())

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()                # the function tries to get a value from inv from makeCacheMatrix 
        
        if(!is.null(inv)) {                  # if x is not NULL, it is returned (to the parent environment)
                message("getting cached data")
                return(inv)
        }
        data <- x$get()                      # only if inv is NULL, then the function gets x from get() in makeCacheMatrix and calculates the inverse
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
