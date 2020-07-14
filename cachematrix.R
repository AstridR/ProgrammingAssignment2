## The following functions only work together, as cacheSolve requires to get a value from makeCacheMatrix

# The first function makeCacheMatrix consists of setters and getters for x and minv, a matrix and its inverse
# Since a list is returned, objects can be accessed by : aMatrix <- makeCacheMatrix(typeinmatrix) and then: aMatrix$get(...), aMatrix$set(...)


# example matrix: x <- matrix(data = c(2,1,0,-1,2,-1,0,-2,1), 3,3)

makeCacheMatrix <- function(x = matrix()) {       # x is defined as empty vector, no further definition necessary
        inv <- NULL                               # m is defined as a NULL vector for later use of mean calculation 
        
        set <- function(y) {                      
                x <<- y                           # the value of the input argument y is assigned to x 
                inv <<- NULL                      # m is set to NULL again in case m already has a value in makeVector
        }
        
        get <- function() x                    # getter function: get retrieves the value for x, which is not defined in get() and
                                               # therefore taken from the makeVector environment
        setinverse <- function(solve) inv <<- solve   # setter function for the mean. m needs to be taken from the parent environment by <<-
        
        getinverse <- function() inv                # m is taken from the parent environment again, so after setmean has been calculated
        
        list(set = set, get = get,             # creates a list of both values and functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve only works with makeCacheMatrix. it takes the value by getinverse(), returns the inverse or calculates it from x (by get())

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()                # the function tries to get a value from x which was calculated in makeVector and assign it to m
        
        if(!is.null(inv)) {                  # if x is not NULL it is returned to the parent environment
                message("getting cached data")
                return(inv)
        }
        data <- x$get()                      # only if m is NULL, then the function gets  x from get() in makevector and calculates a mean
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
