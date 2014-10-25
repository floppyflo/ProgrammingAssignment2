## The following R script is the solution for Programming Assignment 2 in R Programming course
## Each function has in-line description

#This function is used to create a list of functions to manage the creation and data access

makeCacheMatrix <- function(X = matrix()) {             
        inv <- NULL
        set <- function(Y) {                            #execute this function to assing the matrix for inversion. This should be the second step after makeCacheMatrix() in called. The argument contains the matrix for inversion
                X <<- Y
                inv <<- NULL
        }
        get <- function() X                             #execute this funcition to pull the data that was created invoking set() function
        setinv <- function(i) inv <<-i                  #execute this funcition to cache the inversion
        getinv <- function() inv                        #execute this funcition to pull the inverted value
        list(set = set, get = get,                      #return the list pointing to the functions
             setinv = setinv,
             getinv = getinv)
}

#This function calculates inversion of a matrix using predefined functions

cacheSolve <- function(x, ...) {                             
        inv <- x$getinv()                               #check if the inversion has been already calculated
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()                                 #if no iversion was calculated, pull the matrix to be inverted
        inv <- solve(data, ...)                              #invert the matrix
        x$setinv(inv)                                   #cache the results of the inversion
        inv
}