## This script is able to calculate the inverse of a matrix, and cache 
## the output. 

## The makeCacheMatrix function is able to set the matrix,
## get the matrix, compute the inverse of the matrix, and get the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        #Set the inverse to null
        i <- NULL
        
        #Function to set a matrix to x
        set <- function(y){
            x <<- y
            i <<- NULL
        }
        
        #Function to get the matrix x
        get <- function(){
                #return
                x
        }
        
        #Function to set the inverse of the matrix
        setinverse <- function(solve){
                i <<- solve(x)
        }
        
        #Function to get the inverse of the matrix
        getinverse <- function(){
                i
        }
        
        #Return
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)

}

## The cacheSolve function is able to return the inverse of a matrix, but
## first checks whether it has been cached by the makeCacheMatrix function.
## If it has been cached, it returns the inverse from the cache, otherwise
## it calculates the inverse and sets the inverse in the cache via the
## setinverse function

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        #Check whether the inverse has been cached
        if(!is.null(i)){
                message("Getting cached data.")
                return(i)     
        }
        
        #If there is no cached inverse, get the matrix. calculate the
        #inverse, and cache it
        mat <- x$get()
        i <- solve(mat, ...)
        x$setinverse(i)
        i       
        
}
