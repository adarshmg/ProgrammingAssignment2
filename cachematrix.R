## Cache and Retrieve Inverse Matrix.
## Purpose: Create makeCacheMatrix function to cache the matrix and 
##          its inversion 
##          Create cacheSolve to  look into cache created by makeCacheMatrix
##          and retrieve inverse matrix if present else create inverse
##          inverse matrix and add it to cache.
##      

##
# makeCacheMatrix creates a special "matrix", which is a list 
# containing a function to
#  * set the value of the invertible matrix
#  * get the value of the invertible matrix
#  * set the value of the inverse
#  * get the value of the inverse
#  Parameters - invertible matrix
#  Returns - List
##
makeCacheMatrix <- function(mtx = matrix()) {
        inv <- NULL
        
        # Store the incoming matrix
        set <- function(x){
                mtx <<- x
                inv <<- NULL
        }
        
        # Retrieve the stored matrix.
        get <- function(){
                mtx
        }
        
        # Store the inverse matrix.
        setMatrixInverse <- function(x){
                inv <<- x
        }
        
        # Retrieve the stored matrix.
        getMatrixInverse <- function(){
                inv                
        }
        
        # Return a list of the above functions.
        list(set = set,
             get =get, 
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse
        )
        
}

##
# The below function returns the matrix inverse, if present in cache else
# calculates the inverse, stores it in cache and returns the inverse 
# matrix
# parameters: makeCacheMatrix vector
#              additional inputs to solve function.
# result: inverse matrix.
##
cacheSolve <- function(x, ...) {
        # Retrieve inverse matrix from Cache.
        inv <- x$getMatrixInverse()
        
        # If inverse matrix is present, show a message.
        # If no inverse matrix is present, compute it
        # and store it in cache.
        if( !is.null(inv) ){
                message("Returning cached inverse matrix")
        } else {
                inv <- solve(x$get(), ...)
                x$setMatrixInverse(inv)
        }
        
        # Return a matrix that is the inverse of 'x'
        inv
} 
