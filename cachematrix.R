
## This functions allow us to calculate the inverse of a matrix and store its content in a "globlal environment variable" to use its value when needed. 
 
# makeCacheMatrix returns a list of functions with the possibility to set, and get the result of the invers matrix; it also assign the value of the invers matrix to a "globlal environment variable" or the cache variable. 

# Example of use: cacheSolve(makeCacheMatrix(matrix(nrow=3,ncol=3,c(3,6,7,8,3,5,7,9,3))))

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
        set <- function(y) {  #Sets the value of a new the matrix and if the value of the of a new matrix has been set, the valie of the cache varaible "m" is deleted, and prepered for the new inverse matrix value 
                x <<- y
                m <<- NULL 
        }
        get <- function() x			#Function with no arguments that returns the value of the matrix stored
        setinvmat <- function(invmat) m <<- invmat #Sores the inverse matrix in the global environment variable 
        getinvmat <- function() m  #Returs the value stored in the cache variabe.
        list(set = set, get = get,	#Sets the list of funtions to be call by the cacheSolve function
             setinvmat = setinvmat,
             getinvmat = getinvmat)


}


## This function us the responsabe to evaluate if the matrix has been set and if so, return the calculated value instead of re-calculate it inverse. 

cacheSolve <- function(x, ...) {
       m <- x$getinvmat()        #Mi lista la renombro como "x"
        if(!is.null(m)) {    #pregunto si en m (valor de la media) es contrario a nulo, es decir, si hay valor solo lo extraigo
                message("getting cached data")
                return(m)
        }
        mat <- x$get()    #Mando traer la x
        m <- solve(mat)  #Calculo mi matriz inversa
        x$setinvmat(m)
        m ## Return a matrix that is the inverse of 'x'
}
