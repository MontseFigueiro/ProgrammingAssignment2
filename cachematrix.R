##CACHING THE INVERSE OF A MATRIX
##makeCacheMatrix creates a special matrix
##and cacheSolve computes the inverse of this special matrix

##makecachematrix set the value of a matrix and then
##get the value of the solve function this function
##creates a special Matrix -
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setsolve<-function(solve) m <<- solve
        getsolve<-function() m
        list(set=set, get=get,
             setsolve=setsolve, getsolve=getsolve)
}


##cacheSolve calculates the inverse of the special Matrix
##if the inverse has already been calculated, it gets the
##inverse from the cache, otherwise it calculates the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        m<-solve(x$get(),...)
        x$setsolve(m)
        return(m)
}
##Return the inverse of the matrix calculated.
