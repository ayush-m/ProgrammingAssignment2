 ##We know that computing inverse of a matrix is a complex task specially when the matrix is very big say 100 by 100.
 ##If we want the value of inverse of such large matrix in our computation in R again and againg, it is not advisable to
 ##compute the inverse again and again rather,it may make sense to cache the value of its inverse so that when we need
 ##it again, it can be looked up in the cache rather than recomputed.




##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        # This sets the cache of "mat" object given to its argument
        setcache<-function(mat){
                x<<-mat
                m<<-NULL
        }
        # This gets the stored cache named as "x" from parent environment
        getcache<-function(){
                x
        } 
        # This sets "inver" to cache named as "m"
        setinverse<-function(inver){
                m<<-inver
        }
        # This gets the stored cache named "m"
        getinverse<-function(){
                m
        }
        list(setcache=setcache, getcache=getcache, setinverse=setinverse, getinverse=getinverse)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve
##the inverse from the cache.

cacheSolve <- function(x, ...) {
        inver<-x$getinverse()
        if(!is.null(inver)){
                message("getting cached data")
                return(inver)
        }
        data<-x$getcache()
        inver<-solve(data)
        x$setinverse(inver)
        inver
}
