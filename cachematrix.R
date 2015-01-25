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
        #checks whether the inverse exists in cache or not 
        if(!is.null(inver)){
                message("getting cached data")
                return(inver)
        }
        #if inverse doesn't exist in cache then it creates the cache of inverse of matrix x
        data<-x$getcache()
        #solve() function creates the inverse of matrix
        inver<-solve(data)
        x$setinverse(inver)
        inver
}
##Using above functions
##first of all load the functins
#    source("makeCacheMatrix.R")
#    source("cacheSolve.R")
##now create a matrix
#    mat<-matrix(c(1,2,3,5,7,9,4,6,2),3,3)
##this creates a list named "matrix"
#    matrix<-makeCacheMatrix(mat)
##Now to diplay the cache stored,type
#    matrix$getcache()
#        [,1] [,2] [,3]
#   [1,]    1    5    4
#   [2,]    2    7    6
#   [3,]    3    9    2
##Now apply cacheSolve function on c
#    cacheSolve(c)
#            [,1]       [,2]       [,3]
#   [1,] -2.2222222  1.4444444  0.1111111
#   [2,]  0.7777778 -0.5555556  0.1111111
#   [3,] -0.1666667  0.3333333 -0.1666667
##On apply cacheSolve function again we can see that now we are getting the cached data of inverse of matrix
#    cacheSolve(c)
#   getting cached data
#           [,1]       [,2]       [,3]
#   [1,] -2.2222222  1.4444444  0.1111111
#   [2,]  0.7777778 -0.5555556  0.1111111
#   [3,] -0.1666667  0.3333333 -0.1666667
