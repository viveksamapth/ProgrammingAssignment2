

makeCacheMatrix<- function(x=matrix())  ## This function create a special "matrix" objects that can cache its inverse              
{
        inv<- NULL ## intiallize inv as NULL; will hold the value of matrix inverse.
        set<- function(y)## define the set function to assign new
        {
                x<<-y  ## value of matrix to parent enviroment 
                inv<<- NULL  ## if there is new matrix , reset inv to NULL
        }
        get <-function(){x} ## define the get function, returns the value of matrix arugment
        setInverse<-function(inverse){inv<<-inverse} ## asign the value of inv in parent enviroment
        getInverse<-function(){inv} ## get the inv value where called
        list(set = set, get= get, setInverse = setInverse, getInverse= getInverse) ## you need this in a order to refer to function with $ operator
        
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache
cachesolve<- function(x,...) ## return a matrix is that is inverse of x
{ 
        inv<- x$getInverse()
        if(!is.null(inv)){
                retun(inv)
        }
        mata<-x$get()
        inv<-solve(mata,...)
        x$setInverse(inv)
        inv
}
