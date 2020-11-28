makeCacheMatrix<- function(x=matrix()){
        inv<- NULL
        set<- function(y){
                x<<-y
                inv<<- NULL
        }
        get <-function(){x}
        setInverse<-function(inverse){inv<<-inverse}
        getInverse<-function(){inv}
        list(set = set, get= get, setInverse = setInverse, getInverse= getInverse)
        
}
cachesolve<- function(x,...){
        inv<- x$getInverse()
        if(!is.null(inv)){
                retun(inv)
        }
        mata<-x$get()
        inv<-solve(mata,...)
        x$setInverse(inv)
        inv
}