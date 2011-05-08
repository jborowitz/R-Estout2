`eststo2` <-
function(x,est_column=NULL,stats=NULL, model.name=NULL){ # starting function "eststo"
    # stats is a list from list(stat1=value,stat2=value)...  In the
    # default, it will go with the behavior that was already here.

    # You can optionally name the model that you are storing by putting a
    # string as model.name.  If you don't put a string, you will get a name
    # of 'modeln' where n counts up from 1.

    #print(stats)
#summary <- summary(x)
#class <- attributes(x)$class[[1]]
#coeff <- summary$coefficients
#vars <- attributes(summary$coeff)$dimnames[[1]] #variable.names(x) 
#vars <- attr(coef(x),'names') # my version of names
#col_length <- length(coeff)/4  # number of vars
#stats <- x$stats
#rname <- c("results","stats")
#print(rname)
#nr <- length(rname)
#nc <- length(cname)


#m[["u", "ave"]] <- ave.u
if(exists("ccl",where=1)){
    old.results <<- ccl # access current results matrix into old.results 
    if(is.null(model.name)){
        model.name <- paste('model',length(old.results[1,])+1,sep='')
    }
    these.results<- matrix(list(),2,1,dimnames =
                           list(c('stats','results'),c(model.name)))
    # Make a new matrix of results for this model

    if(!is.null(stats)) these.results[['stats',model.name]] <- stats
    else these.results[['stats',model.name]] <- list()
    these.results[['results',model.name]] <- x
    return(ccl <<-cbind(old.results,these.results))

}  # grabbing existing list
else{
    print("Something bad is happening with the ccl object")
}

} # eststo-end

