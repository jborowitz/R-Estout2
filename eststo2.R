`eststo2` <-
function(x,stats=NULL,tableName='ccl',index=NULL){ # starting function "eststo"
    # stats is a list from list(stat1=value,stat2=value)...  In the
    # default, it will go with the behavior that was already here.
    # x is a results object, which has the following fields defined.
    # Standard errors can be calculated by returning:
    #
    # sqrt(diag(FUN(x)))
    # For some function FUN.  If this function is not 'vcov', then the
    # variance covariance matrix will have to be specified manually during
    # esttab
    #
    # tableName is the name of the string where the results are stored.  It is
    # the string which will be the name of the global variable which will be
    # created to store the models.

name <- as.name(tableName)
if(exists(tableName,where=globalenv())){
    old.results <<- eval(name) # access current results matrix into old.results 
        model.name <- paste('model',length(old.results[1,])+1,sep='')
    # Name the model 'modeln'

    these.results<- matrix(list(),2,1,dimnames =
                           list(c('stats','results'),c(model.name)))
    # Make a new matrix of results for this model, called 'these.results'

    if(!is.null(stats)) these.results[['stats',model.name]] <- stats
    else these.results[['stats',model.name]] <- list()
    # If stats are passed to this model, then put them into the stats
    # object

    these.results[['results',model.name]] <- x
    # Put x into these results

    assign(x=tableName,value=cbind(old.results, these.results),envir=globalenv())
    #Save the results into the global environment

    return(eval(name))

}  # grabbing existing list
else{
    #This is the first model and so doesn't need to be appended
    model.name <- 'model1'
    # Name the first model 'model1'

    these.results<- matrix(list(),2,1,dimnames =
                           list(c('stats','results'),c(model.name)))
    # Make a new matrix of results for this model, called 'these.results'

    if(!is.null(stats)) these.results[['stats',model.name]] <- stats
    else these.results[['stats',model.name]] <- list()
    # If stats are passed to this model, then put them into the stats
    # object

    these.results[['results',model.name]] <- x
    
    # Save the first results to the global environment
    assign(x=tableName,value= these.results,envir=globalenv())
    return(eval(name))
}

} # eststo-end

