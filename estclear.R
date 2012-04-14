`estclear` <-
function(tableName='ccl'){
    assign(x=tableName,value=NULL,envir=globalenv())
    return(1)
}

