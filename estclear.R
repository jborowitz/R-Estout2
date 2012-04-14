`estclear` <-
function(tn='ccl'){
    assign(x=tn,value=NULL,envir=globalenv())
    return(1)
}

