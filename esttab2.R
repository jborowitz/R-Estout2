`esttab2` <-
    function(beta.value=TRUE, se.value=TRUE, t.value=FALSE, p.value=FALSE, round.dec=3, caption=NULL,
             label=NULL, sig.levels=c(0.05, 0.01, 0.001), sig.sym=c("*", "**",
                                                                  "***"),
             filename=NULL, file.type='tex', dcolumn=NULL, table="table",
             table.pos="htbp", caption.top=FALSE, booktabs=FALSE,
             var.order=NULL, sub.sections=NULL, var.rename=NULL, keep=NULL,
             se.func=vcov, coef.func=coef, col.width=24, col.headers=NULL,
             unstack.cells=FALSE,N=TRUE,R2=TRUE, indicate=NULL, drop=NULL){

        # TODO: get rid of repeated row name warning message.  Document,
        # and clean up.  Potentially I can then add .txt and .csv outputs.
        # Also, I should test really long stats inputs to make sure they're
        # rounded OK with that and the N
        # coef.func and se.fun are function names that can be passed in order
        # to report marginal effects (coef.func) or different standard errors
        # (se.func).  These functions need to work with
        # names(coef.func(results))
        to.include <- c(beta.value,se.value,t.value,p.value)
        cell.names <- c('beta','se','t','p')[to.include]
        fileparts <- strsplit(filename,'\\.')
file.type <- fileparts[[1]][[2]]
supported.file.types <- c('tex','txt','csv')
if(!any(file.type == supported.file.types)) {
    warning('\'filename\' extension does not match supported file type.  TeX
            file chosen as default')
    file.type <- 'tex'
}

        # filename: the name of the file where the output will be saved.  The
        # extension to this must be 'tex', 'csv', or 'txt', which will determine
        # which type of output file is saved.
        #col.headers will override using the dependent variable name 
        #unstack.cells will put each auxiliary element in a separate column
        #N: report the number of observations in each model
        #R2: report the R2 of the model.  If this cannot be accessed through
        #the 'r.squared' attribute of the summary of the results, this will
        #cause an error
        # indicate: takes a list('ageyoungest*'='Age of Youngest') type
        # argument
        #var.rename is a list like: list('old name'='new name', 'another
        #old name' = 'another old name')
        # Note that if you keep something that is absorbed into an
        # 'indicator', it will be ignored.
        indicator.yes <- 'X'
        indicator.no <- ' '
        library('stringr')
        # reading list from eststo
        # TODO: sanitize the string inputs to get rid of newlines from
        # broken strings: stringvec<-gsub('\n','',stringvec) for whatever
        # needs it.
        ccl <<- ccl
        num.models <- length(ccl[1,])
        if(!is.null(col.headers) & length(col.headers) == length(dimnames(ccl)[[2]])) {
            dimnames(ccl)[[2]] <- col.headers
        }
        model.names <- dimnames(ccl)[[2]]
        var.list <- NULL
        indicator.list <- NULL
        for(i in dimnames(ccl)[[2]]) var.list<- union(var.list,names(coef.func(ccl[['results',i]])))
        for(i in names(indicate)){
            matches <- grepl(paste('^',indicate[[i]],'$',sep=''),var.list)
            var.list <- var.list[!matches]
            if(any(matches)) {
                indicator.list<-union(indicator.list,i)
            }
        }

        # The variable list is the union of all variables stored
        if(!is.null(keep)) var.list<- intersect(var.list,keep)
        var.list <- unlist(var.list)
        # only keep those variable names in the keep list: use the intersection to
        # find this list
        if(!is.null(drop)) var.list<- setdiff(var.list,drop)

        model.numbers <- 1:num.models
        #vector of numbers enclosed in parenthesis
        ms1 <- paste('(',model.numbers,')',sep='')
        if(file.type == 'tex'){
            # Start working on header strings
            if(unstack.cells) {
                single.column.padding <- col.width
                padding <- (col.width + 1) * length(cell.names) - 1
                num.multicol <- length(cell.names)
            }
            else{
                padding <- col.width
                single.column.padding <- col.width
                num.multicol <- 1
            }
            model.num.string  <- str_pad(paste("\\multicolumn{",num.multicol,'}{c}{',ms1,'}',sep=''),padding,side='both')
            #enclose the numbers in  the multicolumn command, spanning the number of
            #columns necessary if the results are unstacked
            header.string <-
                paste(str_pad(c('',model.num.string),col.width),collapse='&')
            #Concatenate the headers into one string, separated by & and ended with \\
            ms2 <- str_pad(paste('\\multicolumn{',num.multicol,'}{c}{',model.names,'}',sep=''), padding,side='both')
            model.name.string  <-
                paste(str_pad(c('',ms2),col.width),collapse='&')
        }
        if(file.type == 'csv'){
            #blanks <- rep('',length(cell.names)-1)
            if(unstack.cells){
                padding <- col.width * length(cell.names)
                padding <- 0
                single.column.padding <- 0
                model.name.string <-
                    cbind(matrix(NA,1,1),matrix(rbind(model.names,matrix(NA,length(cell.names)-1,num.models)),1,length(cell.names)*num.models))
                print(model.name.string)
                # Insert 'NA' elements in the column header for the extra
                # cells 
                header.string <-
                        cbind(matrix(NA,1,1),matrix(rbind(ms1,matrix(NA,length(cell.names)-1,num.models)),1,length(cell.names)*num.models))
                print(header.string)
            }else{
                padding <- col.width
                padding <- 0
                single.column.padding <- 0
                header.string <- matrix(c(NA,ms1))
                model.name.string <- matrix(c(NA,model.names))
            }
            #TODO: create csv headers
        } 
        if(file.type == 'txt'){
            if(unstack.cells) {
                single.column.padding <- col.width
                padding <- (col.width + 1) * length(cell.names) - 1
                num.multicol <- length(cell.names)
            }
            else{
                padding <- col.width
                single.column.padding <- col.width
                num.multicol <- 1
            }
            model.num.string  <- str_pad(ms1,padding,side='both')
            print(model.num.string)
            #enclose the numbers in  the multicolumn command, spanning the number of
            #columns necessary if the results are unstacked
            header.string <-
                paste(c(str_pad('',col.width,side='both'),model.num.string), collapse=' ')
            #Concatenate the headers into one string, separated by & and ended with \\
            ms2 <- str_pad(model.names, padding,side='both')
            model.name.string  <-
                paste(str_pad(c('',ms2),col.width),collapse=' ')
        }

        # Work on stats:
        stats.list <- NULL
        for(i in model.names) {
            stats.list <- union(stats.list,names(ccl[['stats',i]]))
            #if(R2) ccl[['stats',i]][['R2']] <- summary(ccl[['results',i]])[['r.squared']]
            #if(N) ccl[['stats',i]][['N']] <- length(summary(ccl[['results',i]])[['residuals']])
            if(R2) ccl[['stats',i]] <-
                c(ccl[['stats',i]],R2=summary(ccl[['results',i]])[['r.squared']])
            if(N) ccl[['stats',i]] <-
                c(ccl[['stats',i]],N=length(summary(ccl[['results',i]])[['residuals']]))
        }
        stats.list <- c(stats.list,'R2','N')
        stats.numbers <- 
            array(NA,dim=c(length(stats.list),length(model.names)),dimnames=list(stats.list,model.names)) 
        for(i in stats.list){
            for(j in model.names){
                if(!is.null(ccl[['stats',j]][[i]])) {
                    stat <- ccl[['stats',j]][[i]]
                    if(file.type == 'tex' & unstack.cells) {
                        stat <-
                            str_pad(paste('\\multicolumn{',num.multicol,'}{c}{',round(stat,round.dec),'}',sep=''),
                                    padding,side='both')
                        stats.numbers[i,j] <- stat
                    }
                    else{
                        stats.numbers[i,j] <- str_pad(round(ccl[['stats',j]][[i]],round.dec),padding,side='both')
                    }
                }
                else{
                    stat <- ''
                    if(file.type == 'tex' & unstack.cells){
                        stat <-
                            str_pad(paste('\\multicolumn{',num.multicol,'}{c}{ }',sep=''),
                                    padding,side='both')
                    }
                    stats.numbers[i,j] <- str_pad(stat,padding,side="both")        
                }
            }
        }
        #if(file.type == 'tex'){
            #if(unstack.cells){
                #ms2 <-
                    #str_pad(paste('\\multicolumn{',num.multicol,'}{c}{',stats.numbers,'}',sep=''), padding,side='both')
                #model.name.string  <-
                    #paste(str_pad(c('',ms2),col.width),collapse='&')
            #}
        #}

        # catching use if empty ccl
        if(is.matrix(ccl)){}else{return("No values stored. I think you need to store some models first.")}

        # setting dcolumn = NULL
        if(is.null(dcolumn)){dcolumn <- "c"}else{dcolumn <- dcolumn}

        # setting tablepos if non-NULL
        if(is.null(table.pos)){}else{table.pos <- paste("[",table.pos,"]",sep="")}

        # setting caption for TeX
        if(is.null(caption)){texcaption <- caption}else{texcaption <- paste("\\caption{",caption,"}\n",sep="")}

        # setting label if non-NULL
        if(is.null(label)){}else{label <- paste("\\label{tab:",label,"}\n",sep="")}

        # converting sub.sections vector to list of type list[i] = c(position,text)
        sub.sections.list <- NULL
        if(is.null(sub.sections) == FALSE){
            if((length(sub.sections)%%2) == 0){
                sub.sections.list  <- list() 
                for(i in 1:(length(sub.sections)/2)){
                    sub.sections.list[[i]]  <- c(sub.sections[[2*i-1]], sub.sections[[2*i]])
                }
            }
            else{
                return(cat("Your 'sub.section' vector contains a mistake. Please check and run again."))
            }
        }

        # converting var.names vector to list of type list[i] = c(name.old,name.new)
        #var.rename.list <- NULL

        # for CSV
        if(file.type == 'csv' ){
            delimiter <- ","
            R2 <- "R^2"
            aR2 <- "adj.R^2"
            N <- "N"
            om.end <- "\n"
            caption <- paste(caption,om.end,sep="")
            threestar <- paste(sig.sym[[3]],sep="")
            twostar <- paste(sig.sym[[2]],sep="")
            onestar <- paste(sig.sym[[1]],sep="")
            line.end <- '\n'
            na.string <- ' '
        }
        # for TeX
        else if(file.type == 'txt' ) {
            na.string <- str_pad('',col.width)
            delimiter <- " "
            R2 <- "R^2"
            aR2 <- "adj.R^2"
            N <- "N"
            threestar <- paste(sig.sym[[3]],sep="")
            twostar <- paste(sig.sym[[2]],sep="")
            onestar <- paste(sig.sym[[1]],sep="")
            line.end <- '\n'
            caption <- paste(caption,line.end,sep="")
        }
        else{
            na.string <- str_pad('',col.width)
            save.file <- paste(filename,".tex",sep="")
            delimiter <- "&"
            R2 <- "$R^2$"
            aR2 <- "$adj.R^2$"
            N <- "$N$"
            om.end <- "\\\\\n"
            caption <- caption
            threestar <- paste("\\sym{",sig.sym[[3]],"}",sep="")
            twostar <- paste("\\sym{",sig.sym[[2]],"}",sep="")
            onestar <- paste("\\sym{",sig.sym[[1]],"}",sep="")
            line.end <- '\\\\\n'
        }
        ######### creating a vector of variable names ##########################
        if(is.null(var.order) == FALSE){
            var.list <- var.order
        }
        var.count <- length(var.list)
        #cat(var.count)
        #cat("\n") #control
        #cat(var_list)
        #cat("\n")  #control
        #########################################################################

        #########################################################################
        # Begin Jeff's Comments:
        # It looks like end.sep.line is where the line is drawn.  What I want to do
        # is make something that defaults to this if stats is null, and if it's not
        # null will put all the various stats in there.  The key thing is to put a
        # stat taht only applies to some models.
        #########################################################################
        ########## making stars and putting in matrix #### stars depend on p-values ##########
        #table.rows <- matrix(rbind(var.list,matrix(paste(var.list,'aux',sep=''),1,length(var.list))),2*length(var.list),1)
        # This code creates a vertical matrix of "RT", "", "income", etc...
        # interpsersing blanks with the variable names

        # Now, this is going to only print the coefficients portion of the table
        body_numbers <-
            array(NA,dim=c(length(var.list),length(model.names),
                           length(cell.names)),dimnames=list(var.list,model.names,cell.names)) 
        indicator.strings <-
            matrix(NA,length(names(indicate)),length(model.names),dimnames=list(names(indicate),model.names))

        for (j in model.names){ 
            for(k in var.list){
                aux <- paste(k,'aux',sep='')
                model.list <- ccl[[j]]
                col_length <- length(model.list)
                if(!is.na(coef.func(ccl[['results',j]])[k])){
                    #for (i in 1:(col_length-1)){
                    coefficient <- coef.func(ccl[['results',j]])[[k]]
                    se <- sqrt(diag(se.func(ccl[['results',j]])))[[k]]
                    tstat <- coefficient/se
                    p <- 2*pt(-1*abs(tstat),res1[['df.residual']])
                    if ( p < sig.levels[3] ) {
                        sigs <-
                            str_pad(paste(round(coefficient,round.dec),threestar,sep=""),single.column.padding,side="both") #coefficient
                        std_err <-
                            str_pad(paste("(",se,")",sep=""),single.column.padding,side="both")        #std.err
                        t_val <-
                            str_pad(paste("[",round(tstat,round.dec),"]",sep=""),single.column.padding,side="both")          #t-value
                        p_val <-
                            str_pad(paste("[",round(p,round.dec),"]",sep=""),single.column.padding,side="both")          #p-value
                    }
                    else if( p < sig.levels[2] ) {
                        sigs <-
                            str_pad(paste(round(coefficient,round.dec),twostar,sep=""),single.column.padding,side="both")
                        std_err <-
                            str_pad(paste("(",round(se,round.dec),")",sep=""),single.column.padding,side="both")
                        t_val <-
                            str_pad(paste("[",round(tstat,round.dec),"]",sep=""),single.column.padding,side="both")
                        p_val <-
                            str_pad(paste("[",round(p,round.dec),"]",sep=""),single.column.padding,side="both")          #p-value
                    }
                    else if( p < sig.levels[1] ) {
                        sigs <-
                            str_pad(paste(round(coefficient,round.dec),onestar,sep=""),single.column.padding,side="both")
                        std_err <-
                            str_pad(paste("(",round(se,round.dec),")",sep=""),single.column.padding,side="both")
                        t_val <-
                            str_pad(paste("[",round(tstat,round.dec),"]",sep=""),single.column.padding,side="both")
                        p_val <-
                            str_pad(paste("[",round(p,round.dec),"]",sep=""),single.column.padding,side="both")          #p-value
                    }
                    else{
                        sigs <-
                            str_pad(paste(round(coefficient,round.dec),"",sep=""),single.column.padding,side="both")
                        std_err <-
                            str_pad(paste("(",round(se,round.dec),")",sep=""),single.column.padding,side="both")
                        t_val <-
                            str_pad(paste("[",round(tstat,round.dec),"]",sep=""),single.column.padding,side="both")
                        p_val <-
                            str_pad(paste("[",round(p,round.dec),"]",sep=""),single.column.padding,side="both")          #p-value
                    }
                    #k <- 1
                    #while(var.list[k] != coef.info[[1]]){
                    #k <- k +1
                    #}
                    #body_matrix[k*2,j+1] <- sigs #entry of coefficients
                    if(beta.value) body_numbers[k,j,'beta'] <- sigs
                    if(t.value) body_numbers[k,j,'t'] <- t_val
                    if(p.value) body_numbers[k,j,'p'] <- p_val
                    if(se.value) body_numbers[k,j,'se'] <- std_err
                    #} # end for (i)

                    # rename dep.var
                    #dep.var <- deparse(ccl[[j]][[col_length]][[1]])
                    #if(is.null(var.rename) == FALSE){
                        #for(i in 1:length(var.rename.list)){
                            #if(dep.var == var.rename.list[[i]][[1]]){
                                #dep.var <- var.rename.list[[i]][[2]]
                            #}
                        #}
                    #}
                }else{
                    # If there isn't a coefficient for this row, this is set to a
                    # blank string
                    for(i in cell.names){
                        body_numbers[k,j,i] <-
                            str_pad('',single.column.padding,side="both")        
                    }
                }
            }
            
            if(!is.null(indicate)){
                for(i in names(indicate)){
                    d <- grepl(paste('^',indicate[[i]],'$',sep=''),names(coef(ccl[['results',j]])))
                    if(any(d)) {
                        if(file.type == 'tex' & unstack.cells)
                            indicator.strings[[i,j]]  <-
                                str_pad(paste("\\multicolumn{",num.multicol,'}{c}{',indicator.yes,'}',sep=''),padding,side='both')
                        else indicator.strings[[i,j]] = str_pad(indicator.yes,
                                                                padding,side='both')
                        if(file.type == 'csv'){

                        }
                    } else{
                        if(file.type == 'tex' & unstack.cells){
                            indicator.strings[[i,j]]  <-
                                str_pad(paste("\\multicolumn{",num.multicol,'}{c}{',indicator.no,'}',sep=''),padding,side='both')
                        }
                        else{
                            indicator.strings[[i,j]] = str_pad(indicator.no,
                                                               padding,side='both')
                        }
                    }
                }
            } # end for model (j)
        }
        table.rows <- matrix(rbind(var.list,matrix(paste(var.list,'aux',sep=''),1,length(var.list))),2*length(var.list),1)
        #o2 <-
        #matrix(NA,length(cell.names)*length(var.list),length(model.names),dimnames=list(table.rows,model.names))
        #print(var.list)
        #print(model.names)
        if(unstack.cells) {
            #TODO: create the labels for this shape matrix, which basically means
            #putting some blanks in with the model names, analogous to if unstack is
            #fals
            #print(body_numbers)
            #print(aperm(body_numbers,c(1,3,2)))
            renamed.var.list <- var.list
            for(i in names(var.rename)) renamed.var.list[var.list == i] <- var.rename[[i]]
            renamed.var.list <-
                unlist(lapply(renamed.var.list,str_pad,width=single.column.padding,side='right'))
            print(renamed.var.list)
            nm <-
                matrix(rbind(model.names,matrix(NA,length(cell.names)-1,length(model.names))),1,length(cell.names)*length(model.names))
            #model.names <-
            print(renamed.var.list)
                #cbind(NA,matrix(rbind(model.names,matrix(NA,length(cell.names)-1,num.models)),1,length(cell.names)*num.models))
            print(model.names)
            #model.names <-
                #write.table(model.names,sep=',',row.names=FALSE,
                            #col.names=FALSE,na='')
            #print(model.names)
            table.rows <- renamed.var.list
            body_strings <-
                array(aperm(body_numbers,c(1,3,2)),c(length(renamed.var.list),length(model.names)*length(cell.names),1),dimnames=list(renamed.var.list,nm)) 
            print(body_strings)
        }
        else {
            # Combine body_numbers table into groups of rows for each coefficient
            blanks <- matrix(str_pad('',single.column.padding),length(cell.names)-1,length(unlist(var.list)))
            # TODO: change '' to NA here and see if I can avoid duplicate
            # row names
            # Create a matrix of blank strings that will be the row names for the t
            # stats, standard errors, etc
            renamed.var.list <- var.list
            for(i in names(var.rename)) renamed.var.list[var.list == i] <- var.rename[[i]]
            renamed.var.list <-
                unlist(lapply(renamed.var.list,str_pad,width=padding,side='right'))
            #s <- rbind(blanks,renamed.var.list)
            #print(s)
            #table.rows <-
                #matrix(rbind(unlist(renamed.var.list),blanks),length(unlist(renamed.var.list))*length(cell.names),1)
            table.rows <-
                matrix(rbind(renamed.var.list,blanks),length(unlist(renamed.var.list))*length(cell.names),1)
            # Combine the list of variable names with the blanks to make the row names
            s <- aperm(body_numbers,c(3,1,2),resize=TRUE)
            body_strings <-
                array(s,c(length(cell.names)*length(renamed.var.list),length(model.names),1),dimnames=list(table.rows,model.names))
        }
        #rownames(body_numbers) <- sub('.*aux *$','',rownames(body_numbers))
        print('made it here')
        #print(table.rows)
        #body.string <- write.table(body_strings,sep=delimiter,eol='\\\\\n', na =
        #str_pad('',col.width), row.names = rn, quote = FALSE)

        # setting TeX commands for booktabs
        if(file.type == 'tex'){
            if(booktabs == TRUE){
                toprule <- "\\toprule\n"
                midrule <- "\\midrule"
                bottomrule <- "\\bottomrule\n"
            } else{
                toprule <- "\\hline\\hline\n"
                midrule <- "\\hline"
                bottomrule <- "\\hline\\hline\n"
            }
        } 
        if(file.type == 'csv'){
            toprule <- ''
            midrule <- ''
            bottomrule <- ''
        }
        if(file.type == 'txt'){
            toprule <- ''
            if(unstack.cells) {
                midrule <- rep('_',padding*(num.models+1)*length(cell.names))
                print(length(midrule))
            }
            else {
                midrule <- rep('_',padding*(num.models+1))
            }
            #print(length(num.models))
            #print(col.width)
            #print(midrule)
        }

        #if(file.type == 'tex')
        # Everything here is a 'write.table' command except for the rule
        # lines
        currentWarning <- getOption('warn')
        options(warn=-1)
        cat(toprule,file=filename)
        write.table(header.string,file=filename, append=TRUE,sep=delimiter,
                    eol=line.end, row.names = FALSE, col.names = FALSE,
                    quote = FALSE, na=na.string)
        write.table(model.name.string,file=filename, append=TRUE,sep=delimiter,
                    eol=line.end, row.names = FALSE, col.names = FALSE,
                    quote = FALSE, na=na.string)
        cat(midrule,file=filename, sep='',append=TRUE, fill=TRUE)
        write.table(body_strings,sep=delimiter,eol=line.end, na =
                    na.string, col.names = FALSE, row.names =
        table.rows, quote =
        FALSE,  append=TRUE, file=filename)
        #body.string <- write.table(body_strings,sep=delimiter,eol='\\\\\n', na =
        #str_pad('',col.width), row.names = rn, quote = FALSE)
        if(!is.null(indicate)){
            write.table(indicator.strings,sep=delimiter,col.names = FALSE, quote
                        = FALSE, file=filename,append=TRUE, row.names =
            str_pad(dimnames(indicator.strings)[[1]],single.column.padding,side='right'),eol=line.end)
        }
        cat(midrule,file=filename, append=TRUE, sep='', fill=TRUE)
        #cat(stats.string)
        write.table(stats.numbers,sep=delimiter,eol=line.end, na =
                    na.string, row.names =
        str_pad(dimnames(stats.numbers)[[1]],single.column.padding,side='right'), col.names = FALSE, quote = FALSE,file=filename, append=TRUE)
        if(file.type=='tex') cat(bottomrule,file=filename, append=TRUE)
        options(warn=currentWarning)
        #print(header.string[[1]])
        #print(model.name.string[[1]])
        #write(cat(header.string,col.headers,body_strings,stats.string),file='temp.tex')
    } 
