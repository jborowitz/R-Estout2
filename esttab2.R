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
                padding <- col.width * length(cell.names)
                num.multicol <- length(cell.names)
            }
            else{
                padding <- col.width
                num.multicol <- 1
            }
            model.num.string  <- str_pad(paste("\\multicolumn{",num.multicol,'}{c}{',ms1,'}',sep=''),padding,side='both')
            #enclose the numbers in  the multicolumn command, spanning the number of
            #columns necessary if the results are unstacked
            #write.table(str_pad(c('',unlist(model.num.string)),col.width),sep='&',quote=FALSE,file='test.tex')
            header.string <-
                paste(paste(str_pad(c('',model.num.string),col.width),collapse='&'),'\\\\\n',sep='')
            #write.table(header.string, file='test.tex',quote=FALSE)
            #Concatenate the headers into one string, separated by & and ended with \\
            ms2 <- str_pad(paste('\\multicolumn{',num.multicol,'}{c}{',model.names,'}',sep=''), padding,side='both')
            model.name.string  <-
                paste(paste(str_pad(c('',ms2),col.width),collapse='&'),'\\\\\n',sep='')
            #write.table(model.name.string, file='test.tex',quote=FALSE,append=TRUE)
        }
        if(file.type == 'csv'){
            #blanks <- rep('',length(cell.names)-1)
            #header.string <- paste(str_pad(paste('',ms1
            #TODO: create csv headers
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
                if(!is.null(ccl[['stats',j]][[i]])) stats.numbers[i,j] <-
                    str_pad(round(ccl[['stats',j]][[i]],round.dec),col.width,side='both')
                else{
                    stats.numbers[i,j] <- str_pad('',col.width,side="both")        
                }

            }
        }

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
            save.file <- paste(filename,".csv",sep="")
            delimiter <- ","
            R2 <- "R^2"
            aR2 <- "adj.R^2"
            N <- "N"
            om.end <- "\n"
            caption <- paste(caption,om.end,sep="")
            threestar <- paste(sig.sym[[3]],sep="")
            twostar <- paste(sig.sym[[2]],sep="")
            onestar <- paste(sig.sym[[1]],sep="")
        }
        # for TeX
        else if(file.type == 'txt' ) {
            save.file <- paste(filename,".csv",sep="")
            delimiter <- " "
            R2 <- "R^2"
            aR2 <- "adj.R^2"
            N <- "N"
            om.end <- "\n"
            caption <- paste(caption,om.end,sep="")
            threestar <- paste(sig.sym[[3]],sep="")
            twostar <- paste(sig.sym[[2]],sep="")
            onestar <- paste(sig.sym[[1]],sep="")

        }
        else{
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
        table.rows <- matrix(rbind(var.list,matrix(paste(var.list,'aux',sep=''),1,length(var.list))),2*length(var.list),1)
        # This code creates a vertical matrix of "RT", "", "income", etc...
        # interpsersing blanks with the variable names

        # Now, this is going to only print the coefficients portion of the table
        body_matrix <- matrix()
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
                            str_pad(paste(round(coefficient,round.dec),threestar,sep=""),col.width,side="both") #coefficient
                        std_err <-
                            str_pad(paste("(",se,")",sep=""),col.width,side="both")        #std.err
                        t_val <-
                            str_pad(paste("[",round(tstat,round.dec),"]",sep=""),col.width,side="both")          #t-value
                        p_val <-
                            str_pad(paste("[",round(p,round.dec),"]",sep=""),col.width,side="both")          #p-value
                    }
                    else if( p < sig.levels[2] ) {
                        sigs <-
                            str_pad(paste(round(coefficient,round.dec),twostar,sep=""),col.width,side="both")
                        std_err <-
                            str_pad(paste("(",round(se,round.dec),")",sep=""),col.width,side="both")
                        t_val <-
                            str_pad(paste("[",round(tstat,round.dec),"]",sep=""),col.width,side="both")
                        p_val <-
                            str_pad(paste("[",round(p,round.dec),"]",sep=""),col.width,side="both")          #p-value
                    }
                    else if( p < sig.levels[1] ) {
                        sigs <-
                            str_pad(paste(round(coefficient,round.dec),onestar,sep=""),col.width,side="both")
                        std_err <-
                            str_pad(paste("(",round(se,round.dec),")",sep=""),col.width,side="both")
                        t_val <-
                            str_pad(paste("[",round(tstat,round.dec),"]",sep=""),col.width,side="both")
                        p_val <-
                            str_pad(paste("[",round(p,round.dec),"]",sep=""),col.width,side="both")          #p-value
                    }
                    else{
                        sigs <-
                            str_pad(paste(round(coefficient,round.dec),"",sep=""),col.width,side="both")
                        std_err <-
                            str_pad(paste("(",round(se,round.dec),")",sep=""),col.width,side="both")
                        t_val <-
                            str_pad(paste("[",round(tstat,round.dec),"]",sep=""),col.width,side="both")
                        p_val <-
                            str_pad(paste("[",round(p,round.dec),"]",sep=""),col.width,side="both")          #p-value
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
                        body_numbers[k,j,i] <- str_pad('',col.width,side="both")        
                    }
                }
            }
            
            if(!is.null(indicate)){
                for(i in names(indicate)){
                    s <- paste('^',indicate[[i]],'$',sep='')
                    d <- grepl(s,names(coef(ccl[['results',j]])))
                    if(any(d)) {
                        indicator.strings[[i,j]] = str_pad(indicator.yes,
                                                           col.width,side='both')
                    } else{
                        indicator.strings[[i,j]] = str_pad(indicator.no,
                                                           col.width,side='both')
                    }
                }
            } # end for model (j)
        }
        #table.rows <- matrix(rbind(var.list,matrix(paste(var.list,'aux',sep=''),1,length(var.list))),2*length(var.list),1)
        #o2 <-
        #matrix(NA,length(cell.names)*length(var.list),length(model.names),dimnames=list(table.rows,model.names))
        if(unstack.cells) {
            #TODO: create the labels for this shape matrix, which basically means
            #putting some blanks in with the model names, analogous to if unstack is
            #fals
            body_strings <- array(aperm(body_numbers,c(1,3,2)),c(length(var.list),length(cell.names)*length(model.names)),dimnames=list(table.rows,model.names))
        }
        else {
            # Combine body_numbers table into groups of rows for each coefficient
            blanks <- matrix('',length(cell.names)-1,length(unlist(var.list)))
            # TODO: change '' to NA here and see if I can avoid duplicate
            # row names
            # Create a matrix of blank strings that will be the row names for the t
            # stats, standard errors, etc
            renamed.var.list <- var.list
            for(i in names(var.rename)) renamed.var.list[var.list == i] <- var.rename[[i]]
            table.rows <-
                matrix(rbind(unlist(renamed.var.list),blanks),length(unlist(renamed.var.list))*length(cell.names),1)
            # Combine the list of variable names with the blanks to make the row names
            body_strings <-
                array(aperm(body_numbers,c(3,1,2),resize=TRUE),c(length(cell.names)*length(var.list),length(model.names),1),dimnames=list(table.rows,model.names))
        }
        rownames(body_numbers) <- sub('.*aux *$','',rownames(body_numbers))
        rn <- unlist(lapply(table.rows,str_pad,width=col.width,side='right'))
        #body.string <- write.table(body_strings,sep=delimiter,eol='\\\\\n', na =
        #str_pad('',col.width), row.names = rn, quote = FALSE)
        #print(rn)

        # setting TeX commands for booktabs
        if(file.type == 'tex'){if(booktabs == TRUE){
            toprule <- "\\toprule\n"
            midrule <- "\\midrule\n"
            bottomrule <- "\\bottomrule\n"
        }
        else{
            toprule <- "\\hline\\hline\n"
            midrule <- "\\hline\n"
            bottomrule <- "\\hline\\hline\n"
        }
        }else{
            midrule <- cat(rep('_',col.width*(length(num.models)+1) ,
                               sep='',fill=TRUE))
        }

        #print(dimnames(indicator.strings))
        print(body_strings)
        cat(toprule,file='test.tex')
        cat(header.string,file='test.tex', append=TRUE)
        cat(model.name.string,file='test.tex', append=TRUE)
        cat(midrule,file='test.tex', append=TRUE)
        write.table(body_strings,sep=delimiter,eol='\\\\\n', na =
                    str_pad('',col.width), col.names = FALSE,row.names = rn, quote =
        FALSE,  append=TRUE, file='test.tex')
        #body.string <- write.table(body_strings,sep=delimiter,eol='\\\\\n', na =
        #str_pad('',col.width), row.names = rn, quote = FALSE)
        if(!is.null(indicate)){
            write.table(indicator.strings,sep='&',col.names = FALSE, quote
                        = FALSE, file='test.tex',append=TRUE, row.names =
            str_pad(dimnames(indicator.strings)[[1]],col.width,side='right'),eol='\\\\\n')
        }
        cat(midrule,file='test.tex', append=TRUE)
        #cat(stats.string)
        write.table(stats.numbers,sep='&',eol='\\\\\n', na =
                    str_pad('',col.width), row.names =
        str_pad(dimnames(stats.numbers)[[1]],col.width,side='right'), col.names = FALSE, quote = FALSE,file='test.tex', append=TRUE)
        if(file.type=='tex') cat(bottomrule,file='test.tex', append=TRUE)
        #print(header.string[[1]])
        #print(model.name.string[[1]])
        #write(cat(header.string,col.headers,body_strings,stats.string),file='temp.tex')
    } 
