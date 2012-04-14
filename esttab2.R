`esttab2` <-
    function(beta.value=TRUE, se.value=TRUE, t.value=FALSE, p.value=FALSE,
             round.dec=3, sig.levels=c(0.05, 0.01, 0.001), sig.sym=c("*",
                                                                     "**",
                                                                     "***"),
             filename=NULL, file.type='tex', dcolumn=NULL, table="table",
             booktabs=FALSE, var.rename=NULL, keep=NULL, se.func=vcov,
             coef.func=coef, col.width=24, col.headers=NULL,
             unstack.cells=FALSE,N=TRUE,R2=TRUE, indicate=NULL, drop=NULL,
             headlines=FALSE, footlines=FALSE, tableName='ccl'){
        library('stringr')

        # coef.func and se.fun are function names that can be passed in order
        # to report marginal effects (coef.func) or different standard errors
        # (se.func).  These functions need to work with
        # names(coef.func(results))
        # headlines and footlines are whether to have double \hlines as the
        # first and second line of the table
        # filename: the name of the file where the output will be saved.  The
        # extension to this must be 'tex', 'csv', or 'txt', which will determine
        # which type of output file is saved.
        # col.headers will override using the dependent variable name 
        # unstack.cells will put each auxiliary element in a separate column
        # N: report the number of observations in each model
        # R2: report the R2 of the model.  If this cannot be accessed through
        # the 'r.squared' attribute of the summary of the results, this will
        # cause an error
        # indicate: takes a list('ageyoungest*'='Age of Youngest') type
        # argument
        # var.rename is a list like: list('old name'='new name', 'another
        # old name' = 'another old name')
        # Note that if you keep something that is absorbed into an
        # 'indicator', it will be ignored.

        # This table is written in 5 distinct parts.  There is a head line,
        # which has model numbers in columns and is built in header.string.
        # Then, there is a model titles line, which is in the
        # model.name.string.  Next, comes the body of main results and
        # auxiliary results, either stacked or not.  This is built in
        # body.string.  After that comes the indicator.string, which marks
        # with an X or not whether each model has a collection of
        # variables.  Finally comes the stats.string, which contains
        # auxiliary stats and or N and R2 for each model.

        # From a technical standpoint, the ccl object contains the stats
        # field, which is a list of statistic=value; and a results object,
        # which contains the results of fitting the model.  Currently, key
        # things that are necessary for the results object are that either
        # it returns a variance-covariance matrix with the command
        # vcov(result) OR a function is provided as se.func which provides
        # this vcov matrix; and that the summary object of the results has
        # r.squared and residuals fields, and that the coef function or
        # whatever is specified for coef.func) has a names field for the
        # variable names.  

        # The factor that drives most of the complexity in this function is
        # the ability to 'unstack' the results.  For every coefficient for
        # every model, there is likely another piece of information that
        # you want to display, such as a standard error, p-value, or
        # t-stat.  You can display any or all of these statistics.  In
        # future versions, I hope to expand this to allow for multiple
        # arbitrary functions for each coefficient, so that the marginal
        # effect could be displayed along-side the coefficient, etc.
        # Currently, marginal effects could be implemented by using a
        # function that has returns a $names field that is passed to the
        # coef.func option.  In any case, if these 'cells' for each
        # coefficient are unstacked, they are displayed horizontally across
        # the cell, while if they are not unstacked they are displayed
        # vertically, one on each line.

        # Some Variable Definitions:
        # cell.names: this is a list of the names of the cells that are
        # actually included in the output table.
        # num.models - the number of models in the ccl object
        # var.list - the initial variable names from all models together
        # renamed.var.list - The variable names updated by inputs and
        # decreased by whtaever goes into the indicator section
        # model.list - A vector of the model names
        # padding - the text width of an unstacked column for the unstacked
        # table
        # single.column.padding - the text width of a stacked column
        # num.multicol - the number of cells, which is now big the latex
        # \multicol command needs to be in order to have a valid table
        # model.name.string - The string of model names to be printed in a
        # table
        # header.string - The string with the model numbers for a table
        # body_numbers: a mis-named matrix that contains the un-padded strings for
            #the body of the table, the model coefficients
        # body.string - The string matrix with the coefficients for the
        # main part of the table
        # indicator.string - The string matrix with the indicator.yes and
        # indicator.no strings in the appropriate places indicating whether
        # any of sets of variables are present
        # stats.string - The matrix of strings of auxiliary statistics for
        # the table

        to.include <- c(beta.value,se.value,t.value,p.value)
        cell.names <- c('beta','se','t','p')[to.include]
        # Create a vector that contains the names of the analysis elements
        # to include.
        fileparts <- strsplit(filename,'\\.')
        file.type <- fileparts[[1]][[2]]
        # Determine the file type from the output filename
        supported.file.types <- c('tex','txt','csv')
        if(!any(file.type == supported.file.types)) {
            warning('\'filename\' extension does not match supported file type.  TeX
                    file chosen as default')
            file.type <- 'tex'
        }

        indicator.yes <- 'X'
        indicator.no <- ' '
        for(i in names(indicate)) {
            new.name <- gsub('\\s+',' ',i)
            if(new.name != i){
                old.val <- indicate[[i]]
                indicate[[i]] <- NULL
                indicate[[new.name]] <- old.val
            }
        }
        for(i in names(var.rename)) var.rename[[i]] <-
            gsub('\\s+',' ',var.rename[[i]])
        # Sanitize inputs for var.rename and indicate.  TODO: try to find a
        # way to keep the sorting of the indicate variables.  I think the
        # current method might get rid of sorting.
        ccl <<- eval(as.name(tableName))
        # Grab the global results element.
        num.models <- length(ccl[1,])
        model.names <- dimnames(ccl)[[2]]
        var.list <- NULL
        for(i in model.names) var.list<- union(var.list,names(coef.func(ccl[['results',i]])))
        # For each model, add the list of independent variables to the
        # overall list through the 'union' command.
        indicator.list <- NULL
        for(i in names(indicate)){
            matches <- grepl(paste('^',indicate[[i]],'$',sep=''),var.list)
            var.list <- var.list[!matches]
            if(any(matches)) {
                indicator.list<-union(indicator.list,i)
            }
        }
        # Look for indicator matches.  If you find them, remove them from
        # the variable list and add the overall indicator to the indicator
        # list

        # The variable list is the union of all variables stored
        if(!is.null(keep)) var.list<- intersect(var.list,keep)
        var.list <- unlist(var.list)
        # only keep those variable names in the keep list: use the intersection to
        # find this list
        if(!is.null(drop)) var.list<- setdiff(var.list,drop)

        renamed.var.list <- var.list
        for(i in names(var.rename)) renamed.var.list[var.list == i] <- var.rename[[i]]
        # Create 'renamed.var.list', which applies the renaming
        # specified in the var.rename option

        if(!is.null(col.headers) & length(col.headers) == length(dimnames(ccl)[[2]])) {
            model.labels <- col.headers
        } else{
            model.labels <- model.names
        }
        # Set the column headers to the specified values if entered
        # Everything here is a 'write.table' command except for the rule
        # lines.  This happens at the end so that redundant columns headers
        # can be allowed. 

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
            ms2 <-
                str_pad(paste('\\multicolumn{',num.multicol,'}{c}{',model.labels,'}',sep=''), padding,side='both')
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
                    cbind(matrix(NA,1,1),matrix(rbind(model.labels,matrix(NA,length(cell.names)-1,num.models)),1,length(cell.names)*num.models))
                # Insert 'NA' elements in the column header for the extra
                # cells 
                header.string <-
                        cbind(matrix(NA,1,1),matrix(rbind(ms1,matrix(NA,length(cell.names)-1,num.models)),1,length(cell.names)*num.models))
            }else{
                padding <- col.width
                padding <- 0
                single.column.padding <- 0
                header.string <- matrix(c(NA,ms1),1, length(ms1)+1)
                model.name.string <- matrix(c(NA,model.labels),1,
                                            length(model.labels)+1)
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
            #enclose the numbers in  the multicolumn command, spanning the number of
            #columns necessary if the results are unstacked
            header.string <-
                paste(c(str_pad('',col.width,side='both'),model.num.string), collapse=' ')
            #Concatenate the headers into one string, separated by & and ended with \\
            ms2 <- str_pad(model.labels, padding,side='both')
            model.name.string  <-
                paste(str_pad(c('',ms2),col.width),collapse=' ')
        }

        # Work on stats:
        stats.list <- NULL
        for(i in model.names) {
            stats.list <- union(stats.list,names(ccl[['stats',i]]))
            if(R2) ccl[['stats',i]] <-
                c(ccl[['stats',i]],R2=summary(ccl[['results',i]])[['r.squared']])
            if(N) ccl[['stats',i]] <-
                c(ccl[['stats',i]],N=length(summary(ccl[['results',i]])[['residuals']]))
        } 
        stats.list <- c(stats.list,'R2','N')
        stats.string <- 
            array(NA,dim=c(length(stats.list),length(model.names)),dimnames=list(stats.list,model.names)) 
        # Build the list of stats (stats.list) by taking the union of all
        # stats reported for each model.  R2 and N are placed at the end if
            # specified by the R2 and N options
        for(i in stats.list){
            for(j in model.names){
                if(!is.null(ccl[['stats',j]][[i]])) {
                    stat <- ccl[['stats',j]][[i]]
                    if(file.type == 'tex' & unstack.cells) {
                        stat <-
                            str_pad(paste('\\multicolumn{',num.multicol,'}{c}{',round(stat,round.dec),'}',sep=''),
                                    padding,side='both')
                        stats.string[i,j] <- stat
                    }
                    else{
                        stats.string[i,j] <- str_pad(round(ccl[['stats',j]][[i]],round.dec),padding,side='both')
                    }
                }
                else{
                    stat <- ''
                    if(file.type == 'tex' & unstack.cells){
                        stat <-
                            str_pad(paste('\\multicolumn{',num.multicol,'}{c}{ }',sep=''),
                                    padding,side='both')
                    }
                    stats.string[i,j] <- str_pad(stat,padding,side="both")        
                }
            }
        }

        if(file.type == 'csv' & unstack.cells){
            # Unstack for a CSV, which means inserting blank cells in
            # the right spots.
            dummynames <- 1:(length(cell.names)*num.models)
            # These names are the column names for the indicator variables
            # with extra columns stuck in.  These column names are not
            # printed, so they just have to match the dimensions
            new.stats.string <-
                matrix(NA,length(stats.list),length(model.names)*length(cell.names),dimnames=list(stats.list,dummynames))
            # Initialize a bigger stats.string matrix that will be filled
            # with appropriately spaced blanks and elemtents of the old
            # matrix.  This code puts the stats in the left-most column
            # that pertains to a given model
            for(i in rownames(stats.string)){
                print(i)
                temp<-stats.string[i,]
                # take a single row of stats.string
                temp<-rbind(temp, matrix(NA, length(cell.names)-1,
                                         num.models))
                # Add blank rows to this row
                temp <- matrix(temp,1,length(cell.names)*num.models)
                # Reshape this matrix to a row vector
                new.stats.string[i,] <- temp
                # Save the row vector
            }
            stats.string <- new.stats.string
        }

        ## catching use if empty ccl
        #if(is.matrix(ccl)){}else{return("No values stored. I think you need to store some models first.")}

        ## setting dcolumn = NULL
        #if(is.null(dcolumn)){dcolumn <- "c"}else{dcolumn <- dcolumn}

        ## setting tablepos if non-NULL
        #if(is.null(table.pos)){}else{table.pos <- paste("[",table.pos,"]",sep="")}

        ## setting caption for TeX
        #if(is.null(caption)){texcaption <- caption}else{texcaption <- paste("\\caption{",caption,"}\n",sep="")}

        ## setting label if non-NULL
        #if(is.null(label)){}else{label <- paste("\\label{tab:",label,"}\n",sep="")}

        ## converting sub.sections vector to list of type list[i] = c(position,text)
        #sub.sections.list <- NULL
        #if(is.null(sub.sections) == FALSE){
            #if((length(sub.sections)%%2) == 0){
                #sub.sections.list  <- list() 
                #for(i in 1:(length(sub.sections)/2)){
                    #sub.sections.list[[i]]  <- c(sub.sections[[2*i-1]], sub.sections[[2*i]])
                #}
            #}
            #else{
                #return(cat("Your 'sub.section' vector contains a mistake. Please check and run again."))
            #}
        #}
        # The above commented code came from the original esttab.R, by
        # Felix Kaminsky <fkamin@uni-goettingen.de>.  I have currently not
        # implemented multiple panels or captions.


        if(file.type == 'csv' ){
            delimiter <- ","
            R2 <- "R^2"
            aR2 <- "adj.R^2"
            N <- "N"
            om.end <- "\n"
            #caption <- paste(caption,om.end,sep="")
            threestar <- paste(sig.sym[[3]],sep="")
            twostar <- paste(sig.sym[[2]],sep="")
            onestar <- paste(sig.sym[[1]],sep="")
            line.end <- '\n'
            na.string <- ''
            quote.strings <- TRUE
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
            #caption <- paste(caption,line.end,sep="")
            quote.strings <- FALSE
        }
        else{# For txt
            na.string <- str_pad('',col.width)
            save.file <- paste(filename,".tex",sep="")
            delimiter <- "&"
            R2 <- "$R^2$"
            aR2 <- "$adj.R^2$"
            N <- "$N$"
            om.end <- "\\\\\n"
            #caption <- caption
            threestar <- paste("\\sym{",sig.sym[[3]],"}",sep="")
            twostar <- paste("\\sym{",sig.sym[[2]],"}",sep="")
            onestar <- paste("\\sym{",sig.sym[[1]],"}",sep="")
            line.end <- '\\\\\n'
            quote.strings <- FALSE
        }

        ########## making stars and putting in matrix #### stars depend on p-values ##########

        body_numbers <-
            array(NA,dim=c(length(var.list),length(model.names),
                           length(cell.names)),dimnames=list(var.list,model.names,cell.names)) 
        indicator.strings <-
            matrix(NA,length(names(indicate)),length(model.names),dimnames=list(names(indicate),model.names))

        for (j in model.names){ 
            variance.matrix <- se.func(ccl[['results',j]])
            for(k in var.list){
                model.list <- ccl[[j]]
                col_length <- length(model.list)
                if(!is.na(coef.func(ccl[['results',j]])[k])){
                    coefficient <- coef.func(ccl[['results',j]])[[k]]
                    se <- sqrt(diag(variance.matrix))[[k]]
                    tstat <- coefficient/se
                    #p <- 2*pt(-1*abs(tstat),res1[['df.residual']])
                    p <- 2*pt(-1*abs(tstat),df.residual(ccl[['results',j]]))
                    # Calculate the statistics for inclusion in the cells
                    if ( p < sig.levels[3] ) {
                        # Begin logic that stars significant coefficients
                        # appropriately
                        sigs <-
                            str_pad(paste(round(coefficient,round.dec),threestar,sep=""),single.column.padding,side="both") #coefficient
                        std_err <-
                            str_pad(paste("(",round(se,round.dec),")",sep=""),single.column.padding,side="both")        #std.err
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
                            str_pad(paste("[",round(p,round.dec),"]",sep=""),single.column.padding,side="both")          
                    }
                    else if( p < sig.levels[1] ) {
                        sigs <-
                            str_pad(paste(round(coefficient,round.dec),onestar,sep=""),single.column.padding,side="both")
                        std_err <-
                            str_pad(paste("(",round(se,round.dec),")",sep=""),single.column.padding,side="both")
                        t_val <-
                            str_pad(paste("[",round(tstat,round.dec),"]",sep=""),single.column.padding,side="both")
                        p_val <-
                            str_pad(paste("[",round(p,round.dec),"]",sep=""),single.column.padding,side="both")         
                    }
                    else{
                        sigs <-
                            str_pad(paste(round(coefficient,round.dec),"",sep=""),single.column.padding,side="both")
                        std_err <-
                            str_pad(paste("(",round(se,round.dec),")",sep=""),single.column.padding,side="both")
                        t_val <-
                            str_pad(paste("[",round(tstat,round.dec),"]",sep=""),single.column.padding,side="both")
                        p_val <-
                            str_pad(paste("[",round(p,round.dec),"]",sep=""),single.column.padding,side="both")          
                    }
                    if(beta.value) body_numbers[k,j,'beta'] <- sigs
                    if(t.value) body_numbers[k,j,'t'] <- t_val
                    if(p.value) body_numbers[k,j,'p'] <- p_val
                    if(se.value) body_numbers[k,j,'se'] <- std_err
                    # Set values determined above to the body_numbers
                    # matrix
                }else{
                    # If there isn't a coefficient for this variable for
                    # this model this is set to a blank string
                    for(i in cell.names){
                        body_numbers[k,j,i] <-
                            str_pad('',single.column.padding,side="both")        
                    }
                }
            }
            
            if(!is.null(indicate)){
                for(i in names(indicate)){
                    d <- grepl(paste('^',indicate[[i]],'$',sep=''),names(coef(ccl[['results',j]])))
                    # See if the regular expression matches any variables.
                    # The key thing is that it must match the ENTIRE line,
                    # so that e.g. 'college' doesn't match 'collegeyears'
                    if(any(d)) {
                        if(file.type == 'tex' & unstack.cells)
                            indicator.strings[[i,j]]  <-
                                str_pad(paste("\\multicolumn{",num.multicol,'}{c}{',indicator.yes,'}',sep=''),padding,side='both')
                        # make the unstacked multicolumn command for the
                        # indicator value
                        else indicator.strings[[i,j]] = str_pad(indicator.yes,
                                                               padding,side='both')
                        if(file.type == 'csv' & unstack.cells){

                        }
                    } else{
                        if(file.type == 'tex' & unstack.cells){
                            indicator.strings[[i,j]]  <-
                                str_pad(paste("\\multicolumn{",num.multicol,'}{c}{',indicator.no,'}',sep=''),padding,side='both')
                            # Generate the multicolumn command for
                            # unstacked cells when there is not a match
                        }
                        else{
                            indicator.strings[[i,j]] = str_pad(indicator.no,
                                                               padding,side='both')
                        }
                    }
                }
            } 
        }
        if(file.type == 'csv' & unstack.cells){
            dummynames <- 1:(length(cell.names)*num.models)
            # These names are the column names for the indicator variables
            # with extra columns stuck in.  These column names are not
            # printed, so they just have to match the dimensions

            new.indicator.strings <-
                matrix(NA,length(names(indicate)),length(model.names)*length(cell.names),dimnames=list(names(indicate),dummynames))
            # Initialize a bigger indicator strings matrix that will be
            # filled with appropriately spaced blanks and elemtents of the
            # old indicator matrix.  This code puts the indicator.yes or
            # indicator.no symbol in the left-most column that pertains to
            # a given model
            for(i in rownames(indicator.strings)){
                temp<-indicator.strings[i,]
                # take a single row of the indicator strings
                temp<-rbind(temp, matrix(NA,
                                                   length(cell.names)-1,
                                                   num.models))
                # Add blank rows to this row
                temp <- matrix(temp,1,length(cell.names)*num.models)
                # Reshape this matrix to a row vector
                new.indicator.strings[i,] <- temp
                # Save the row vector
            }
            indicator.strings <- new.indicator.strings
            #rownames(indicator.strings) <- names(indicate)
        }
        # End work on indicators, begin work on row names for variable list
        if(unstack.cells) {
            renamed.var.list <-
                unlist(lapply(renamed.var.list,str_pad,width=single.column.padding,side='right'))
            # Pad the variable list.  Note that this padds even the csv
            # variable list but for csvs the padding is 0
            nm <-
                matrix(rbind(model.names,matrix(NA,length(cell.names)-1,length(model.names))),1,length(cell.names)*length(model.names))
            # Combine the model names with enough blanks so that there are
            # enough headers.  This is actually unnecessary at htis point,
            # since I suppress column name printing for the body.  However
            # I am leaving it here because in the future this could be a
            # good way to put in cell names.
            table.rows <- renamed.var.list
            body.string <-
                array(aperm(body_numbers,c(1,3,2)),c(length(renamed.var.list),length(model.names)*length(cell.names),1),dimnames=list(renamed.var.list,nm)) 
            # Unstack the body.string.  The body.string was a 3-d array,
            # but I am now flatting it out to 2d, according to the unstack
            # command.
        }
        else {
            # Combine body_numbers table into groups of rows for each coefficient
            blanks <- matrix(str_pad('',single.column.padding),length(cell.names)-1,length(unlist(var.list)))
            # I wanted to change '' to NA here and see if I can avoid duplicate
            # row names.  Now I know that this doesn't work.  Blank row names are
            # file, but give a warning.  I have instead suppressed exactly
            # this warning when the matrix is written to the file.
            renamed.var.list <-
                unlist(lapply(renamed.var.list,str_pad,width=padding,side='right'))
            table.rows <-
                matrix(rbind(renamed.var.list,blanks),length(unlist(renamed.var.list))*length(cell.names),1)
            # Combine the list of variable names with the blanks to make
            # the row names for the stacked variables
            s <- aperm(body_numbers,c(3,1,2),resize=TRUE)
            body.string <-
                array(s,c(length(cell.names)*length(renamed.var.list),length(model.names),1),dimnames=list(table.rows,model.names))
        }

        # setting TeX commands for booktabs.  This hasn't been changed much
        # from the original code, and I'm not sure that it is doing much.
        # I have included blanks for csv tables and underscore lines for
        # txt files
        if(file.type == 'tex'){
            if(booktabs == TRUE){
                toprule <- "\\toprule\n"
                midrule <- "\\midrule"
                bottomrule <- "\\bottomrule\n"
            } else{
                if(headlines) toprule <- "\\hline\\hline\n"
                else toprule <- ''
                midrule <- "\\hline\n"
                if(footlines) bottomrule <- "\\hline\\hline\n"
                else bottomrule <- ''
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
                midrule <-
                    c(rep('_',padding*num.models +
                          single.column.padding),'\n')
                # Create an underline that is the appropriate length
            }
            else {
                midrule <- c(rep('_',padding*(num.models+1)),'\n')
            }
        }

        ################################################################################
        # Begin writing output
        cat(toprule,file=filename)
        write.table(header.string,file=filename, append=TRUE,sep=delimiter,
                    eol=line.end, row.names = FALSE, col.names = FALSE,
                    quote = quote.strings, na=na.string)
        write.table(model.name.string,file=filename, append=TRUE,sep=delimiter,
                    eol=line.end, row.names = FALSE, col.names = FALSE,
                    quote = quote.strings, na=na.string)
        cat(midrule,file=filename, sep='',append=TRUE)
        currentWarning <- getOption('warn')
        options(warn=-1)
        # I have blank row names for the body.string, which is the case
        # because if the cells are not unstacked, there are rows beneath
        # each coefficient for the other cell statistics without names.
        # estting warn to -1 turns off the warning about overlapping row
        # names.
        write.table(body.string,sep=delimiter,eol=line.end, na = na.string,
                    col.names = FALSE, row.names = table.rows, quote =
                    quote.strings,  append=TRUE, file=filename)
        options(warn=currentWarning)
        if(!is.null(indicate)){
            write.table(indicator.strings,sep=delimiter,col.names = FALSE, quote
                        = quote.strings, file=filename,append=TRUE, row.names =
            str_pad(dimnames(indicator.strings)[[1]],single.column.padding,side='right'),eol=line.end,
            na=na.string)
        }
        cat(midrule,file=filename, append=TRUE, sep='')
        write.table(stats.string,sep=delimiter,eol=line.end, na =
                    na.string, row.names =
        str_pad(dimnames(stats.string)[[1]],single.column.padding,side='right'),
        col.names = FALSE, quote = quote.strings,file=filename, append=TRUE)
        if(file.type=='tex') cat(bottomrule,file=filename, append=TRUE)
    } 
