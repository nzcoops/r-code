info.table <- function(data, vari, dpm=3, dps=3, std.dev=F, ci=T, total.col=T, factor.na=F, ci.limit=0.95, sub ,html, comp=F)
	{
	# Author: Matt Cooper
	# Version 0.936
	# Last updated: 20/01/12
  attach(data)
  on.exit(detach(data))	
  tot.col <- 1
  
# STOPPING FUNCTION IF SOMETHING WILL BREAK IT
  
  if(ci.limit<=0 | ci.limit >= 1) stop("The value you entered for ci.limit is either less than 0 or greater than 1, this value must be between (exclusive) 0 and 1, express percentages as proportions eg 95% = 0.95", call.=F)
  cip <- 1-(1-ci.limit)/2

  if(missing(sub) & total.col==T) stop("You have not provided a variable to subset on and the function is set to produce a total column. This will essentially duplicate the same results twice. The total column is produced by default, if you only want the table constructed for the entire data.frame please set total.col to 'F' (FALSE)" , call.=F)

  if(missing(sub) & comp==T) stop("Hmmm, so no subsetting variable provided and you want a comparison of something done... You've got me stumped. Try comp=F (the default) since I've got no groups to do a comparison between" , call.=F)
  
  if(missing(html) & !"xtable" %in% .packages(all.available=T)) stop("You've requested to output an html table, it appears you don't have the package 'xtable' installed. Please install it and try again", .call=F)

# ORDERING VARIABLES

  class.list <- vector()
  for(i in 1:length(vari)) {ccheck <- data[1,vari[i]]; class.list[i] <- class(ccheck)}

  if(any(!class.list %in% c("numeric","integer","factor"))) stop(paste("Please remove the following variable(s) as it is not of class numeric, integer or factor: ",vari[which(!class.list %in% c("numeric","integer","factor"))],". What did you think I could do with that?",sep=""), call.=F)

  vari.order <- vector
  if(any(class.list=="numeric") | any(class.list=="integer")) {numint.vari <-  which(class.list == "numeric" | class.list == "integer")}
  if(any(class.list=="factor")) {fac.vari <- which(class.list == "factor")}
  if(exists("numint.vari")) {numint.vari <- vari[numint.vari]; numint.vari <- sort(numint.vari, decreasing=F)} 
  if(exists("fac.vari")) {fac.vari <- vari[fac.vari]; fac.vari <- sort(fac.vari, decreasing=F)}
  if(exists("numint.vari")) {vari.order <-  numint.vari}
  if(exists("fac.vari") & exists("numint.vari")) {vari.order <- c(vari.order,fac.vari)}
  if(exists("fac.vari") & !exists("numint.vari")) {vari.order <- fac.vari}
  if(exists("numint.vari")){num.numint <- length(numint.vari)} else  {num.numint <- -1}
  vari <- vari.order

# IF THE USER HAS SUBGROUPS IN THEIR TABLE
  
  if(!missing(sub))
		{

    # WARNINGS PRESENTED TO THE USER
		
    if(any(is.na(sub))) warning("Not everybody had a value for the variable you chose to subset on, as a result those rows have been removed from the data.frame for the generation of this table", immediate.=T, call.=F)
    
    sub <- as.factor(sub)	
		data <- data[!is.na(sub),]
		sub <- sub[!is.na(sub)]
 
    if(length(levels(sub))>5) warning("Variable for subsetting contains more than 5 levels, this may cause low (meaningless) results in some cells", immediate.=T, call.=F)
    
    # SETTING UP THE RIGHT NUMBER OF COLUMNS, INITIAL COLUMN NAMES AND SUBHEADINGS
    
    if(comp==T & total.col==T)
			{
			info.table.out <- as.data.frame(matrix(ncol=(length(levels(sub))+3)))
      info.table.out[1,] <- c("Variable",if(std.dev==F){rep("Mean (CI) [n]",length(levels(sub))+1)}else{rep("Mean (SD) [n]",length(levels(sub))+1)},"P-Value")
      if(!exists("fac.vari")) {} else {if(exists("fac.vari") & num.numint!= -1){info.table.out[2+num.numint,] <- c("",rep("% [n]",length(levels(sub))+1)," ")} else{info.table.out[1,] <- c("Variable",rep("% [n]",length(levels(sub))+1),"P-Value")}}          
      names(info.table.out)[1:2] <- c(" ",paste("Total (n=",nrow(data),")",sep=""))
      tot.col <- 2  # If the total column is present everything needs to be shifting over 1 TBH
      
      } else if (comp==T & total.col==F){
			info.table.out <- as.data.frame(matrix(ncol=(length(levels(sub))+2)))
    	info.table.out[1,] <- c("Variable",if(std.dev==F){rep("Mean (CI) [n]",length(levels(sub)))}else{rep("Mean (SD) [n]",length(levels(sub)))},"P-Value")
    	if(!exists("fac.vari")) {} else {if(exists("fac.vari") & num.numint!= -1){info.table.out[2+num.numint,] <- c("",rep("% [n]",length(levels(sub)))," ")} else{info.table.out[1,] <- c("Variable",rep("% [n]",length(levels(sub))),"P-Value")}} 
      names(info.table.out)[1:2] <- c(" ",paste("Total (n=",nrow(data),")",sep=""))
			
      } else if (comp==F & total.col==T){
			info.table.out <- as.data.frame(matrix(ncol=(length(levels(sub))+2)))
      info.table.out[1,] <- c("Variable",if(std.dev==F){rep("Mean (CI) [n]",length(levels(sub))+1)}else{rep("Mean (SD) [n]",length(levels(sub))+1)})
      if(!exists("fac.vari")) {} else {if(exists("fac.vari") & num.numint!= -1){info.table.out[2+num.numint,] <- c("Variable",rep("% [n]",length(levels(sub))+1))} else{info.table.out[1,] <- c("Variable",rep("% [n]",length(levels(sub))+1))}} 
      names(info.table.out)[1:2] <- c(" ",paste("Total (n=",nrow(data),")",sep=""))
      tot.col <- 2  # If the total column is present everything needs to be shifting over 1 TBH
      } else { 
      info.table.out <- as.data.frame(matrix(ncol=(length(levels(sub))+1)))
    	info.table.out[1,] <- c("Variable",if(std.dev==F){rep("Mean (CI) [n]",length(levels(sub)))}else{rep("Mean (SD) [n]",length(levels(sub)))})
    	if(!exists("fac.vari")) {} else {if(exists("fac.vari") & num.numint!= -1){info.table.out[2+num.numint,] <- c("",rep("% [n]",length(levels(sub))))} else{info.table.out[1,] <- c("Variable",rep("% [n]",length(levels(sub))))}} 
      names(info.table.out)[1:2] <- c(" ",paste("Total (n=",nrow(data),")",sep=""))
      }
      
    # VALUES FOR THE TOTAL COLUMN
    if(total.col==T)
			{
      ins.row <- 2
      for(i in 1:length(vari))
				{
        temp <- data[,vari[i]]
        # NUMERIC
        if(class(temp)=="numeric" | class(temp)=="integer")
					{
					temp <- as.numeric(temp)
					m <- round(mean(temp,na.rm=T),dpm)
					se <- qnorm(cip)* ( sd(temp,na.rm=T)/sqrt(length(temp)) )
					sedp <- round(se, dps)
					sdp <- round(sd(temp,na.rm=T), dps)
					u <- round(m+se,dps)
					l <- round(m-se,dps)
					usd <- round(m+sdp,dps)
					lsd <- round(m-sdp,dps)
					info.table.out[ins.row,1] <- vari[i]
						if(std.dev==T & ci==F) {
						info.table.out[ins.row,2] <- noquote(paste(m," (",sdp,")"," [",length(temp[!is.na(temp)]),"]",sep=""))
						} else if(std.dev==T & ci==T) {
						info.table.out[ins.row,2] <- noquote(paste(m," (",lsd,",",usd,")"," [",length(temp[!is.na(temp)]),"]",sep=""))												
						} else if(std.dev==F & ci==F) {
            			info.table.out[ins.row,2] <- noquote(paste(m," (",sedp,")"," [",length(temp[!is.na(temp)]),"]",sep=""))
            			} else {
            			info.table.out[ins.row,2] <- noquote(paste(m," (",l,",",u,")"," [",length(temp[!is.na(temp)]),"]",sep=""))            			
            			}
            ins.row <- ins.row + 1
					} else
        # FACTOR					
				if(class(temp)=="factor" & vari[i]==fac.vari[1] & num.numint!= -1)
          {ins.row <- ins.row + 1}
        if(class(temp)=="factor")
					{
          for(j in 0:length(levels(temp)))
						{
						if(j==0)
						  {
						  info.table.out[ins.row,1] <- vari[i]
              info.table.out[ins.row,2] <- paste("n=",max(length(temp[!is.na(temp)]),1),sep="")
              ins.row <- ins.row + 1 
						  } else{
						  answer <- paste(round(length(which(temp %in% levels(temp)[j]))/max(length(temp[!is.na(temp)]),1)*100,dpm)," [",length(which(temp %in% levels(temp)[j])),"]",sep="")
              info.table.out[ins.row,2] <- answer
    					ins.row <- ins.row + 1
						  }
						}
						if(factor.na==T)
              {
              answer <- paste(sum(is.na(temp == levels(temp)[j])),sep="") 
		          info.table.out[ins.row,1] <- "Missing"					
						  info.table.out[ins.row,2] <- answer
						  ins.row <- ins.row + 1    
					    }
            }
         }
       }
    # END OF TOTAL COLUMN
		# START OF SUBSETTED COLUMNS
    ins.row <- 2
    for(k in 1:length(levels(sub)))
			{
			group <- levels(sub)[k]
			if(comp==T)
				{
				names(info.table.out)[k+tot.col] <- paste(group," (n=",length(which(sub %in% group)),")",sep="")
				} else{
				names(info.table.out)[k+tot.col] <- paste(group," (n=",length(which(sub %in% group)),")",sep="")
				}
			ins.row <- 2
			
			for(i in 1:length(vari))
				{	
				temp <- data[sub==group,vari[i]]
				# NUMERIC
        if(class(temp)=="numeric" | class(temp)=="integer")
					{
					temp <- as.numeric(temp)
					m <- round(mean(temp,na.rm=T),dpm)
					se <- qnorm(cip)* ( sd(temp,na.rm=T)/sqrt(length(temp)) )
					sedp <- round(se, dps)
					sdp <- round(sd(temp,na.rm=T), dps)
					u <- round(m+se,dps)
					l <- round(m-se,dps)
					usd <- round(m+sdp,dps)
					lsd <- round(m-sdp,dps)
					info.table.out[ins.row,1] <- vari[i]
						if(std.dev==T & ci==F) {
						info.table.out[ins.row,k+tot.col] <- noquote(paste(m," (",sdp,")"," [",length(temp[!is.na(temp)]),"]",sep=""))
						} else if(std.dev==T & ci==T) {
						info.table.out[ins.row,k+tot.col] <- noquote(paste(m," (",lsd,",",usd,")"," [",length(temp[!is.na(temp)]),"]",sep=""))												
						} else if(std.dev==F & ci==F) {
            			info.table.out[ins.row,k+tot.col] <- noquote(paste(m," (",sedp,")"," [",length(temp[!is.na(temp)]),"]",sep=""))
            			} else {
            			info.table.out[ins.row,k+tot.col] <- noquote(paste(m," (",l,",",u,")"," [",length(temp[!is.na(temp)]),"]",sep=""))            			
            			}
    		   ins.row <- ins.row + 1
					} else
				
        # FACTOR	
				if(class(temp)=="factor" & vari[i]==fac.vari[1] & num.numint!= -1 )
          {ins.row <- ins.row + 1}
        if(class(temp)=="factor")
					{
          for(j in 0:length(levels(temp)))
						{
						if(j==0)
						  {
						  info.table.out[ins.row,1] <- vari[i]
						  info.table.out[ins.row,k+tot.col] <- paste("n=",max(length(temp[!is.na(temp)]),1),sep="")
              ins.row <- ins.row + 1 
						  }  else {
  					  answer <- paste(round(length(which(temp %in% levels(temp)[j]))/max(length(temp[!is.na(temp)]),1)*100,dpm)," [",length(which(temp %in% levels(temp)[j])),"]",sep="")
							info.table.out[ins.row,1] <- paste("-",levels(temp)[j])
              info.table.out[ins.row,k+tot.col] <- answer
						  ins.row <- ins.row + 1
						  }
						}
						if(factor.na==T)
              {
              answer <- paste(sum(is.na(temp == levels(temp)[j])),sep="") 
		          info.table.out[ins.row,1] <- "Missing"					
						  info.table.out[ins.row,k+tot.col] <- answer
						  ins.row <- ins.row + 1    
					    }
				     else{
              }
					 }  
				}
				# END OF SUBSETTED COLUMNS
			}
			}
			# COMPARISONS 
  
  else {
	  
    # IF THERE ARE NO SUBSETS CHOSEN
    info.table.out <- as.data.frame(matrix(ncol=2))
		info.table.out[1,] <- c("",if(std.dev==F){"Mean (CI)[n]"}else{"Mean (SD)[n]"})
    if(exists("fac.vari")){info.table.out[2+num.numint,] <- c("","% [n]")}
    names(info.table.out) <- c(" ","Total")
    ins.row <- 2
		
		for(i in 1:length(vari))
			{	
			temp <- data[,vari[i]]
			
      # NUMERIC
			if(class(temp)=="numeric" | class(temp)=="integer")
				{
				temp <- as.numeric(data[,vari[i]])
				m <- round(mean(temp,na.rm=T),dpm)
				sdp <- round(sd(temp,na.rm=T), dps)
				se <- qnorm(cip)* ( sd(temp,na.rm=T)/sqrt(nrow(data)) )
				sedp <- round(se, dps)
				sdp <- round(sd(temp,na.rm=T), dps)
				u <- round(m+se,dps)
				l <- round(m-se,dps)
				usd <- round(m+sdp,dps)
				lsd <- round(m-sdp,dps)
				info.table.out[ins.row,1] <- vari[i]
				if(std.dev==T & ci==F) {
						info.table.out[ins.row,2] <- noquote(paste(m," (",sdp,")"," [",length(temp[!is.na(temp)]),"]",sep=""))
						} else if(std.dev==T & ci==T) {
						info.table.out[ins.row,2] <- noquote(paste(m," (",lsd,",",usd,")"," [",length(temp[!is.na(temp)]),"]",sep=""))												
						} else if(std.dev==F & ci==F) {
            			info.table.out[ins.row,2] <- noquote(paste(m," (",sedp,")"," [",length(temp[!is.na(temp)]),"]",sep=""))
            			} else {
            			info.table.out[ins.row,2] <- noquote(paste(m," (",l,",",u,")"," [",length(temp[!is.na(temp)]),"]",sep=""))            			
            			}
				ins.row <- ins.row + 1
				} else
        # FACTOR	
				if(class(temp)=="factor" & vari[i]==fac.vari[1] & num.numint!= -1 )
          {ins.row <- ins.row + 1}
        if(class(temp)=="factor")
					{
          for(j in 0:length(levels(temp)))
						{
						if(j==0)
						  {
						  info.table.out[ins.row,1] <- vari[i]
						  info.table.out[ins.row,2] <- paste("n=",max(length(temp[!is.na(temp)]),1),sep="")
              ins.row <- ins.row + 1 
						  }  else {
  					  answer <- paste(round(length(which(temp %in% levels(temp)[j]))/max(length(temp[!is.na(temp)]),1)*100,dpm)," [",length(which(temp %in% levels(temp)[j])),"]",sep="")
							info.table.out[ins.row,1] <- levels(temp)[j]
              info.table.out[ins.row,2] <- answer
						  ins.row <- ins.row + 1
						  }
						}
						if(factor.na==T)
              {
              answer <- paste(sum(is.na(temp == levels(temp)[j])),sep="") 
		          info.table.out[ins.row,1] <- "Missing"					
						  info.table.out[ins.row,2] <- answer
						  ins.row <- ins.row + 1    
					    }
				     else{
              }
					 } 
       }
    }


			if(comp==T)
			  {
			  names(info.table.out)[ncol(info.table.out)] <- " "
			  }
      if(comp==T & length(levels(sub))==2)
				{
				ins.row <- 2
				for(i in 1:length(vari))
					{	
					temp <- data[,vari[i]]
					if(class(temp)=="numeric" | class(temp)=="integer")
						{
						t.p.value.raw <- t.test(temp~sub)$p.value
						t.p.value <- round(t.p.value.raw , 3)

            if(t.p.value!=0.000)
              {
              info.table.out[ins.row,ncol(info.table.out) ] <- t.p.value
              } else if(t.p.value.raw <=0.00015)
                       {
                       info.table.out[ins.row,ncol(info.table.out) ] <- "<0.0001"
                       } else {
                       info.table.out[ins.row,ncol(info.table.out) ] <- round(t.p.value.raw, 4)
                       }
						ins.row <- ins.row + 1              
						} else if(class(temp)=="factor")
                     {
						         if(vari[i]==fac.vari[1] & num.numint!= -1 )
		                   {
		                   ins.row <- ins.row + 1
		                   }

                     t.p.value.raw <- chisq.test(temp,sub, correct=T)$p.value
                     t.p.value <- round(t.p.value.raw , 3)
						         
                        if(t.p.value!=0.000)
                          {
                          info.table.out[ins.row,ncol(info.table.out) ] <- t.p.value
                          } else if(t.p.value.raw <=0.00015)
                                   {
                                   info.table.out[ins.row,ncol(info.table.out) ] <- "<0.0001"
                                   } else {
                                   info.table.out[ins.row,ncol(info.table.out) ] <- round(t.p.value.raw, 4)
                                   }
                     for(l in 1:length(levels(temp)))
                        {
                        ins.row <- ins.row + 1
                        info.table.out[ins.row,ncol(info.table.out)] <- " "
                        }
                     if(factor.na==T)
                       {
                       ins.row <- ins.row + 1
                       info.table.out[ins.row,ncol(info.table.out)] <- " "
                       }                      
                     ins.row <- ins.row + 1

						         }
					}
				} # END OF TWO LEVEL COMPARISONS
				
			if(comp==T & length(levels(sub))>=3)

				{
				ins.row <- 2
				for(i in 1:length(vari))
					{	
					temp <- data[,vari[i]]
					if(class(temp)=="numeric" | class(temp)=="integer")
						{
						t.p.value.raw <- oneway.test(temp~sub)$p.value
						t.p.value <- round(t.p.value.raw , 3)
            if(t.p.value!=0.000)
              {
              info.table.out[ins.row,ncol(info.table.out) ] <- t.p.value
              } else if(t.p.value.raw <=0.00015)
                       {
                       info.table.out[ins.row,ncol(info.table.out) ] <- "<0.0001"
                       } else {
                       info.table.out[ins.row,ncol(info.table.out) ] <- round(t.p.value.raw, 4)
                       }
						ins.row <- ins.row + 1              
						} else if(class(temp)=="factor")
                     {
						         if(vari[i]==fac.vari[1] & num.numint!= -1 )
		                   {
		                   ins.row <- ins.row + 1
		                   }
						         t.p.value.raw <- chisq.test(temp,sub)$p.value                     
                     t.p.value <- round(t.p.value.raw ,3)

                        if(t.p.value!=0.000)
                          {
                          info.table.out[ins.row,ncol(info.table.out) ] <- t.p.value
                          } else if(t.p.value.raw <=0.00015)
                                   {
                                   info.table.out[ins.row,ncol(info.table.out) ] <- "<0.0001"
                                   } else {
                                   info.table.out[ins.row,ncol(info.table.out) ] <- round(t.p.value.raw, 4)
                                   }
                     for(l in 1:length(levels(temp)))
                        {
                        ins.row <- ins.row + 1
                        info.table.out[ins.row,ncol(info.table.out)] <- " "
                        }
                     if(factor.na==T)
                       {
                       ins.row <- ins.row + 1
                       info.table.out[ins.row,ncol(info.table.out)] <- " "
                       }                      
                     ins.row <- ins.row + 1

						         }
					}
				
    }	



	if(!missing(html))
		{
		require(xtable,quietly=T)
		right <-"r|"
		for(i in 1:(ncol(info.table.out)-2))
			{
			right <- paste(right,"r|",sep="")	
			}
		
		if(!missing(sub))
			{
			alignment <-paste("|l|l|",right,sep="")
			} else
			{
			alignment <-"|l|l|r|"
			}
		html.out <- xtable(info.table.out, align= alignment)
		print(html.out, type="html", include.rownames=F, quote=F, file=html,na.print = "&nbsp;")
		}
	return(info.table.out)
  }
