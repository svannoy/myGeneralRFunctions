testArgs <- function(printIt = 1116.123456789, meanDigits = getOption("digits") , sdDigits = getOption("digits") , freqDigits=getOption("digits") , statDigits = getOption("digits") , pDigits=getOption("digits") ){
  
  meanFormatStr <- paste("%0.", meanDigits, "f", sep='')
  sdFormatStr <- paste("%0.", sdDigits, "f", sep='')
  freqFormatStr <- paste("%0.", freqDigits, "f", sep='')
  statFormatStr <- paste("%0.", statDigits, "f", sep='')
  pFormatStr <- paste("%0.", pDigits, "f", sep='')
  
  sprintf(meanFormatStr, printIt)
  sprintf(sdFormatStr, printIt)
  sprintf(freqFormatStr, printIt)
  sprintf(statFormatStr, printIt)
  sprintf(pFormatStr, printIt)
}

testArgs()

# Note there is a bug if there are zero's in a group by variable cell.....
buildTable1Rows <- function(theVariable, theData, groupBy=NULL, percentFirst = TRUE, conductGroupTests = TRUE, 
                          meanDigits = getOption("digits") , sdDigits = getOption("digits") , freqDigits=getOption("digits") , statDigits = getOption("digits") , pDigits=getOption("digits")){
require(dplyr)
require(tidyr)  

    if (!(theVariable %in% colnames(theData))){
    stop(paste("Called getTable1Row with theVariable set to ", theVariable, " but it is not a column name in theData"))
  }

  if (is.null(groupBy) == FALSE) {
    if (!(groupBy %in% colnames(theData))){
      stop(paste("Called getTable1Row with groupBy set to ", groupBy, " but it is not a column name in theData"))
    }
  }

  if (nrow(theData) == 0) {
      stop(paste("Called getTable1Row with an empty data frame "))
  }

  meanFormatStr <- paste("%0.", meanDigits, "f", sep='')
  sdFormatStr <- paste("%0.", sdDigits, "f", sep='')
  freqFormatStr <- paste("%0.", freqDigits, "f", sep='')
  statFormatStr <- paste("%0.", statDigits, "f", sep='')
  pFormatStr <- paste("%0.", pDigits, "f", sep='')
  
  numericFormatStr <- paste(meanFormatStr, " (", sdFormatStr, ")", sep='')
  
  # Process the "main" demographic (i.e. the total value ignoring groupBy)
  if (is.numeric(theData[,theVariable])){
    table1Rows <- data.frame(Demographic = theVariable, Value = sprintf(numericFormatStr, mean(theData[,theVariable], na.rm=TRUE), sd(theData[,theVariable], na.rm=TRUE)))
  } else {
    rows <- cbind(prop.table(table(theData[,theVariable]))*100, table(theData[,theVariable]))
    table1Rows <- NULL
    if (percentFirst == TRUE){
      firstIndex = 1
      secondIndex = 2
      firstString = "%"
      secondString = ""
      formatStr <- paste(freqFormatStr,"%s (%0.0f%s)", sep='') #%0.1f%s (%0.0f%s)
    } else {
      firstIndex = 2
      secondIndex = 1
      firstString = ""
      secondString = "%"
      formatStr <-  paste("%0.0f%s (", freqFormatStr,"%s)", sep='')  #"%0.0f%s (%0.1f%s)"
    }
    for (i in 1:nrow(rows)){
      table1Rows <- rbind(table1Rows, data.frame(demo = row.names(rows)[i], value = sprintf(formatStr, rows[i,firstIndex],firstString, rows[i,secondIndex], secondString)))
    }
    colnames(table1Rows) <- c(theVariable, "Frequency")
  }
  
  # Do group stratification if asked for
  if (is.null(groupBy) == FALSE){
    # Process the "main" demographic (i.e. the total value ignoring groupBy)
    if (is.numeric(theData[,theVariable]) == TRUE){
      # Used to have na.omit in here, I don't think it is necessary or wanted but I'm leaving the syntaxt for now     groupSummary <- na.omit(select_(theData, theVariable, groupBy)) %>% group_by_(groupBy) %>% select_(theVariable) %>% dplyr::summarize_each(funs(mean, sd))
      groupSummary <- select_(theData, theVariable, groupBy) %>% group_by_(groupBy) %>% select_(theVariable) %>% na.omit() %>% dplyr::summarize_each(funs(mean, sd))
      groupRow <- NULL
      for (i in 1:nrow(groupSummary)){
        groupRow <- rbind(groupRow, data.frame(groupBy = groupSummary[i,1], meanSd = sprintf(numericFormatStr, groupSummary[i,2],groupSummary[i,3]), stringsAsFactors = FALSE))
      }
      groupSpread <- tidyr::spread_(data = groupRow, key=groupBy, value="meanSd")
      table1Rows <- cbind(table1Rows, groupSpread)

      if (conductGroupTests == TRUE){
        f <- paste(theVariable, "~", groupBy)
        aov_summary <- summary(do.call("aov", list(as.formula(f), data=theData)))
        table1Rows <- cbind(table1Rows, FStat=sprintf(statFormatStr, aov_summary[[1]][["F value"]][1]), PValue = sprintf(pFormatStr, aov_summary[[1]][["Pr(>F)"]][1]), stringsAsFactors = FALSE)
        # Just for snazzy feature, put a '<' sign in front of pvalue if it is showing all zeros
        if (as.numeric(table1Rows$PValue) == 0.0) { # One of the few times a direct comparison of real zero is helpful/valid
          table1Rows$PValue <- paste("<", table1Rows$PValue)
        }
      }
    } else {
      groupSummary <- as.data.frame(table(theData[,theVariable], theData[,groupBy]))
      groupCount <- group_by(groupSummary, Var2) %>% dplyr::summarize(count = sum(Freq))
      groupSummary$percent = (groupSummary$Freq/groupCount$count)*100
      if (percentFirst == TRUE){
        groupSummary$countPercentStr <- sprintf(formatStr, groupSummary$percent, "%", groupSummary$Freq, "")
      } else {
        groupSummary$countPercentStr <- sprintf(formatStr, groupSummary$Freq, "",  groupSummary$percent, "%")
      }
      groupTable <- select(groupSummary, Var1, Var2, countPercentStr)
      names(groupTable) <- c(theVariable, groupBy, "countPercentStr")
      groupTable <- spread_(groupTable, groupBy, "countPercentStr")
      table1Rows <- left_join(x=table1Rows, y=groupTable, by=theVariable)

      # Do chi-square test to see if they differ by grouping variable
      if (conductGroupTests == TRUE){
        names(groupSummary) <- c(theVariable, groupBy, "Freq", "percent", "countPercentStr")
        chiData <- ungroup(groupSummary[,c(2, 3)]) %>% arrange_(groupBy)
        groupByColumn <- match(groupBy, names(theData))
        numGroups <- length(levels(theData[,groupByColumn]))
        varColumn <- match(theVariable, names(theData))
        numVarLevels <- length(levels(theData[,varColumn]))
        chiData$row <- rep(1:numVarLevels, numGroups)
        chiData <- spread_(data=chiData, key=groupBy, value="Freq")
        chiData$row <- NULL
        chiProbs <- prop.table(table(theData[,theVariable]))
        chiData <- as.matrix(chiData)
        chiSquareResults <- chisq.test(chiData)
        chiSquareResults$statistic
        chiSquareResults$p.value
  
        table1Rows$Chisq <- c(sprintf(statFormatStr, as.numeric(chiSquareResults$statistic)), rep("", nrow(table1Rows)-1))
        table1Rows$PValue <- c(sprintf(pFormatStr, as.numeric(chiSquareResults$p.value)), rep("", nrow(table1Rows)-1))
               # Just for snazzy feature, put a '<' sign in front of pvalue if it is showing all zeros
        if (as.numeric(table1Rows$PValue[1]) == 0.0) { # One of the few times a direct comparison of real zero is helpful/valid
          table1Rows$PValue[1] <- paste("<", table1Rows$PValue[1])
        }
      }
    }
  }
  return(table1Rows)
}

buildTable1List <- function(theData, theVariables, groupBy, percentFirst = TRUE, conductGroupTests = TRUE,
                          meanDigits = getOption("digits") , sdDigits = getOption("digits") , freqDigits=getOption("digits") , statDigits = getOption("digits") , pDigits=getOption("digits")){

  table1List <- lapply(theVariables, FUN=buildTable1Rows, theData=theData, groupBy=groupBy, conductGroupTests=conductGroupTests, 
                       meanDigits=meanDigits, sdDigits=sdDigits, freqDigits=freqDigits, statDigits=statDigits, pDigits=pDigits)
}
