library(rtf)

# This file contains "specialty" print functions that will print to either an RTF file if supplied or to the screen, with special rounding and column formating

# These functions use some 'global' variables for tracking table and plot numbers

myTablePrint.env <- new.env()
myTablePrint.env$tableNum = 1
myTablePrint.env$figureNum = 1

resetTableCount <- function() myTablePrint.env$tableNum <- 1
setTableCount <- function(x) myTablePrint.env$tableNum <- x


resetFigureCount <- function() myTablePrint.env$figureNum <- 1
setFigureCount <- function(x) myTablePrint.env$figureNum <- x

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
myRegressionPrint <- function(rtf=NULL, modelFit, isLogit = FALSE, numDigits=as.numeric(options("digits")), title=" "){
  
  modelSummary <- summary(modelFit)
  
  if (isLogit == TRUE){
    dfToPrint <- as.data.frame(exp(cbind(OR=coef(modelFit),confint(modelFit))))
    dfToPrint$pValues <-  cbind(modelSummary$coefficients[,4])
    dfToPrint$estimates <-  cbind(modelSummary$coefficients[,1])
    dfToPrint$std.error <-  cbind(modelSummary$coefficients[,2])
    dfToPrint$Z <-  cbind(modelSummary$coefficients[,3])
    
    
    colNames = c("OR", "2.5%", "97.5%", "P-Value", "Estimate", "S.E.", "Z")
    myRoundedTablePrint(rtfFile, dfToPrint, colsToRound=c(1:7), numDigits, title, includeRowNames=TRUE, colNames)
    
  }
  else { 
    dfToPrint <- as.data.frame(modelSummary$coefficients)
    colNames = c("Estimate", "S.E.", "Z", "P-Value")
    myRoundedTablePrint(rtfFile, dfToPrint, colsToRound=c(1:4), numDigits, title, includeRowNames=TRUE, colNames)
    
    # Blank line
    myTextPrint(rtfFile, "")
    
    # Use Broom package to get regression summary
    require(broom)
    myRoundedTablePrint(rtfFile, dfToPrint = glance(modelFit),colsToRound = c(1:5,7:10),numDigits=4, paste("Summary Stats for ", title))
  }

}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
myChiSquarePrint <- function(rtf=NULL, chiSquareResults, numDigits=as.numeric(options("digits")), title=""){
    
  dfToPrint <- data.frame(Chi2 = as.numeric(chiSquareResults$statistic), Df=as.numeric(chiSquareResults$parameter), PValue = chiSquareResults$p.value)
  
  myRoundedTablePrint(rtfFile, dfToPrint, colsToRound=c(1,3), numDigits, title, includeRowNames=FALSE, colNames = c("Chi-Squ", "D.F.", "P-Value"))
}



#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
myWaldTestPrint <- function(rtf=NULL, waldResults, numDigits=as.numeric(options("digits")), title=""){
  
valuesToPrint <- as.numeric(unlist(waldResults$result))
  
  dfToPrint <- data.frame(Chi2 = valuesToPrint[1], Df=valuesToPrint[2], PValue = valuesToPrint[3])
  
  myRoundedTablePrint(rtfFile, dfToPrint, colsToRound=c(1:3), numDigits, title, includeRowNames=FALSE, colNames = c("Chi-Squ", "D.F.", "P-Value"))
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
myRoundedTablePrint <- function(rtf=NULL, dfToPrint=NULL, colsToRound=0, numDigits=as.numeric(options("digits")), title="", includeRowNames=FALSE, colNamesForPrinting){
  
  if (is.null(dfToPrint)) {
    warning("You called myRoundedTablePrint without a table to print (or a NULL table?)")
    return(NULL)
  }
  
  # How many columns to round?
  if(missing(colsToRound) == TRUE){
    numCols = 0
  }
  else{ # Note the error checking and abort of function if number of columns to round doesn't work for the function
    numCols <- length(colsToRound)
    
    if (numCols > ncol(dfToPrint)){
      warning(paste("You called myRoundedTablePrint with numCol = ", numCols, " but there were only ",ncol(dfToPrint), "columns in your dataframe"))
      return(NULL)
    }
    
    if (max(colsToRound) >  ncol(dfToPrint)){
      warning(paste("You called myRoundedTablePrint with max(colsToRound) = ", max(colsToRound), " but there were only ", ncol(dfToPrint), "columns in your dataframe"))
      return(NULL)
    }
    
  }
  
  # Set variable for inclusion of row names
  if(missing(includeRowNames)){
    includeRowNames = FALSE
  }
  
  # Get how many digits to round to depending on if it was set or not
  if (missing(numDigits) == TRUE){
    howManyDigits = options("digits")
  }
  else {
    howManyDigits = numDigits
  }
  
  # Set the title to include the current table number and then increment current table number
  title <- paste("Table ",myTablePrint.env$tableNum, " - ",title)
  myTablePrint.env$tableNum <<- myTablePrint.env$tableNum + 1
  
  # If column names for printing were supplied, then assign them
  if(missing(colNamesForPrinting) == FALSE) {
    
    if (length(colNamesForPrinting) != ncol(dfToPrint)){
      warning(paste("You called myRoundedTablePrint with ", length(colNamesForPrinting), " column labels, but there are ", ncol(dfToPrint), "columns in your dataframe"))
      return(NULL)
    }
    
    colnames(dfToPrint) <- colNamesForPrinting
  }

  # If they provided columns to round then do it
  if (numCols != 0){
    # Round the columns
    for (i in 1:numCols){
      dfToPrint[,colsToRound[i]] <- round(dfToPrint[,colsToRound[i]], howManyDigits)
    }
  }
  
  # If they didn't provide an rtf object, then do a normal print
  if ((missing(rtf) == TRUE) || (is.null(rtf) == TRUE)){
    print(title)
    print(dfToPrint)
  }
  else {
    # Setup to print to rtf, currently bordered by two paragraph marks, should become an option (first confirm we have the right object type for the RTF file)
    if(class(rtf)[1] == "RTF"){
      addNewLine(rtf)
      addNewLine(rtf)
      
      addParagraph(rtf, title)
      addTable(rtf, dfToPrint, font.size=11, col.justify="C", header.col.justify="C", row.names=includeRowNames)
      
      addNewLine(rtf)
      addNewLine(rtf)
    }
    else {
      warning("Called myRoundedTablePrint with an invalid rtf object")  
    }
  }
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
myTextPrint <- function(rtf, textStr){
  
  if ((missing(rtf) == TRUE) || (is.null(rtf) == TRUE)){
    print(textStr)
  }
  else {
    addParagraph(rtf, textStr)
  }
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
myTextListPrint <- function(rtf = NULL, textList, includeRowNums = FALSE){
  
  for (i in 1:length(textList)){

      if (includeRowNums == TRUE) {
        printString <- sprintf("[%d] %s",i, textList[i])
      } else {
        printString <- sprintf("%s", textList[i])
      }

    if (is.null(rtf) == TRUE){
        print(printString)
    }
    else {
      addParagraph(rtf, printString)
    }
  }
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
myPlotPrint <- function(rtf = NULL, thePlot){
  
  # Get the title and add the Figure # (note, not all plots sent here have titles -  e.g. BayesianFirstAid)
  if (is.element('labels',names(thePlot))){
    thePlot$labels$title <- paste("Figure ", myTablePrint.env$figureNum, " - ",  thePlot$labels$title)
  } else {
    plot(thePlot)
    return(NULL)
  }
  
  # Increment current count of figures
  myTablePrint.env$figureNum <- myTablePrint.env$figureNum +1
  
  # Determine if we are printing to standard out, or going to an rtf file
  if ((missing(rtf) == TRUE) || (is.null(rtf) == TRUE)){
    print(thePlot) # Standard out
  }
  else{ # Going to an RTF file
    
    addNewLine(rtf)
    addNewLine(rtf)

    addPlot(rtf, plot.fun=print, thePlot,  width=7.25, height=7.25, res=300)

    addNewLine(rtf)
    addNewLine(rtf)
  }
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
plotSurvResultsRTF <- function(rtf=NULL){
  
  
  titleTable <-   c("Survival For Entire Sample - 10 Year",
                    "Survival by Sex - 10 Year",
                    "Survival by Minority Status - 10 Year",
                    "Survival by Median Age - 10 Year",
                    "Survival by Quantile Age - 10 Year",
                    "Survival by Investigation Outcome - 10 Year",
                    
                    
                    "Survival For Entire Sample - 1 Year",
                    "Survival by Sex - 1 Year",
                    "Survival by Minority Status - 1 Year",
                    "Survival by Median Age - 1 Year",
                    "Survival by Quantile Age - 1 Year",
                    "Survival by Investigation Outcome - 1 Year",
                    
                    
                    "Survival For Entire Sample - 90 day",
                    "Survival by Sex - 90 day",
                    "Survival by Minority Status - 90 day",
                    "Survival by Median Age - 90 day",
                    "Survival by Quantile Age - 90 day",
                    "Survival by Investigation Outcome - 90 day",
                    
                    "Survival For Entire Sample - 30 day",
                    "Survival by Sex - 30 day",
                    "Survival by Minority Status - 30 day",
                    "Survival by Median Age - 30 day",
                    "Survival by Quantile Age - 30 day",
                    "Survival by Investigation Outcome - 30 day",
                    
                    "Survival For Entire Sample - 7 day",
                    "Survival by Sex - 7 day",
                    "Survival by Minority Status - 7 day",
                    "Survival by Median Age - 7 day",
                    "Survival by Quantile Age - 7 day",
                    "Survival by Investigation Outcome - 7 day")
  
  for( i in 1:length(titleTable)){
    titleTable[i] <- paste("Figure ", myTablePrint.env$figureNum, " - ", titleTable[i], sep="")
    myTablePrint.env$figureNum <- myTablePrint.env$figureNum + 1
  }
  
  if (is.null(rtf) != TRUE){ # For printing to RTF File
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, survTot[[1]], title=titleTable[1])
    addPlot(rtf, plot.fun=myAddPlot, width=7.25, height=7.25,res=300, survTot[[2]], title=titleTable[2])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, survTot[[5]],title=titleTable[3])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, survTot[[8]],title=titleTable[4])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, survTot[[11]],title=titleTable[5])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, survTot[[14]],title=titleTable[6])
    
    
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv365Day[[1]], title=titleTable[7])
    addPlot(rtf, plot.fun=myAddPlot, width=7.25, height=7.25,res=300, surv365Day[[2]], title=titleTable[8])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv365Day[[5]],title=titleTable[9])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv365Day[[8]],title=titleTable[10])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv365Day[[11]],title=titleTable[11])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv365Day[[14]],title=titleTable[12])
    
    
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv90Day[[1]], title=titleTable[13])
    addPlot(rtf, plot.fun=myAddPlot, width=7.25, height=7.25,res=300, surv90Day[[2]], title=titleTable[14])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv90Day[[5]],title=titleTable[15])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv90Day[[8]],title=titleTable[16])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv90Day[[11]],title=titleTable[17])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv90Day[[14]],title=titleTable[18])
    
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv30Day[[1]], title=titleTable[19])
    addPlot(rtf, plot.fun=myAddPlot, width=7.25, height=7.25,res=300, surv30Day[[2]], title=titleTable[20])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv30Day[[5]],title=titleTable[21])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv30Day[[8]],title=titleTable[22])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv30Day[[11]],title=titleTable[23])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv30Day[[14]],title=titleTable[24])
    
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv7Day[[1]], title=titleTable[25])
    addPlot(rtf, plot.fun=myAddPlot, width=7.25, height=7.25,res=300, surv7Day[[2]], title=titleTable[26])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv7Day[[5]],title=titleTable[27])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv7Day[[8]],title=titleTable[28])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv7Day[[11]],title=titleTable[29])
    addPlot(rtf, plot.fun=myAddPlot,  width=7.25, height=7.25,res=300, surv7Day[[14]],title=titleTable[30])
  }
  else {
    myAddPlot(survTot[[1]], title=titleTable[1])
    myAddPlot(survTot[[2]], title=titleTable[2])
    myAddPlot(survTot[[5]],title=titleTable[3])
    myAddPlot(survTot[[8]],title=titleTable[4])
    myAddPlot(survTot[[11]],title=titleTable[5])
    myAddPlot(survTot[[14]],title=titleTable[6])
    
    
    myAddPlot(surv365Day[[1]], title=titleTable[7])
    myAddPlot(surv365Day[[2]], title=titleTable[8])
    myAddPlot(surv365Day[[5]],title=titleTable[9])
    myAddPlot(surv365Day[[8]],title=titleTable[10])
    myAddPlot(surv365Day[[11]],title=titleTable[11])
    myAddPlot(surv365Day[[14]],title=titleTable[12])
    
    
    myAddPlot(surv90Day[[1]], title=titleTable[13])
    myAddPlot(surv90Day[[2]], title=titleTable[14])
    myAddPlot(surv90Day[[5]],title=titleTable[15])
    myAddPlot(surv90Day[[8]],title=titleTable[16])
    myAddPlot(surv90Day[[11]],title=titleTable[17])
    myAddPlot(surv90Day[[14]],title=titleTable[18])
    
    myAddPlot(surv30Day[[1]], title=titleTable[19])
    myAddPlot(surv30Day[[2]], title=titleTable[20])
    myAddPlot(surv30Day[[5]],title=titleTable[21])
    myAddPlot(surv30Day[[8]],title=titleTable[22])
    myAddPlot(surv30Day[[11]],title=titleTable[23])
    myAddPlot(surv30Day[[14]],title=titleTable[24])
    
    myAddPlot(surv7Day[[1]], title=titleTable[25])
    myAddPlot(surv7Day[[2]], title=titleTable[26])
    myAddPlot(surv7Day[[5]],title=titleTable[27])
    myAddPlot(surv7Day[[8]],title=titleTable[28])
    myAddPlot(surv7Day[[11]],title=titleTable[29])
    myAddPlot(surv7Day[[14]],title=titleTable[30])
    
  }
}