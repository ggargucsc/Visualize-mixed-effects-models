library(shiny)
library(ggplot2)
library(gridExtra)
library(lme4)    
library(grid); 
library(viridis)  #plotly requires viridis
library(plotly)   #for interactive visualizations



#function to rescale the fixed predictors 
rescaleVariables<-function(data)
{
  if(is.null(data))
    return(NULL)
  
  colNames<-names(data)
  
  #converting fixed variables with class 'integer' to factor if levels are <=10, in case not done by default 
 for( i in colNames)
  {
    if(class(data[,i]) %in% 'integer')
       if(length(levels(factor(data[,i])))<=10) 
          data[,i]<-factor(data[,i])
 }
   
   classVector<-sapply(data, class) %in% c("integer", "numeric")
      
    #rescaling the values by dividing them with the absolute maximum value
    dataRescaled <- apply(data[classVector], 2, function(x) {
      x <- x/abs(max(x))
        
      #round all the values to 3 digits after rescaling
      x <- round(x, digits=3)
        
      return(x)
      })
   
   if(length(dataRescaled)==0)
   {
     
     #final data with original and rescaled variables
     dataFinal <- data[colNames]
     
     
   } else {
    
     includeColumns <- setdiff(colNames, names(data.frame(dataRescaled)))
     
     #final data with original and rescaled variables
     dataFinal <- as.data.frame(cbind(data[includeColumns], dataRescaled))
   }
   
         
  return(dataFinal)
                                                              
}



#function to rank random variables in data based on mean of dependent variable
rankVariablesBasedOnMean<-function(data, randomVar, depVar)
{
  
  if(is.null(data))
     return(NULL)
 
  if (is.null(randomVar) || length(randomVar)==0 || is.null(depVar) || length(depVar)==0) 
    return(NULL)
  
  #removing NA values from subset of data
  dataSubset<- na.omit(data[,c(randomVar, depVar)])
  
  dependent <- dataSubset[,depVar]
  random <- dataSubset[,randomVar]
  
  #calculate the mean of dependent variable for every random factor
  meanDependent <- aggregate(dependent ~ random, dataSubset, function(x) mean(x))
  
  #rank the levels based on the mean value
  reordered <- reorder(meanDependent$random, meanDependent$dependent)
  dataSubset[,randomVar] <- factor(dataSubset[,randomVar], levels(reordered))
  
  return(dataSubset)
}



#function to remove the selected columns from list
remove <- function(colnames, removeColumn)
{
  
  if (setequal(colnames, removeColumn)) {
    # If sets are equal, return an empty string
    return("")
  } else {
    # Return elements that are only in colnames
    setdiff(colnames, removeColumn)
  }
}



####################### shiny server logic starts from here ############################################

shinyServer(function(input, output,clientData, session) {
  observe({
  
    
  #read datafile 
  #It is "reactive" function and therefore should be automatically re-executed when inputs change
  dataFull <- reactive({
    file1 <- input$file
    
    if(is.null(file1))
    return()
    
    #read datafile based on the inputs selected by the user
    read.table(file=file1$datapath, sep=input$sep, header=input$header,stringsAsFactors=input$stringsAsFactors)
    })
  
  
  #get the column names
  colnames <- reactive({ 
    names(dataFull()) 
    })
  
 
  #table output is displayed in the first tab
  output$table <- renderDataTable({
    if(is.null(dataFull()))
      return()
    
      dataFull()
    }, options=list(pageLength=10))
  
  

 #Dependent variable - selectInput is updated based on the column names
 #observeEvent updates the dependent variable when data changes
  observeEvent(dataFull(), {
    updateSelectInput(session, "dependent", choices = colnames())
   })
  

#Fixed variables - dependent variable is removed from the choices provided to user for fixed variables
chooseFixedVariables <- reactive({
  depVariable <- input$dependent
  
  if(is.null(depVariable)) 
    return()
  
  return(remove(colnames(), depVariable))
})


#Random variables - checkBoxGroup is updated after removing selected dependent variable
observeEvent(chooseFixedVariables(), {
    updateCheckboxGroupInput(session, "fixed",
                           choices = chooseFixedVariables(), selected = "") 
})



################### action after user clicks the button after selecting dependent, fixed and random variables ##

variablesEntered <- eventReactive(input$plotButton, {
  
  #validate to return a validation error that user can understand  
  validate(
    need(try(!is.null(input$fixed) || input$random != ""), "Please select atleast one variable (either fixed or random) to continue")
  )  

  randomVariablesVector <- NULL
  
 #number of fixed variables selected by user
 lengthFixed <- length(input$fixed)
 
 
 if( input$random == "")
 {
  #includes both fixed variables and random variables selected by the user 
  totalVariablesVector <- c(input$fixed)
  
   lengthRandom <- 0
   randomVariablesVector <- input$random
   
 } else {
   
 #extract random vectors entered; separated by comma 
 randomVector <- (strsplit(input$random, "\\,"))[[1]]  
 
 #number of random variables
 lengthRandom <- length(randomVector)
 
 #removing the whitespaces if any in the random variables
 for( i in 1:lengthRandom)
 randomVariablesVector[i] <- gsub("\\s", "", randomVector[i])
 
 
 #merge the fixed and random variables in one
 totalVariablesVector <- c(input$fixed, randomVariablesVector)
 }
 
 return(list(input$dependent, totalVariablesVector, lengthFixed, lengthRandom, input$fixed,  randomVariablesVector)) 
})


#from the function above, individual values are extracted so that it can be used by other functions
#dependent variable
dependent <- reactive({
  variablesEntered()[[1]]
})


#includes both fixed and random variables
totalVariables <- reactive({
  variablesEntered()[[2]]
})


#length of total number of fixed and random variables
lengthTotalVariables <- reactive({
  length(variablesEntered()[[2]])
})


#length of fixed variables 
lengthFixed <- reactive({
  variablesEntered()[[3]]
})


#length of random variables 
lengthRandom <- reactive({
  variablesEntered()[[4]]
})


#fixed variables selected 
fixedVariables <- reactive({
  variablesEntered()[[5]]
})


#random variables selected
randomVariables <- reactive({
  variablesEntered()[[6]]
})

#use only subset of data 
data <- reactive({
  if(is.null(dataFull()))
    return()
  
  data <- dataFull()[,c(dependent(),fixedVariables(),randomVariables())]
  
  return(data)
})

########### plots showing relationship between dependent/fixed and dependent/random variables #########

# Insert the right number of plot output objects into the web page
#dynamic UI
output$uiFixedRandomPlots <- renderUI({
  
  #list of plots 
  plotOutputList <- lapply(1:lengthTotalVariables(), function(i) {
    
    plotname <- paste("plot", i, sep="")
    
    tags$div(class = "group-output",
             
             #plotly is used for interactive visualizations
             #output is a Plotly plot
             plotlyOutput(plotname, height = 500, width = 500),
             
             #checkbox is placed next to every plot so that user can select the plots to be included in the model
             checkboxInput(inputId = paste0("plotVariable", i), " ")
             
    )
})
  
  
  # Convert the list to a tagList - this is necessary for the list of items
  # to display properly.
  do.call(tagList, plotOutputList)
  
})


observe({ 
  
  # plot is created for each of the variable (both fixed and random)
  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  for( i in 1:lengthTotalVariables())
  {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      
      plot.i <- i
      plotname <- paste("plot", plot.i, sep="")
      
     #renderPlotly to render Plotly output
      output[[plotname]] <- renderPlotly({
        
        # plots showing relationship between dependent and fixed variables only 
        if(plot.i <= lengthFixed())
        {
          fixedVariable <- totalVariables()[plot.i]
          
          #removed NA values from subset of data
          dataSubset <- na.omit(data()[, c(dependent(), fixedVariable)])
          
          gg <- ggplot(dataSubset, aes_string(y = dependent(), x = fixedVariable))+
               geom_point(alpha=0.2, size=8)+theme_bw()+
               geom_smooth(method = lm, se=FALSE, shape = 21, color = "red") +
               ggtitle(paste(dependent(), "~", fixedVariable))
          
         #plotly is used for making ggplots interactive 
          p <- ggplotly(gg)
          p
          
        } else {  
          # plots showing relationship between dependent and random variables only
          
          randomVariable <- totalVariables()[plot.i]
          
          #randomVariable is ranked based on the mean of dependent variable for every random factor
          dataAfterRank <- rankVariablesBasedOnMean(data(), randomVariable, dependent())
          
          #ggplot showing relationship between dependent and random variable
          gg <- ggplot(dataAfterRank, aes_string(y = dependent(), x = randomVariable))+
                geom_point(alpha=0.2, size=8)+theme_bw()+
                stat_summary(aes_string(y = dependent(), x = randomVariable), fun.y="mean",geom="point", colour="blue", size=9)+
                ggtitle(paste(dependent(), "varies by", randomVariable))
          
          p <- ggplotly(gg)
          p
        }
      })
      
    })
  }
  
})  

################### random slope density plots and correlation plots ############################

#some of the fixed variables shouldn't be included in the random slope 
# as they are random variable level covariate, so there is no variation based on that fixed variable

#this function return a list of fixed variables to be included in the random slope plots, and a list 
#of fixed variables that shouldn't be included in the random slope plots
FixedVariablesForRandomSlope <- reactive({
  
  variablesList <- list()
  variablesNotInList <- list()
  k <- 1
  m <- 1
  
  if(lengthRandom() != 0 && lengthFixed() != 0)
  {
    
    for(i in 1:lengthRandom())
    {
      for(j in 1:lengthFixed())
      {
        #levels of random variable
        uniqueLevelsRandom <- length(levels(factor(data()[,randomVariables()[i]])))
        
        #unique combination of random variable and fixed variable
        uniqueFixedRandomCombinations <- nrow(unique(subset(data(), select = c(randomVariables()[i], fixedVariables()[j]))))
        
        #fixed variables to be included for the random slope plots
        if(uniqueLevelsRandom != uniqueFixedRandomCombinations)
        {
          
          #find the slope coefficients for the variables that are included for random slope plots
          beta <- NULL
          dataTemp <- na.omit(data()[,c(dependent(), fixedVariables()[j], randomVariables()[i])])
          
          for(temp in unique(dataTemp[,randomVariables()[i]]))
          {
            dataSubset <- dataTemp[dataTemp[, randomVariables()[i]]==temp, ]
            
            beta <- rbind(beta, lm(dataSubset[ ,dependent()] ~ dataSubset[ ,fixedVariables()[j]])$coeff)
          }
          
          colnames(beta) <- c("intercept", "slope")
          beta <- na.omit(beta)
          beta[, "slope"] <- round(beta[,"slope"], digits=2)
          #find the max and min value of slope coefficients
          maxSlope <- max(beta[, "slope"])
          minSlope <- min(beta[, "slope"])
          
          
          #including the min and max value of slope coefficients with other variables
          variablesList[[k]] <- c(fixedVariables()[j], randomVariables()[i], "slope", maxSlope, minSlope )
          k <- k+1
          
          #this was addes in the list as a reference for correlation plots
          variablesList[[k]] <- c(fixedVariables()[j], randomVariables()[i], "correlation")
          k <- k+1
          
        } else {
          
          #fixed variables that shouldn't be included for the random slope plots
          variablesNotInList[[m]] <- c(fixedVariables()[j], randomVariables()[i])
          m <- m+1
        }
      }
    }
    
    return(list(variablesList, variablesNotInList))
    
  } else {
    
    return()
  }
  
})



output$uiSlopeCorrelationPlots <- renderUI({
  
  #returns a list of the fixed variables that are not random variable level covariates and 
  #can be included for random slopes
  variablesIncluded <- FixedVariablesForRandomSlope()[[1]]
  
  
  #for the random slope plots, length of both fixed variables and random variables shouldn't be zero
  if(lengthRandom() != 0 && lengthFixed() != 0)
  {
    plotOutput <- lapply(1:length(variablesIncluded) , function(i){
      
      plot.slope <- paste("plot.slope", i, sep="")
      
      tags$div(class = "group-output",
               
               plotOutput(plot.slope, height = 500, width = 500),
               
               checkboxInput(inputId = paste0("plot.slope", i), " ")
      )
    })
    
    
    do.call(tagList, plotOutput)
    
  }
  
  else {
    
    return()
  }
  
  
})


#random slope plots are created here
observe({
  
  #number of random slope and correlation plots to be equal to number of items in the below list
  variablesIncluded <- FixedVariablesForRandomSlope()[[1]]
  
  for(i in 1:length(variablesIncluded))
  {
    
    local({
      my.i <- i
      plot.slope.i <- my.i
      plot.slope <- paste("plot.slope", plot.slope.i, sep="")
      
      #for random slope and correlation plots
      output[[plot.slope]] <- renderPlot({
        
        beta <- NULL
        
        fixedVar <-variablesIncluded[[my.i]][1]
        randomVar <- variablesIncluded[[my.i]][2]
        typeVar <- variablesIncluded[[my.i]][3]
        
        dataTemp <- na.omit(data()[,c(dependent(), fixedVar, randomVar)])
        
        #intercept and slope coefficients for every random factor
        for(temp in unique(dataTemp[,randomVar]))
        {
          dataSubset <- dataTemp[dataTemp[, randomVar]==temp, ]
          beta <- rbind(beta, lm(dataSubset[ ,dependent()] ~ dataSubset[ ,fixedVar])$coeff)
        }
        
        colnames(beta) <- c("intercept", "slope")
        beta<- na.omit(beta)
        beta<- as.data.frame(beta)
        
        
        if(typeVar %in% "slope")
        {
          #for a given random variable, this function finds maximum and minimum value of slope coefficients 
          #among different fixed variables
          maxMinSlope <- as.numeric(findMaxSlope(randomVar))
          
         
          #random slope density plots
          plot(density(beta[,"slope"]), xlim=c(maxMinSlope[1], maxMinSlope[2]), xlab="slope", main=paste("Variation on", fixedVar, "by", randomVar))
          #gg <- ggplot(beta, aes(x = slope)) + geom_density() +theme_bw() +
          #      ggtitle(paste("Variation on", fixedVar, "by", randomVar)) +
                #range of every random slope density plot should be same for a given random variable
                #for comparisons
          #      coord_cartesian(xlim = c(maxMinSlope[1], maxMinSlope[2]))
          #p <- ggplotly(gg)
          #p
          
        } else {
          #correlation plots
          ggplot(beta, aes(y = slope, x = intercept)) + 
                geom_point()+theme_bw()+stat_smooth(method="lm", se=FALSE)+ 
                ggtitle(paste("Correlation plot\n", fixedVar, randomVar))
        # p <- ggplotly(gg)
        # p
        }   
        
      })
      
    })
  }
})


#this function finds maximum and minimum value of slope coefficients for a given random variable
#among different fixed variables
findMaxSlope <- function(random)
{
  variablesIncluded <- FixedVariablesForRandomSlope()[[1]]
  
  #there are always two items in the above list with same fixed variable and random variable
  #only difference is one provides reference to slope plots, other to correlation plots
  #so nrow is half the length
  matrixTemp <- matrix(0, nrow=length(variablesIncluded)/2, ncol=5)
  colnames(matrixTemp) <- c("fixed", "random", "slopecorr", "maxslope", "minslope")
  
  j<-1
  for( i in 1: length(variablesIncluded))
  {
    #only those list variables are included as matrix rows that are reference to random slope plots
    if(variablesIncluded[[i]][3]=="slope")
    {
    matrixTemp[j,] <- variablesIncluded[[i]]
    j <- j+1
    }
  }
  
  #select rows from a matrix with the same random variable
  tempData <- matrixTemp[matrixTemp[,"random"]==random,, drop=FALSE]
  
  #find max and min values of slope coefficients from the selected rows
  maxSlope <- max(as.numeric(tempData[, "maxslope"]))
  minSlope <- min(as.numeric(tempData[, "minslope"]))
  
  return(c(minSlope, maxSlope ))    
}   


###################### model building #############################################################

#action after user selects the plots to be included in the model 
modelSelected <- reactive({
  variablesSelectedForModel <- NULL
  
  #this is for dependent/fixed plots and dependent/random plots 
  #includes the variables for the plots that are selected by user  
  for(i in 1:lengthTotalVariables())
  {
  temp <- input[[paste0("plotVariable",i)]]
  
  #if user selects the plot then include the variable associated with it
  if(!is.null(temp) && temp == TRUE)
  {
    #includes the variables for both dependent/fixed and dependent/random plots
    variablesSelectedForModel<-c(variablesSelectedForModel, totalVariables()[i])
  }
  }
 
 #separate fixed and random variables
 #fixed variables for model 
 fixedSelectedForModel <- intersect( fixedVariables(), variablesSelectedForModel )
 
 #random variables for model
 randomSelectedForModel <- setdiff(variablesSelectedForModel, fixedSelectedForModel)

 #includes the variables for selected plots for random slopes and correlation
 #this is a list that includes (fixed variable, random variable, slope or correlation)
 slopeVariablesIncluded <- FixedVariablesForRandomSlope()[[1]]
 
 SlopeCorrForModel <- list()
 m <- 1
 
 for( i in 1:length(slopeVariablesIncluded))
 {
    fixedVarForSlope <- slopeVariablesIncluded[[i]][1]
    randomVarForSlope <- slopeVariablesIncluded[[i]][2]
    plotType <- slopeVariablesIncluded[[i]][3]
    
     #check if random slope is selected
     temp1 <- input[[paste0("plot.slope",i)]]
     
     if(!is.null(temp1)  && temp1 == TRUE && plotType %in% "slope")
     {
        
         # i+1 means correlation plot
         #check if user selected the correlation plot as well for that random slope
         temp <- input[[paste0("plot.slope",i+1)]]
         
         #correlation exists between random intercept and random slope
         #if user includes both random slope and correlation plot, check if user included random intercept plot as well 
         #1 will be added else 0 will be added
         if(temp == TRUE && randomVarForSlope %in% randomSelectedForModel)
         {
           
           SlopeCorrForModel[[m]] <- c(fixedVarForSlope, randomVarForSlope, 1)
           m <- m+1
           
         } else {
           
           SlopeCorrForModel[[m]] <- c(fixedVarForSlope, randomVarForSlope, 0)
            m <- m+1
         }
       } 
 }
 
  #returns fixedVariables, randomVariables, totalvariables and Slope-Correlation Variables for Model building
  return(list(fixedSelectedForModel, randomSelectedForModel, variablesSelectedForModel,  SlopeCorrForModel))
   
  
})

#fixedVariables for model building
fixedSelectedForModel <- reactive({
  modelSelected()[[1]]
})

#randomVariables for model building
randomSelectedForModel <- reactive({
  modelSelected()[[2]]
})

#total Variables for model building
totalVariablesForModel <- reactive({
  modelSelected()[[3]]
})

#slope-correlation terms for model building
interceptSlopeVariables <- reactive({
  modelSelected()[[4]]
})



################ model diagnostics plots for fixed variables and random intercepts only ###################
#output is a grid of model diagnostics plots(for fixed variables and random intercepts only)
output$modelPlots<-renderPlot({
 
  #reorder the fixed selected variables based on R2(predictive power) before including them in model
  fixedVector <- rankBasedOnR2(fixedSelectedForModel())
  lengthFixedVector <- length(fixedVector)
  
  #reorder the random variables based on AIC values before including them in model
  randomVector <- rankBasedOnAic(randomSelectedForModel())
  lengthRandomVector <- length(randomVector)
  
  #length of total fixed and random variables selected for model
  lengthTotalVar <- length(totalVariablesForModel())
  
  temp <- NULL
  temp1 <- NULL
  g <-list()
  m <- 1
  j <- 1 
  

  for( i in 1:lengthTotalVar)
  {
    #model diagnostics plots only when fixed variables are included in the model
    if(i <= lengthFixedVector)
    { 
      temp <- c(temp, fixedVector[i])
      
      #subset the data and remove NA values
      dataSubset <- na.omit(data()[, c(dependent(), temp)])
      
      regressionFormula <- paste0(dependent(), "~", paste(temp, collapse="+"))
      
      #simple regression for fixed variables
      model <- lm(regressionFormula, data=dataSubset)
      
      r.squared <- format(round(summary(model)$r.squared, digits=4))
      aic <- format(round(AIC(model), digits=2))
      
      #for ggtitle and formula in ggplot
      title <- paste("AIC - ",aic, "; R2-squared - ",r.squared)
      formula <- paste("Model after including covariate", fixedVector[i])
      
    } else {
      
      temp1 <- c(temp1,randomVector[j] )
      
      
      #subset the data and remove NA values(now including random variable)
      dataSubset <- na.omit(data()[, c(dependent(), fixedVector, temp1)])
      
      mixedFormula <- paste0(dependent(), "~", paste(fixedVector, collapse="+"), "+( 1|", paste(temp1, collapse=")+( 1|"), ")" )
      
      #linear mixed effects model 
      model <- lme4::lmer(as.formula(mixedFormula), data=dataSubset)  
      
      #calculate AIC value for the model
      aic <- format(round(AIC(model), digits=2))
      
      #for ggtitle and formula in ggplot
      title <- paste("AIC - ",aic)
      formula <- paste("Model after including random intercept for ", randomVector[j])
      j <- j+1
      
    }
   
    predictedValue <- predict(model)
    
    #calculating the residuals
    residuals <- predictedValue - dataSubset[,dependent()]
    
    dataSubset <- as.data.frame(cbind(dataSubset[dependent()], predictedValue, residuals))
    
    colnames(dataSubset)<-c("dep", "pred", "resid")
    
   
    #observed vs predicted values
    g[[m]] <- ggplot(dataSubset, aes(y=dep, x=pred))+
              geom_point(alpha=0.1, size=4) + xlim(c(range(dataSubset["dep"])))+ 
              xlab(label=paste("predicted", dependent())) + ylab(label=paste("observed", dependent()))+
              stat_smooth(method="lm", se=FALSE, colour="red")+theme_bw()+
              ggtitle(formula)+theme(plot.title = element_text(size=14,color='black',face='bold',  vjust=1), axis.title = element_text(face='bold'))
    
    m <- m+1
    
    #residuals vs predicted values
    g[[m]] <- ggplot(dataSubset, aes(y=resid, x=pred))+geom_point(alpha=0.1, size=4)+
              xlim(c(range(dataSubset["dep"])))+ ylim(c(rangeResiduals()))+ xlab(label=paste("predicted", dependent()))+
              ylab(label="residuals")+geom_hline(yintercept=0, colour="red")+
              ggtitle(title)+theme_bw()+
              theme(plot.title = element_text(size=14,color='black',face='bold',  vjust=1), axis.title = element_text(face='bold'))
    
    
  #  g[[m]] <- ggExtra::ggMarginal(
  #    g[[m]],
  #    type = 'histogram',
  #    margins = 'y',
  #    size = 5,
  #    binwidth=0.2,
  #    col = 'black',
  #    fill = 'grey')
    
    m <- m+1
    
    
  }
  
  #arrange all the plots in a grid
  do.call("grid.arrange", c(g, list(ncol=2 ), respect=TRUE))
  
})

#range of residuals should be same for all the plots( and based on first plot) for comparison
#created a function so that it can be used for slope-corr plots as well
rangeResiduals <- function()
{
  fixedVector <- rankBasedOnR2(fixedSelectedForModel())
  
  #calculate range of residuals based on first plot
  regFormula <- paste(dependent(), "~", fixedVector[1])
  dataS <- na.omit(data()[, c(dependent(),fixedVector[1])])
  model <- lm(regFormula, data=dataS)
  
  predicted <- predict(model)
  
  #calculating the residuals
  resid <- predicted - dataS[,dependent()]
  
  return(range(resid))
}



#dynamic height based on number of plots 
plotHeight<-function()
{
  lengthTotalVar <- length(totalVariablesForModel())

  return(500*lengthTotalVar)
}

#grid of model diagnostics plots for fixed variables and random intercepts only
#are rendered to UI based on the dynamic height
output$plotModel<-renderUI({
  
  plotOutput('modelPlots', height=plotHeight())
  
  
})


################ model diagnostics plots after including random slopes and correlations ################

#output is a grid of model diagnostics plots(after including random slope and correlations terms)
output$plotSlope <- renderPlot({
  
  slopeVector <- interceptSlopeVariables()
  fixedVector <- rankBasedOnR2(fixedSelectedForModel())
  
  
  #matrix is created for including list elements 
  matrixTemp <- matrix(0, nrow=length(slopeVector), ncol=3)
  colnames(matrixTemp) <- c("fixed", "random", "corr")
  
  for( i in 1: length(slopeVector))
    matrixTemp[i,] <- slopeVector[[i]]
  
  #unique number of random variables 
  uniqueRandom <- unique(matrixTemp[,2])
 
  
  for( i in 1:length(uniqueRandom))
  {
    #select rows in a matrix with same random variable
    tempData <- matrixTemp[matrixTemp[,"random"]==uniqueRandom[i],, drop=FALSE]
   
    temp1 <- NULL
    temp2 <- NULL
   
    #if correlation is 1, then  model formula will change accordingly
    for(j in 1:length(tempData[,1]))
    {
      #if correlation is included, then (1+) will be added to the fixed variable that is included for random slope
      #also variables will be added one by one (1+var1+var2)
      if(tempData[j,"corr"] == 1)
      {
        temp1 <- c(temp1, tempData[j, "fixed"])
        tempData[j, "fixed"] <- paste0("1 +", paste0(temp1, collapse = "+"))
        
      } else if(tempData[j,"corr"] == 0) { #if no correlation , then (0+) will be added
        
        temp2 <- c(temp2, tempData[j, "fixed"])
        tempData[j, "fixed"] <- paste0("0 +", paste0(temp2, collapse = "+"))
      }
    }
    matrixTemp[matrixTemp[,"random"]==uniqueRandom[i],] <- tempData
    
  }
  
  #matrixTemp will look something like this based on user selection of plots
  # (1+var1, randomVar1, 1)
  # (0+var2, randomvar2, 0)
  # (1+var1+var2, randomVar1, 1)
  # (0+var3, randomVar1, 0)
  
  dataSubset <- na.omit(data()[,c(dependent(), fixedVector, randomSelectedForModel())])
  
  #rescaling fixed variables to avoid warning messages after including random slopes in mixed model
  dataRescaled <- rescaleVariables(dataSubset[fixedVector])
  
  data.final <- as.data.frame(cbind(dataSubset[c(dependent(), randomSelectedForModel())], dataRescaled))
  
  randomVector <- rankBasedOnAic(randomSelectedForModel())
  nrowMatixTemp <- nrow(matrixTemp)
   i <- 1
   m <- 1
   
   g <- list()
  
  for(j in 1:nrowMatixTemp)
  {
    
    t1 <- matrixTemp[i, "random"]
    t2 <- matrixTemp[i, "corr"]
    t3 <- matrixTemp[i, "fixed"]
    
    #logic behind this is 
    #find row with the same random variable and correlation term before ith row
    #if exists, delete that row
    #
    #for example - if there are two rows (1+var1, randomVar1, 1) and (1+var1+var2, randomVar1, 1)
    #delete the first one so that there exists one unique updated row to be included in the model 
    
    if(i-1 > 0)
    { 
      if(i==2)
      {
         temp <- matrixTemp[1,"random"]==t1 && matrixTemp[1,"corr"]==t2  
        
      } else if(i > 2)
      {
         temp <- apply(matrixTemp[1:(i-1),], 1, function(x){ 
         x["random"]==t1 && x["corr"]==t2 
      })
      }
      
      #if matched, temp is TRUE, and hence find its row number to delete that row
      a <- which(temp==TRUE)
      
      #choose the last row
      b<-tail(a,1)
      
      if(length(b) >=1)
      {
        #delete that row
        matrixTemp <- matrixTemp[-b,, drop=FALSE] 
        i <- (i-1)
        
      }
    }
   
    #if (1+var1|randomvar1) term is in the model, then remove (1|randomvar1) term from the model
    if(t2 == "1")
      randomVector <-  randomVector[!(randomVector %in% t1)]
    
    
    if(i ==1)
    {
      matrixSlope <- paste0("(", matrixTemp[1,"fixed"], "|", matrixTemp[1,"random"], ")")
      
    } else if(i > 1)
    {
    matrixSlope <- apply(matrixTemp[1:i,], 1, function(x){
      paste0("(", x["fixed"], "|", x["random"], ")")
      
    })
    }
    
    if(length(randomVector)!=0)
    {
      mixedformula<-paste0(dependent(), "~", paste(fixedVector, collapse="+") ,"+ (1|", paste(randomVector, collapse=") + (1|"), ") + " , paste(matrixSlope, collapse="+") )
      
      #Part1 and Part2 are created for ggtitle in ggplot to display properly
      #mixedformulaPart1 <- paste0(dependent(), "~", paste(fixedVector, collapse="+") ,"+ (1|", paste(randomVector, collapse=") + (1|"), ") + " )
      #mixedformulaPart2 <- paste(matrixSlope, collapse="+")
      
      #for ggplot title
      formula <- paste("Model after adding random slope (",matrixTemp[i,"fixed"],") by", matrixTemp[i,"random"])
             
    } else {
      
      mixedformula<-paste0(dependent(), "~", paste(fixedVector, collapse="+") ," + " ,paste(matrixSlope, collapse="+")  ) 
      
      #mixedformulaPart1 <- paste0(dependent(), "~", paste(fixedVector, collapse="+") , " + " )
      #mixedformulaPart2 <- paste(matrixSlope, collapse="+")

     #for ggplot title
     formula <- paste("Model after adding random slope (", matrixTemp[i,"fixed"], ") by", matrixTemp[i,"random"])
      
    }
   
   #mixed model formula 
   modelPred<-lme4::lmer(as.formula(mixedformula), data=data.final)
   modelAIC<-lme4::lmer(as.formula(mixedformula), data=data.final, REML=FALSE)
    
    #the values should be rescaled back to original value and then predicted value should be calculated
    #will update it once done -- UPDATE REQUIRED
    predicted.value<-predict(modelPred)
   
    residuals <- predicted.value-data.final[,dependent()]
    
    dataSubset <- as.data.frame(cbind(data.final[,dependent()], predicted.value, residuals))
    colnames(dataSubset)<-c("dep", "pred", "resid")
    
    #AIC value for model
    aic<-format(round(AIC(modelAIC), digits=2))
    
    #observed vs predicted plots
    g[[m]]<-ggplot(dataSubset, aes(y=dep, x=pred))+geom_point(alpha=0.1, size=4)+ 
            xlab(label=paste("predicted", dependent()))+xlim(c(range(dataSubset["dep"])))+
            ylab(label=paste("observed", dependent()))+theme_bw()+
            ggtitle(formula)+
            stat_smooth(method="lm", se=FALSE, colour="red")+
            theme(plot.title = element_text(size=14,color='black',face='bold',  vjust=1), axis.title = element_text(face='bold')) 
    
    
    m <- m+1
    
    #residuals vs predicted plots
    g[[m]] <- ggplot(dataSubset, aes(y=resid, x=pred))+geom_point(alpha=0.1, size=4)+ 
              xlab(label=paste("predicted", dependent()))+ylab(label="residuals")+
              xlim(c(range(dataSubset["dep"])))+ylim(c(range(rangeResiduals())))+
              geom_hline(yintercept=0, colour="red")+theme_bw()+
              ggtitle(paste("AIC - ",aic))+theme(plot.title = element_text(size=14,color='black',face='bold',  vjust=1), axis.title = element_text(face='bold'))
    
  
  #    g[[m]] <- ggExtra::ggMarginal(
  #    g[[m]],
  #    type = 'histogram',
  #    margins = 'y',
  #    size = 5,
  #    binwidth=0.2,
  #    col = 'black',
  #    fill = 'grey')
    
  
    
    i <- (i+1)
    m <- m+1
  }
   
   
   #arrange plots in a grid
   do.call("grid.arrange", c(g, list(ncol=2 ), respect=TRUE))
    
})

#dynamic height 
plotHeight2<-function()
{
  lengthSlope <- length(interceptSlopeVariables())
  return(500*lengthSlope)
  
}

#render plots to UI
output$plot_ui3<-renderUI({
  
  plotOutput('plotSlope', height=plotHeight2())
  
})

#will update this and then include in UI --UPDATE REQUIRED
output$text1 <- renderText({ 
  if(is.null(model()))
  {
    return()
  }
  else
  {
    "The model is build by adding variables one at a time. At every step, there are three plots: First - Observed vs Predicted, Second - Residuals vs Predicted, Third - Marginal plot displaying histogram of residuals "
  }
  
})



#function to arrange fixedVector based on R2 values
rankBasedOnR2 <- function(vectorSelected )
{
  
  
  if(!is.null(vectorSelected))
  {
    rSquaredValues <- NULL
  
    for ( i in 1: length(vectorSelected))
    {
      dataSubset <- na.omit(data()[,c(dependent(), vectorSelected[i])])
      
      regressionformula <- paste0(dependent(), "~", vectorSelected[i])
      model <- lm(regressionformula, data = dataSubset)
      rSquaredValues[i] <- summary(model)$r.squared
    }
   
    rankVector <- vectorSelected[order(-rSquaredValues)]  
    
  } else {
    return()
  }
  
  
  return(rankVector)
  
}


#function to arrange randomVector based on AIC values
rankBasedOnAic <- function(randomSelectedForModel )
{
  
  
  if(!is.null(randomSelectedForModel))
  {
    aicValues <- NULL
    
    for ( i in 1: length(randomSelectedForModel))
    {
      dataSubset <- na.omit(data()[,c(dependent(), fixedSelectedForModel(), randomSelectedForModel[i])])
      
      mixedformula <- paste0(dependent(), "~", paste(fixedSelectedForModel(), collapse = "+"), "+ (1|"   ,randomSelectedForModel[i], ")")
      model <- lme4::lmer(as.formula(mixedformula), data = dataSubset)
      aicValues[i] <- AIC(model)
    }
    
    #rank the vector based on AIC values 
    rankVector <- randomSelectedForModel[order(aicValues)]  
    
  } else {
    return()
  }
  
  
  return(rankVector)
  
}


})})
  


  
 
