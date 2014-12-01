library(shiny)
library(ggplot2)
source("helpers.R")

shinyServer(function(input, output) {
  origData<<-NULL
  output$read_Origfile <- renderUI({
    fileInput("origfile",label="Read NONMEM raw data",accept=c('.csv','.txt','.sim','.dat'))    
  })

  output$choose_Xvar <- renderUI({
    if(is.null(input$origfile))
      return()    
    origData.name<-paste(input$origfile$datapath,input$origfile$name,sep="/") 
    origData<<-read.PKPDdata(input$origfile$datapath)   
    colnames <- colnames(origData)
    selectInput("Xvar", "Choose X(TIME) variable", 
                       choices  = c(" ",colnames))
  })

  
  output$choose_Yvar <- renderUI({
    if(is.null(input$origfile))
       return()
    if(is.null(input$origfile) | is.null(origData))
    { choice.temp<-c(" "," ")
    } else
    { choice.temp<-c(" ",colnames(origData))
    }
    selectInput("Yvar", "Choose Y(DV) variable", 
                choices  =choice.temp )
  })
  
  output$choose_IDvar <- renderUI({
    if(is.null(input$origfile))
      return()
    if(is.null(input$origfile) | is.null(origData))
    { choice.temp<-c(" "," ")
    } else
    { choice.temp<-c(" ",colnames(origData))
    }    
    selectInput("IDvar", "Choose ID variable", 
                choices  =choice.temp )
  })
  
  output$choose_COVvar <- renderUI({
    if(is.null(input$origfile))
      return()
    if(is.null(input$origfile) | is.null(origData))
    { choice.temp<-c(" "," ")
    } else
    { choice.temp<-c(" ",colnames(origData))
    }    
    
    selectInput("COVvar", "Choose Covariate for stratification", 
                choices  = choice.temp )
  })
  
  output$choose_COVn <- renderUI({
    if(is.null(input$origfile) | is.null(origData))
    { return()
    } else if(is.null(input$COVvar))
    { return()
    } else if(input$COVvar==" ")
    {  return()
    } 
    numericInput("COVn","Number of COV stratification",1)
    
  })
  
  output$summary <- renderPrint({
    if(is.null(input$origfile) | is.null(origData))
    { return()
    } else
    { 
       summary(origData)
    }
  })

  output$plot<-renderPlot({
    if(is.null(input$origfile)| is.null(origData) | is.null(input$Xvar)| is.null(input$Yvar)| is.null(input$IDvar) | is.null(input$COVvar))
    {  return()
    } else if(input$Xvar==" " | input$Yvar==" " | input$IDvar==" ")
    { return()
    } else  
    {
      X.name<-input$Xvar
      Y.name<-input$Yvar
      x.lim<-range(origData[,input$Xvar],na.rm=TRUE)
      y.lim<-range(origData[,input$Yvar],na.rm=TRUE)
      if(input$COVvar==" " | is.null(input$COVn)) 
      {   print(input$PlotMethod)            
         if(input$PlotMethod=="XY Scatter plot")
         {  XYplot.orig(origData,input$Xvar,input$Yvar,input$IDvar,x.lim,y.lim)
            
         } else if(input$PlotMethod=="profile plot")
         {  XYplotwithID.orig(origData,input$Xvar,input$Yvar,input$IDvar,x.lim,y.lim)
         }  
      } else if(input$COVn==0)   
      {
        if(input$PlotMethod=="XY Scatter plot")
        {  XYplot.orig(origData,input$Xvar,input$Yvar,input$IDvar,x.lim,y.lim)
        } else if(input$PlotMethod=="profile plot")
        {  XYplotwithID.orig(origData,input$Xvar,input$Yvar,input$IDvar,x.lim,y.lim)
        }               
      } else
      { 
        N.covbin<-input$COVn
        ID.temp<-origData[,input$IDvar]
        
        COV.ID<-which(!is.na(origData[,input$COVvar]))
        if(length(COV.ID)==nrow(origData))
        {   temp.COV<-origData[,input$COVvar]
        } else  
        {  temp.COV<-NULL
           for(i in COV.ID)
           temp.COV<-c(temp.COV,rep(origData[i,input$COVvar],
                                   length=sum(ID.temp==origData[i,input$IDvar])))
        }   
        if(length(table(temp.COV))<=5)
        {
          cov.bin<-list(COV.bin=factor(temp.COV),COV.bin.ID=names(table(temp.COV)))
        } else
        { cov.bin<-makeCOVbin(temp.COV,N.covbin)
        }  
        if(input$PlotMethod=="XY Scatter plot")
        {  XYplot.orig.with.COV(origData,input$Xvar,input$Yvar,input$IDvar,x.lim,y.lim,cov.bin)
        } else if(input$PlotMethod=="profile plot")
        {  XYplotwithID.orig.with.COV(origData,input$Xvar,input$Yvar,input$IDvar,x.lim,y.lim,cov.bin)
        }        
      }
    }
  })

})
