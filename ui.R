shinyUI(fluidPage(
  headerPanel("Explore NONMEM data"),
  
  sidebarPanel(
    uiOutput("read_Origfile"),
    br(),
    uiOutput("choose_Xvar"),
    uiOutput("choose_Yvar"),
    uiOutput("choose_IDvar"),
    uiOutput("choose_COVvar"),
    uiOutput("choose_COVn"),
    br()
  ),
  
  sidebarPanel(
    radioButtons("PlotMethod",label=h4("Plot type"),c("XY Scatter plot","profile plot"))
  ),
    
  mainPanel(
    plotOutput("plot"),
    h4("Summary Statistics"),
    verbatimTextOutput("summary")
  )
))
