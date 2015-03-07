shinyUI(fluidPage(
  
  titlePanel("Crime Data App Example"),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("this_age", "Select age:",
                  min=16, max=60, value=20, step=1,
                  sep="", animate=animationOptions(interval=300, loop=TRUE)
                  ),
      
      sliderInput("this_year", "Select year:",
                  min=1989, max=2011, value=2000, step=1,
                  sep="", animate=animationOptions(interval=300, loop=TRUE)
                  ),
      
      sliderInput("this_byear", "Select birth year:",
                  min=1960, max=1985, value=1981, step=1,
                  sep="", animate=animationOptions(interval=300, loop=TRUE)
                  )
      
      ),
    
    mainPanel(
      plotOutput("agePlot"),
      plotOutput("yearPlot"),
      plotOutput("byearPlot")
      
      )
                   
      ) 
    )
  )