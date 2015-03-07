

require(tidyr)
require(plyr)
require(dplyr)
require(ggplot2)
require(rgl)



data <- read.csv("data/scotland_all.csv")

names(data) <- c(
  "country",
  "year",
  "age",
  "sex",
  "convicted",
  "total"
)

data$sex <- tolower(data$sex)

data$age <- revalue(data$age, c("90 & over" = "90"))
data$age <- as.numeric(as.character(data$age))
data <- data  %>% 
  mutate(
    convict_rate = convicted/total,
    birth_year = year-age
    ) %>%
  filter(age <=60)

shinyServer(function(input, output){
  
  
  output$agePlot <- renderPlot({
    out <- data %>%
      filter(age==input$this_age) %>%
      ggplot() +
      geom_line(aes(x=year, y=convict_rate, group=sex, linetype=sex, colour=sex)) +
      scale_colour_manual(values=c("red", "blue")) + 
      scale_linetype_manual(values=c("dashed", "solid")) + 
      coord_cartesian(ylim=c(0,0.15)) + 
      labs(list(
        title=paste0("Conviction rates for Scots aged ", input$this_age),
        x="Year", y="Conviction rate"))
    return(out)
  })
  
  output$yearPlot <- renderPlot({
    out <- data %>%
      filter(year==input$this_year) %>%
      ggplot() +
      geom_line(aes(x=age, y=convict_rate, group=sex, linetype=sex, colour=sex)) +
      scale_colour_manual(values=c("red", "blue")) + 
      scale_linetype_manual(values=c("dashed", "solid")) + 
      coord_cartesian(ylim=c(0,0.15), xlim=c(16, 50)) + 
      labs(list(
        title=paste0("Conviction rates for Scots in year ", input$this_year),
        x="Age in years", y="Conviction rate"))
    return(out)
  })
  
  output$byearPlot <- renderPlot({
    out <- data %>%
      filter(birth_year==input$this_byear) %>%
      ggplot() +
      geom_line(aes(x=age, y=convict_rate, group=sex, linetype=sex, colour=sex)) +
      scale_colour_manual(values=c("red", "blue")) + 
      scale_linetype_manual(values=c("dashed", "solid")) + 
      coord_cartesian(ylim=c(0,0.15), xlim=c(16, 50)) + 
      labs(list(
        title=paste0("Conviction rates for Scots born in year ", input$this_byear),
        x="Age in years", y="Conviction rate"))
    return(out)
  })
  
  
  
})