#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(2, ''),
    column(8, tags$div(HTML(
             '<h1>Distribution plots for Neighborhood Nestwatch species measurement data</h1>
             <hr>
             <p style = "font-size: 18px">This document contains the distribution of mass, wing, and tail measurements for the Neighborhood Nestwatch (NN) focal species. The intent of this document is to provide a mechanism for field technicians to double-check whether the measurements that they obtain are valid. The plotted distributions are based on measurements taken at NN sites. Use this document to compare your measurement with the observed distribution. The x axis is labelled with:</p>'
           )
           )),
    column(2, '')
  ),
  fluidRow(
    column(2, ''),
    column(8, tags$div(HTML(
      '<img src="exampleDistribution.png" 
	title="the tidyverse"
      alt = "icons"
      border="0"
      style= "float: right; 
      padding-right: 0px;
      border-style: solid;
      border-width: 10px;
      border-color: white;
      width: 25%;"/>
      <ul style = "font-size: 18px">
      <li>The mean</li>
      <li>the mean &plusmn; 1 standard deviation</li>
      <li>the minimum and maximum values beyond which the measurement is considered an outlier</li>
      </ul>
      <p style = "font-size: 18px">If your measurement is within the blue region, it is within the acceptable range. If your measurement is outside of the blue region, please take the measurement again.</p>
      <br>
      <hr>'
    )
    )),
    column(2, '')
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

