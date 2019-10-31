
################
# WEEK 8 WEDNESDAY SHINY PROJECT
# Help code found on Steens github
# Create a reactive app, add to git
################


library(shiny)
library(tidyverse)

#assigning objects - setting slider range to range of carats in dataset
min_mpg <- min(mtcars$mpg) # set these objects before creating slider adjuster in the UI
max_mpg <- max(mtcars$mpg)

var_axis <- names(mtcars) #names of axes within mtcars

factor.indices <- vapply(mtcars, is.factor, TRUE) # re-read why this is needed!
factor.columns <- var_axis[factor.indices]

# Define UI for application that draws a histogram
ui <- fluidPage( #UI is all about the look of the app
  
  # Title
  titlePanel("In-Class Shiny Project"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("mpg_adjuster", #slider ID
                  "Miles per gallon", #what will actual show as the slider name
                  min = min_mpg,
                  max = max_mpg,
                  value = c(min_mpg, max_mpg)),
      
      
      selectInput(inputId = "xvar",
                  label = "X axis",
                  choices = var_axis,
                  selected = "x"), # the first one that will be picked when you open the app
      
      
      selectInput(inputId = "yvar",
                  label = "Y axis",
                  choices = var_axis,
                  selected = "y"),
      
      
      submitButton(text = "Go!") # can use submit or action 
    ),
    
    
    mainPanel(
      plotOutput("mtcars_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) { # what is happening under the hood to make the app work
  
  cars_filt <- reactive({
    #Filter the mtcars plot so that it only contains the specified range
    low.mpg <- input$mpg_adjuster[1] # figure out why we are doing 1 and 2 on these lines
    high.mpg <- input$mpg_adjuster[2]
    
    mtcars %>%
      filter(mpg >= low.mpg) %>%
      filter(mpg <= high.mpg)
  })
  
  output$mtcars_plot <- renderPlot({
    
    ggplot(data = cars_filt(), # since we are using reactive for cars_filt, it needs () in the ggplot
           mapping = aes_string(x = input$xvar, 
                                y = input$yvar)) + 
          geom_point()
    
  })
  
  output$diagnostic <- renderText(
    input$mpg_adjuster
  )
  

}

# Run the application 
shinyApp(ui = ui, server = server)


# to add everything to git repo (go to ternmianl)  
# git add * 
# git commit -m "slider is finalized, but rest is not
