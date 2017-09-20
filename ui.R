#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("There is no free lunch !"),
  sidebarLayout(
    sidebarPanel(#"sidebar panel",
                 h1('How-to'),
                 h5("The models selected below are trained on a 300-points random dataset and applied on a 10000 points test set."),
                 h5("The training datasets can have different shapes : linear, triangle and elliptic"),
                 h5("Select a data shape, a model, set some parameters and see how the models behave..."),
                 hr(),
                 # sliderInput("train_nrow", 
                 #             "N train set",
                 #             min = 100, max = 1000, value = 300, step = 10, ticks = F),
                 
                 selectInput("model", "Choose a model:",
                              choices = c("SVM", "Random Forest", "Neural Network", "GBM"),
                              selected = "Random Forest"),
                 
                 conditionalPanel(condition = "input.model == \"SVM\"",
                                   selectInput("kernel", 
                                               "Kernel",
                                               choices = c("linear", "polynomial")),
                                   sliderInput("cost", "Cost",
                                               min = 1, max = 10000, value = 100, step = 100)),
                 
                 
                 conditionalPanel(condition = "input.model == \"Random Forest\"",
                                  sliderInput("ntrees", 
                                              "Number of trees",
                                              min = 1, max = 100, value = 10, step = 1, ticks = F)),
                 
                 
                 conditionalPanel(condition = "input.model == \"Neural Network\"",
                                  sliderInput("size", 
                                              "Number of neurons in hidden layer",
                                              min = 1, max = 10, value = 5, step = 1, ticks = F),
                                  numericInput("decay",
                                              "Weigth decay",
                                              min = 0, max = 2, value = 0.05, step = 0.01)),
                 
                 conditionalPanel(condition = "input.model == \"GBM\"",
                                  sliderInput("gbm.n.iterations", 
                                              "Number of iteration",
                                              min = 1, max = 100, value = 50, step = 5, ticks = F),
                                  numericInput("gbm.interaction.depth",
                                               "Interaction Depth",
                                               min = 1, max = 5, value = 2, step = 1)),
                 
                 
                 tabPanel("Table", tableOutput("mytable")),
                 
                 actionButton("apply", "Apply")),
    mainPanel(#"main panel",
      radioButtons("dist", "Distribution type:",
                   c("Ellipse" = "ellipse",
                     "Triangle" = "triangle",
                     "Linear" = "linear")),
              plotOutput("train_plot"),
              plotOutput("test_plot"))
  )
)
)



