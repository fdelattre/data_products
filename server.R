#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(e1071)
library(randomForest)
library(nnet)
library(caret)
library(gbm)

generate.data <- function(n, dist = "ellipse") {
  x2 <- runif(n, -2,2)
  x1 <- runif(n, -2,2)
  
  if(dist == "triangle"){y <- factor(ifelse((x2>2*x1)|(x2>(2-2*x1)),1,2))}
  if(dist == "ellipse"){y <- factor(ifelse((x1-0.3)^2+(x2-0.4)^2 <= 1,1,2))}
  if(dist == "linear"){y <- factor(ifelse(x2<=x1+1.2,1,2))}
  
  return(data.frame(x1,x2,y))
}


set.seed(54321)


shinyServer(function(input, output) {
  
  train  <- reactive({
    generate.data(300, input$dist)
    })
  
  test  <- reactive({test  <- generate.data(10000, input$dist)})
  
  output$train_plot <- renderPlot({
    g_train <- ggplot(data = train()) +
      geom_point(aes(x1, x2, col = y))+
      ggtitle("Train dataset")
    print(g_train)
  })
  
  output$test_plot <- renderPlot({
    input$apply
    isolate({
      test.dat <- NULL
      train.dat <- train()
      test.tmp  <- test()
      if (input$model == "SVM") {
              if (input$kernel == "linear") {
                svm.lin <-
                  svm(y ~ x1 + x2,
                      train.dat,
                      kernel = "linear",
                      cost = input$cost)
              }
              
              if (input$kernel == "polynomial") {
                svm.lin <-
                  svm(
                    y ~ x1 + x2,
                    train.dat,
                    kernel = "poly",
                    cost = input$cost,
                    coef0 = 1,
                    degree = 2
                  )
              }
              
              pred <- predict(svm.lin, newdata = test.tmp)
              test.dat <- cbind(test.tmp, pred)
            }
            
            
            
            if (input$model == "Random Forest") {
              rf <-
                randomForest(as.factor(y) ~ x1 + x2,
                             data = train.dat,
                             ntree = input$ntrees)
              pred <- predict(rf, newdata = test.tmp)
              test.dat <- cbind(test.tmp, pred)
            }
      
      if (input$model == "Neural Network") {
        net <- nnet(y ~ x1 + x2, data = train.dat, size=input$size, decay=input$decay, maxit=200, trace = F)
        
        pred <- predict(net, newdata = test.tmp, type = "class")
        test.dat <- cbind(test.tmp, pred)
        levels(test.dat$pred) <- levels(train.dat$y) 
      }
      
      if (input$model == "GBM") {
        
        objControl <- trainControl(
          method='cv', 
          number=3, 
          returnResamp='none', 
          summaryFunction = twoClassSummary, 
          classProbs = TRUE)
        
        mod_train <- train()
        mod_train$y <- ifelse(mod_train$y == 1, "X1", "X2")
        
        params <- data.frame(
          interaction.depth = input$gbm.interaction.depth,
          n.trees           = input$gbm.n.iterations,
          shrinkage         = 0.1,
          n.minobsinnode    = 10
        )
        
        objModel <- caret::train(y ~ ., data=mod_train,
                          method='gbm', 
                          trControl=objControl, tuneGrid = params,
                          
                          metric = "ROC",
                          preProc = c("center", "scale"))
        
        pred.gbm <- predict(objModel, newdata = test.tmp)
        pred.gbm <- factor(ifelse(pred.gbm == "X1", 1, 2))
        
        test.dat <- cbind(test.tmp, pred=pred.gbm)
        levels(test.dat$pred) <- levels(train.dat$y) 
      }
      
            if (!is.null(test.dat)) {
              g_test <- ggplot(data = test.dat) +
                geom_point(aes(x1, x2, col = pred)) +
                geom_point(data = test.dat[test.dat$pred != test.dat$y,],
                           aes(x1, x2),
                           col = "black",
                           lwd = 1)+
                ggtitle("Test dataset")
              
              print(g_test)
              
              output$mytable <- renderTable({
                if (!is.null(test.dat)){
                  caret::confusionMatrix(test.dat$pred, test.dat$y)$table
                }
              })
              
            }
      })
    
    
  })
  

  
})
