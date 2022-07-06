library(shiny)
library(shinythemes)
library(dplyr)
library(mvtnorm)
library(ggplot2) #plot
library(bslib) #theme setting
library(markdown)

my_theme <- bs_theme(
  bg = "#fff", fg = "#000", primary = "#A3D1D1",
  secondary = "	#FFBB00",
  base_font = font_google("Lato"),
  code_font = font_google("Lato")
)

ui <- navbarPage( theme = my_theme,
                  "Multivariate Normal", 
                  
                  #PCA
                  tabPanel("PCA",
                           sidebarLayout(
                             sidebarPanel(
                               sliderInput("k", "Class", min = 3, max = 6, value = 3), #k
                               textInput("class1", "Mean vector1 :", ""), #mean vector1
                               textInput("class2", "Mean vector2 :", ""), #mean vector2
                               textInput("class3", "Mean vector3 :", ""), #mean vector3
                               textInput("class4", "Mean vector4 :", ""), #mean vector4
                               textInput("class5", "Mean vector5 :", ""), #mean vector5
                               textInput("class6", "Mean vector6 :", ""), #mean vector6
                               radioButtons("balance", "Balance or not :",c("TRUE"=TRUE,"FALSE"=FALSE),inline = TRUE), #y's balance
                               textInput("balance1", "n :", "100,100,100"),
                               downloadButton("downloadData", "Save") #download data
                               
                             ),
                             mainPanel(
                               HTML("<font size = 6><b>PCA</b></font>"),
                               plotOutput("scatterPlot"),
                               HTML("<font size = 6><b>Data</b></font>"),
                               dataTableOutput("dfdata")
                             )
                           )
                  ),
                  
                  
                  tabPanel("Help",mainPanel(includeMarkdown("Explain.md"))),
)

server <- function(input, output) {
  
  output$scatterPlot <- renderPlot({
    
    if(input$balance==TRUE){
      n <- rep(100,input$k)
    }else{
      n <- as.numeric(unlist(strsplit(input$balance1,",")))
    }
    
    
    df <- data.frame()
    cov_matrix <- matrix(c(2,0.2,1,0.001,
                           0.2,3,0.1,1.2,
                           1,0.1,2,0.01,
                           0.001,1.2,0.01,3),
                         ncol = 4, nrow = 4)
    
    mymean <- list(input$class1, input$class2, input$class3, 
                   input$class4, input$class5, input$class6)
    
    
    for(i in 1:input$k){
      set.seed(1116)
      my_mean <- as.numeric(unlist(strsplit(mymean[[i]],","))) #setting mean vector
      
      y <- as.data.frame(rmvnorm(n[i], mean = my_mean,sigma = cov_matrix))
      type <- as.character(rep(i, n[i]))
      y <- cbind(type, y)
      
      df <- rbind(df, y)
    }
    
    output$dfdata <- renderDataTable(df, options = list(pageLength = 5)) #df data table
    
    # Downloadable csv of selected dataset
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("test", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    
    #pca
    df_pca <- prcomp(formula = ~.,
                     data    = df[,-1],
                     scale.  = TRUE)
    #pca plot
    qplot(df_pca$x[, 1], df_pca$x[, 2], color=df[,1], main="PCA")+ theme_bw()
    
    
    
    
  })
  
  
  
}


shinyApp(ui = ui, server = server)
