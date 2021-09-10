#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(keras)
library(tidyverse)
library(bslib)

# load model
model = load_model_tf("tf_model/")
# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = theme <- bs_theme(
        bg = "#AE9166", fg = "#AE2223", primary = "#AE2223",
        base_font = font_google("Courier Prime"),
        code_font = font_google("Courier Prime")
    ),
    # Application title
    titlePanel("Swans or Sonic Youth"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("text_input", "", width = "100%", placeholder = "Enter your line of lyrics here.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        
        text_input = input$text_input
        
        # make prediction
        pred = model %>% predict(text_input)
        predictions = data.frame(prediction = "prediction",
                                 pred = c(0, pred-0.5))
        
        color = ifelse(predictions$pred > 0, "#B7C3DF", "#FFB50D")
        
        # draw plot
        ggplot(predictions, aes(x=prediction,y=pred[2]))+
            geom_point(size = abs(predictions$pred)*80, color = color)+
            ylim(-1,1)+
            geom_vline(xintercept = 1, color = "grey", alpha = .2)+
            geom_hline(yintercept = 0, color = "grey", alpha = .2)+
            
            coord_flip()+
            annotate("text", x = 1, y = -0.5, label = "SWANS",
                     colour="#AE2223", size=10, family="Courier", fontface="bold")+
            annotate("text", x = 1, y = 0.5, label = "SONIC YOUTH",
                     colour="#CD5A42", size=10, family="Courier", fontface="bold")+
            theme_void()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
