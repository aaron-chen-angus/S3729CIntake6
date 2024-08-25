# Load packages ----------------------------------------------------------------



library(shiny)
library(ggplot2)
library(tools)
library(shinythemes)
library(dplyr)
library(DT)


# Load data --------------------------------------------------------------------



Measurements <- read.csv(file = "https://raw.githubusercontent.com/opl06/S3729CIntake6/main/Disease_symptom_and_patient_profile_dataset__.csv", header = TRUE, sep = ",")



# Define UI --------------------------------------------------------------------



ui <- fluidPage(
  shinythemes::themeSelector(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c(
          "Age" = "age",
          "Systolic Blood Pressure" = "systolic_bp",
          "Diastolic Blood Pressure" = "diastolic_bp",
          "Resting Heart Rate" = "rest_hrt_rate",
          "Stress Level" = "stress_lvl"
        ),
        selected = "age"
      ),
      
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c(
          "Age" = "age",
          "Systolic Blood Pressure" = "systolic_bp",
          "Diastolic Blood Pressure" = "diastolic_bp",
          "Resting Heart Rate" = "rest_hrt_rate",
          "Stress Level" = "stress_lvl"
        ),
        selected = "diastolic_bp"
      ),
      
      selectInput(
        inputId = "z",
        label = "Color by:",
        choices = c(
          "Gender" = "gender",
          "Cholesterol Level" = "cholesterol_lvl",
          "Fever" = "fever",
          "Cough" = "cough",
          "Fatigue" = "fatigue",
          "Breathing Difficulty" = "difficulty_brthg",
          "Disease" = "disease",
          "Outcome" = "outcome_variable"
        ),
        selected = "outcome_variable"
      ),
      
      sliderInput(
        inputId = "alpha",
        label = "Alpha:",
        min = 0, max = 1,
        value = 0.5
      ),
      
      sliderInput(
        inputId = "size",
        label = "Size:",
        min = 0, max = 5,
        value = 2
      ),
      
      textInput(
        inputId = "plot_title",
        label = "Plot title",
        placeholder = "Enter text to be used as plot title"
      ),
      
      actionButton(
        inputId = "update_plot_title",
        label = "Update plot title"
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "scatterplot", brush = brushOpts(id = "plot_brush")),
      DT::dataTableOutput(outputId = "measurementstable"),
      textOutput(outputId = "avg_x"), # avg of x
      textOutput(outputId = "avg_y"), # avg of y
      verbatimTextOutput(outputId = "lmoutput") # regression output
    )
  )
)



# Define server ----------------------------------------------------------------



server <- function(input, output, session) {
  
  new_plot_title <- eventReactive(
    eventExpr = input$update_plot_title,
    valueExpr = {
      toTitleCase(input$plot_title)
    })
  
  output$scatterplot <- renderPlot({
    ggplot(data = Measurements, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha, size = input$size) +
      labs(title = new_plot_title())
  })
  
  output$measurementstable <- renderDataTable({
    brushedPoints(Measurements, brush = input$plot_brush) %>%
      select(person_id,disease,fever,cough,fatigue,difficulty_brthg, cholesterol_lvl, systolic_bp,	diastolic_bp,	resting_hrt_rate,	stress_lvl,	outcome_variable)
  })
  
  output$avg_x <- renderText({
    avg_x <- Measurements %>% pull(input$x) %>% mean() %>% round(2)
    paste("Average", input$x, "=", avg_x)
  })
  
  output$avg_y <- renderText({
    avg_y <- Measurements %>% pull(input$y) %>% mean() %>% round(2)
    paste("Average", input$y, "=", avg_y)
  })
  
  output$lmoutput <- renderPrint({
    x <- Measurements %>% pull(input$x)
    y <- Measurements %>% pull(input$y)
    print(summary(lm(y ~ x, data = Measurements)), digits = 3, signif.stars = FALSE)
  })
  
}



# Create the Shiny app object --------------------------------------------------



shinyApp(ui = ui, server = server)