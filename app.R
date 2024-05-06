library(shiny)

df <- data.frame(
  'majors' = c('business', 'medicine', 'computer science'),
  'job_1_title' = c('business1', 'medicine1', 'computer_sci1'),
  'job_2_title' = c('business2', 'medicine2', 'computer_sci2'),
  'job_1_descrption' = c('perks of business1', 'perks of medicine1', 'perks of computer_sci1'),
  'job_2_descrption' = c('perks of business2', 'perks of medicine2', 'perks of computer_sci2')
)

ui <- fluidPage(
  headerPanel('Effect of Druggg!'),
  tags$label(h3('effect of drugss! ')),
  textOutput("greetings"),
  textInput("name", "What's your name?"),
  radioButtons("gender", "What's your gender?", c('female', 'male'), selected = 'female'),
  numericInput("age", "How old are you?", value = 30, min = 15, max = 80),
  selectInput("major", "What's your major?", choices = df$majors),
  actionButton("start", "Start !"),
  textOutput("struction_1"),
  uiOutput("job_label1"),
  uiOutput("slider_input1"),  
  uiOutput("job_label2"),
  uiOutput("slider_input2"),
  tableOutput("result_table")  # Display the resulting data frame
)

server <- function(input, output, session) {
  output$greetings <- renderText("hello friend! we thank you in advance for helping us in our research")
  
  major_selected <- eventReactive(input$start, { input$major })
  
  # Reactive values to store slider values
  slider_values <- reactiveValues(job_slider1 = 50, job_slider2 = 50)
  
  # Update slider values when they change
  observeEvent(input$job_slider1, {slider_values$job_slider1 <- input$job_slider1})
  observeEvent(input$job_slider2, {slider_values$job_slider2 <- input$job_slider2})
  
  output$struction_1 <- renderText({paste0('Here are 2 jobs with different descriptions and perks for your major (', major_selected(), ')')  })
  
  output$job_label1 <- renderUI({
                                  req(input$start)  
                                  id <- which(df$majors == major_selected())
                                  div(
                                      tags$label(h2(df[id, 1]), style = "display: block; margin-bottom: 20px;"),
                                      tags$label(h3(df[id, 2]), style = "display: block; margin-bottom: 10px;"),
                                      tags$label(h4(df[id, 4]), style = "display: block; margin-bottom: 10px;")
                                      )
                                })
  output$slider_input1 <- renderUI({
                                    req(input$start) 
                                    if (input$start > 0) {sliderInput("job_slider1", "How likely are you to apply for this job?", min = 0, max = 100, value = 50)}
                                  })
  
  
  output$job_label2 <- renderUI({
                                  req(input$start)  
                                  id <- which(df$majors == major_selected())
                                  div(
                                      tags$label(h2(df[id, 1]), style = "display: block; margin-bottom: 20px;"),
                                      tags$label(h3(df[id, 3]), style = "display: block; margin-bottom: 10px;"),
                                      tags$label(h4(df[id, 5]), style = "display: block; margin-bottom: 10px;")
                                      )
                                })
  output$slider_input2 <- renderUI({
    req(input$start) 
    if (input$start > 0) {sliderInput("job_slider2", "How likely are you to apply for this job?", min = 0, max = 100, value = 50)}
                                   })
  
  output$result_table <- renderTable({
    req(input$start)  # Ensure the start button is clicked
    id <- which(df$majors == major_selected())
    data.frame(
      'j1' = df[id, 2],
      'score_j1' = slider_values$job_slider1,
      'j2' = df[id, 3],
      'score_j2' =  slider_values$job_slider2
    )
  })
}



shinyApp(ui, server)
