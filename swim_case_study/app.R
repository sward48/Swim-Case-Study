source('R/libraries.R')
source('R/prepdata.R')
source('R/analysis.R')


if (file.exists("data/Data.rds")) {
  clean_data_global <- readRDS("data/Data.rds")
} else {
  clean_data_global <- clean_data("data/Rawdata.csv")
}

ui <- navbarPage(
  title = "Olympic Swimming Analytics",
  
  #make two tabs
  tabPanel("Predictive Model",
           sidebarLayout(
             sidebarPanel(
               h4("Model Parameters"),
               #choose stroke
               selectInput("pred_stroke", "Choose Stroke:", 
                           choices = c("free", "back", "breast", "fly"), selected='free'),
               
               #choose dist
               selectInput("pred_dist", "Choose Distance:", 
                           choices = c("50", "100", "200"), selected = "100"),
               
               actionButton("run_pred", "Run Prediction", class = "btn-primary", width = "100%"),
               hr(),
               helpText("Please only use the 50 and 200 distance for free.")
             ),
             
             mainPanel(
               tabsetPanel(
                 #tab for pred output
                 tabPanel("Predictions",
                          h4("Predicted vs Actual 2024 Times"),
                          DT::dataTableOutput("pred_table")
                 ),
                 
                 #tab for eval
                 tabPanel("Evaluation & Plots",
                          h4("Model Performance Metrics"),
                          tableOutput("metrics_table_global"),
                          
                          fluidRow(
                            column(6, plotOutput("plot_residual")),
                            column(6, plotOutput("plot_actual_vs_pred"))
                          )
                 ),
                 
                 #tab for age curves
                 tabPanel("Age Curves",
                          h4("Age Progression Curves (Model Effect)"),
                          p("Modeled impact of age on time (lower is faster)"),
                          fluidRow(
                            column(6, h5("Men"), plotOutput("age_curve_m")),
                            column(6, h5("Women"), plotOutput("age_curve_f"))
                          )
                 )
               )
             )
           )
  ),
  
  #second tab
  tabPanel("Relay Simulation",
           sidebarLayout(
             sidebarPanel(
               h4("Relay Configuration"),
               #choose relay type
               selectInput("relay_type", "Relay Event:", 
                           choices = c("4x100_free", "4x100_medley", "4x200_free")),
               #choose gender
               radioButtons("relay_gender", "Gender Category:",
                            choices = c("Male" = "male", 
                                        "Female" = "female", 
                                        "Mixed" = "mixed")),
               #filter swimmers
               selectizeInput(
                 inputId = "exclude_swimmers",
                 label = "Exclude Swimmer(s):",
                 choices = NULL,
                 multiple = TRUE, 
                 options = list(placeholder = 'Type to search name')
               ),
               
               actionButton("run_sim", "Simulate Relay", class = "btn-success", width = "100%"),
               hr(),
               helpText("Simulates the top combinations based on individual and relay-start predictions.")
             ),
             
             mainPanel(
               h3("Fastest Relay Team Combinations"),
               DT::dataTableOutput("relay_results")
             )
           )
  )
)


server <- function(input, output, session) {
  
  observe({
    #wait until user chooses to do anything
    req(input$relay_gender)
    
    
    relevant_names <- clean_data_global
    
    if (input$relay_gender == "male") {
      relevant_names <- relevant_names %>% filter(gender == "M")
    } else if (input$relay_gender == "female") {
      relevant_names <- relevant_names %>% filter(gender == "F")
    }
    
    name_list <- sort(unique(relevant_names$name))
    
    #officially get srid of the swimmers chosen
    updateSelectizeInput(session, "exclude_swimmers", choices = name_list, server = TRUE)
  })

  #run when button is clicked
  prediction_results <- eventReactive(input$run_pred, {
    req(input$pred_stroke, input$pred_dist)
    
    validate(
      need(
        !((input$pred_dist %in% c("50", "200")) & (input$pred_stroke != "free")),
        "Error: Distances 50 and 200 are currently only available for free."
      )
    )
    
    #given the params from user, run the model and predict
    model <- run_model(clean_data_global, input$pred_stroke, input$pred_dist)

    preds <- predict_olympics(model, clean_data_global, input$pred_stroke, input$pred_dist)

    evals <- evaluate_pred(preds)
      
    list(data = preds, metrics = evals, model=model)
    })

  
  #pred outputs
  output$pred_table <- DT::renderDataTable({
    req(prediction_results())
    data <- prediction_results()$data
    
    #make pretty
    DT::datatable(data %>% 
                    dplyr::select(name, gender, age_oly, prediction_type, 
                           pred_time = pred_oly_time_sec_actual,
                           sim_time = pred_oly_time_sec_sim,
                           low_sim = pred_low, high_sim = pred_high,
                           actual_time = actual_oly_time_sec, 
                           error = error_sec, abs_error = abs_error_sec) %>%
                    mutate(across(where(is.numeric), round, 2)),
                  options = list(pageLength = 20))
  })
  
  #eval metrics
  output$metrics_table_global <- renderTable({
    req(prediction_results())
    prediction_results()$metrics$results
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  #eval plots
  output$plot_residual <- renderPlot({
    req(prediction_results())
    data <- prediction_results()$data
    
    ggplot(data, aes(x = pred_oly_time_sec_actual, y = error_sec)) +
      geom_point(aes(color = gender), alpha = 0.6, size = 3) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      scale_color_manual(values = c('F'='deeppink1', 'M'='blue')) +
      labs(title = "Residuals vs. Predicted Speed",
           x = "Predicted Time (sec)", y = "Error (s)") +
      theme_pub()
  })
  
  output$plot_actual_vs_pred <- renderPlot({
    req(prediction_results())
    data <- prediction_results()$data
    
    ggplot(data, aes(x = actual_oly_time_sec, y = pred_oly_time_sec_actual)) +
      geom_point(aes(color = gender), alpha = 0.7, size = 3) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
      scale_color_manual(values = c('F'='deeppink1', 'M'='blue')) +
      labs(title = "Actual vs. Predicted Times",
           x = "Actual Time (s)", y = "Predicted Time (s)") +
      theme_pub() +
      coord_equal()
  })
  
  #age curves
  output$age_curve_m <- renderPlot({
    input$run_pred
    isolate({
      req(input$pred_stroke, input$pred_dist)
      age_curve(clean_data_global, input$pred_stroke, input$pred_dist, "M")
    })
  })

  output$age_curve_f <- renderPlot({
    input$run_pred
    isolate({
      req(input$pred_stroke, input$pred_dist)
      age_curve(clean_data_global, input$pred_stroke, input$pred_dist, "F")
    })
  })
  
  
  #relay sims
  relay_sim_results <- eventReactive(input$run_sim, {
    req(input$relay_type, input$relay_gender)
    
      
    sim_data <- clean_data_global
    
    #check for filtered swimmers
    if (!is.null(input$exclude_swimmers)) {
      sim_data <- sim_data %>% 
        filter(!name %in% input$exclude_swimmers)
    }
    
    sims <- simulate_relay(sim_data, input$relay_type, input$relay_gender)
    
  sims
  })
  
  #relay sim output
  output$relay_results <- DT::renderDataTable({
    req(relay_sim_results())
    df <- relay_sim_results()
    
    #make pretty
    DT::datatable(df %>% mutate(across(where(is.numeric), round, 2)),
                  options = list(pageLength = 10, scrollX = TRUE))
  })
}


shinyApp(ui = ui, server = server)